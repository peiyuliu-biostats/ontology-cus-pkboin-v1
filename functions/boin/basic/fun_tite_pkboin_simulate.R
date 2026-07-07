# =====================================================================
# TITE-PKBOIN-12 simulation engine (pure functions)
# ---------------------------------------------------------------------
# Patient-level time-to-event extension of PKBOIN-12. Interim toxicity
# and efficacy decisions use TITE approximated-likelihood quasi counts;
# PK decisions and final OBD selection reuse the validated PKBOIN-12
# primitives. Final OBD is selected after all toxicity/efficacy windows
# are complete, using complete outcomes.
#
# PERFORMANCE NOTE (this revision):
#   Two hot paths were rewritten for speed WITHOUT changing any numeric
#   result or the paper's section 2.4 logic:
#     (1) tite_pkboin_quasi_obs() now accumulates per-dose vectors and
#         builds the output data.frame ONCE, instead of per-cell
#         `obs$col[d] <-` assignment (which copied the frame each write).
#         The imputation formula and column values are byte-identical.
#     (2) tite_pkboin_one_trial() preallocates patient_df to n_max rows
#         and fills by row index, instead of rbind()-in-loop (which
#         copied the whole frame every cohort, O(cohort^2)). The RNG
#         stream, patient_id sequence, column values/types, and every
#         downstream call receive identical data.
#   No other function is touched; all AL / PK / OBD math is unchanged.
# =====================================================================

tite_pkboin_default_design <- function(tite_design = list()) {
  utils::modifyList(list(A_T = 30, A_E = 60, accrual_rate = 10,
                         suspend_threshold = 0.5, use_susp = TRUE,
                         accrual_random = FALSE), tite_design)
}

tite_pkboin_gen_cohort <- function(cohort, dose, time_current, cs,
                                   p_d, q_d, r_d, CV, g_P, tite_design) {
  td <- tite_pkboin_default_design(tite_design)
  enroll <- if (isTRUE(td$accrual_random)) {
    time_current + cumsum(stats::runif(cs, 0, 2 * td$accrual_rate)) + 1
  } else {
    time_current + seq.int(0, cs - 1L) * td$accrual_rate + 1
  }

  pk <- pkboin_rtruncnorm_pos(cs, r_d, CV * r_d)
  rel <- if (r_d > 0) (pk - r_d) / r_d else rep(0, cs)
  p_ij <- pmin(pmax(p_d * (1 + g_P * rel), 0), 1)
  q_ij <- pmin(pmax(q_d * (1 + g_P * rel), 0), 1)
  y_tox <- stats::rbinom(cs, 1, p_ij)
  y_eff <- stats::rbinom(cs, 1, q_ij)

  tox_event <- ifelse(y_tox == 1, enroll + stats::runif(cs, 0, td$A_T), Inf)
  eff_event <- ifelse(y_eff == 1, enroll + stats::runif(cs, 0, td$A_E), Inf)

  data.frame(
    patient_id = NA_integer_, cohort = cohort, dose = dose, enroll = enroll,
    pk = pk, tox_prob = p_ij, eff_prob = q_ij,
    dlt = y_tox, response = y_eff,
    tox_end = enroll + td$A_T, eff_end = enroll + td$A_E,
    tox_event = tox_event, eff_event = eff_event,
    tox_confirm = pmin(enroll + td$A_T, tox_event),
    eff_confirm = pmin(enroll + td$A_E, eff_event)
  )
}

tite_pkboin_decision_time <- function(patient_df, current_dose,
                                      time_current, cs, tite_design) {
  td <- tite_pkboin_default_design(tite_design)
  base_next <- time_current + cs * td$accrual_rate + 1
  if (!isTRUE(td$use_susp)) return(base_next)

  cur <- patient_df[patient_df$dose == current_dose, , drop = FALSE]
  if (nrow(cur) == 0) return(base_next)
  min_n <- min(floor(nrow(cur) * td$suspend_threshold + 1), nrow(cur))
  max(sort(cur$tox_confirm)[min_n], sort(cur$eff_confirm)[min_n], base_next)
}

tite_pkboin_patient_state <- function(patient_df, time_next, tite_design) {
  td <- tite_pkboin_default_design(tite_design)
  df <- patient_df
  df$follow <- pmax(time_next - df$enroll, 0)
  df$delta_t <- ifelse(df$tox_event <= time_next, 1,
                       ifelse(df$tox_end <= time_next, 0, -1))
  df$delta_e <- ifelse(df$eff_event <= time_next, 1,
                       ifelse(df$eff_end <= time_next, 0, -1))
  df$w_t <- ifelse(df$delta_t == -1, pmin(df$follow / td$A_T, 1), 0)
  df$w_e <- ifelse(df$delta_e == -1, pmin(df$follow / td$A_E, 1), 0)
  df
}

# --- vectorized quasi-count builder (byte-identical to the per-cell
#     version). For each dose we compute the same ESS / p_star / q_star
#     and the SAME approximated-likelihood imputation
#       tox_exp = delta_t          (if delta_t >= 0)
#               = p_star*(1-w_t)/max(1-p_star*w_t, 1e-8)  (if pending)
#       eff_exp = delta_e          (if delta_e >= 0)
#               = q_star*(1-w_e)/max(1-q_star*w_e, 1e-8)  (if pending)
#     and n1..n4 = sum of (1-te)ee, te*ee, (1-te)(1-ee), te*(1-ee).
#     Only the data.frame assembly changed (one construction at the end)
#     -- no per-cell `obs$col[d] <-` writes. ---
tite_pkboin_quasi_obs <- function(patient_state, n_dose) {
  n_v      <- integer(n_dose)
  n1_v     <- numeric(n_dose); n2_v <- numeric(n_dose)
  n3_v     <- numeric(n_dose); n4_v <- numeric(n_dose)
  pstar_v  <- rep(NA_real_, n_dose); qstar_v <- rep(NA_real_, n_dose)
  ESSt_v   <- numeric(n_dose); ESSe_v <- numeric(n_dose)
  pendt_v  <- numeric(n_dose); pende_v <- numeric(n_dose)

  for (d in seq_len(n_dose)) {
    x <- patient_state[patient_state$dose == d, , drop = FALSE]
    if (nrow(x) == 0) next

    n_v[d]     <- nrow(x)
    pendt_v[d] <- sum(x$delta_t == -1)
    pende_v[d] <- sum(x$delta_e == -1)
    ESSt_v[d]  <- sum(x$delta_t %in% c(0, 1)) + sum(x$w_t[x$delta_t == -1])
    ESSe_v[d]  <- sum(x$delta_e %in% c(0, 1)) + sum(x$w_e[x$delta_e == -1])
    p_star     <- if (ESSt_v[d] > 0) sum(x$delta_t == 1) / ESSt_v[d] else 0
    q_star     <- if (ESSe_v[d] > 0) sum(x$delta_e == 1) / ESSe_v[d] else 0
    pstar_v[d] <- p_star
    qstar_v[d] <- q_star

    tox_exp <- ifelse(x$delta_t >= 0, x$delta_t,
                      p_star * (1 - x$w_t) / pmax(1 - p_star * x$w_t, 1e-8))
    eff_exp <- ifelse(x$delta_e >= 0, x$delta_e,
                      q_star * (1 - x$w_e) / pmax(1 - q_star * x$w_e, 1e-8))

    n1_v[d] <- sum((1 - tox_exp) * eff_exp)
    n2_v[d] <- sum(tox_exp * eff_exp)
    n3_v[d] <- sum((1 - tox_exp) * (1 - eff_exp))
    n4_v[d] <- sum(tox_exp * (1 - eff_exp))
  }

  data.frame(dose = seq_len(n_dose), n = n_v,
             n1 = n1_v, n2 = n2_v, n3 = n3_v, n4 = n4_v,
             p_star = pstar_v, q_star = qstar_v,
             ESS_t = ESSt_v, ESS_e = ESSe_v,
             pending_t = pendt_v, pending_e = pende_v)
}

tite_pkboin_complete_obs <- function(patient_df, n_dose) {
  obs <- data.frame(dose = seq_len(n_dose), n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  for (d in seq_len(n_dose)) {
    x <- patient_df[patient_df$dose == d, , drop = FALSE]
    obs$n[d] <- nrow(x)
    obs$n1[d] <- sum(x$response == 1 & x$dlt == 0)
    obs$n2[d] <- sum(x$response == 1 & x$dlt == 1)
    obs$n3[d] <- sum(x$response == 0 & x$dlt == 0)
    obs$n4[d] <- sum(x$response == 0 & x$dlt == 1)
  }
  obs
}

tite_pkboin_pk_summary_from_patients <- function(patient_df, n_dose) {
  obs_pk <- rep(NA_real_, n_dose)
  sigma_pk <- rep(NA_real_, n_dose)
  pk_n <- integer(n_dose)
  for (d in seq_len(n_dose)) {
    x <- patient_df$pk[patient_df$dose == d]
    pk_n[d] <- length(x)
    if (pk_n[d] > 0) obs_pk[d] <- mean(x)
    if (pk_n[d] >= 2) sigma_pk[d] <- stats::sd(x)
  }
  list(mean = obs_pk, sd = sigma_pk, n = pk_n)
}

tite_pkboin_one_trial <- function(p_true, q_true, r_true, design, pk_design,
                                  tite_design, trial, boundaries, u) {
  D <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  zeta1 <- if (!is.null(pk_design$zeta1)) pk_design$zeta1 else
    pkboin_zeta1(pk_design$r_P, pk_design$r_I_mult)

  # ---- preallocate the patient store as TYPED COLUMN VECTORS (not a
  # growing data.frame). This avoids the O(cohort^2) copy that
  # rbind()-in-loop incurred, and avoids any row-assignment type
  # coercion: each column is a plain atomic vector filled by index, and
  # the data.frame view is assembled from the filled slices. The column
  # set and types exactly mirror tite_pkboin_gen_cohort()'s output, so
  # every downstream call receives byte-identical data. Upper bound on
  # rows: loop runs while n_used < Nmax and each cohort adds cs, so at
  # most ceiling(Nmax/cs)*cs rows.
  max_rows <- as.integer(ceiling(Nmax / cs)) * cs
  col_patient_id <- integer(max_rows)
  col_cohort     <- numeric(max_rows)
  col_dose       <- numeric(max_rows)
  col_enroll     <- numeric(max_rows)
  col_pk         <- numeric(max_rows)
  col_tox_prob   <- numeric(max_rows)
  col_eff_prob   <- numeric(max_rows)
  col_dlt        <- integer(max_rows)
  col_response   <- integer(max_rows)
  col_tox_end    <- numeric(max_rows)
  col_eff_end    <- numeric(max_rows)
  col_tox_event  <- numeric(max_rows)
  col_eff_event  <- numeric(max_rows)
  col_tox_confirm<- numeric(max_rows)
  col_eff_confirm<- numeric(max_rows)

  # assemble a data.frame view of the first `k` filled rows (identical
  # columns/types/values to the rbind() accumulation at that point).
  make_pdf <- function(k) {
    ii <- seq_len(k)
    data.frame(
      patient_id  = col_patient_id[ii],
      cohort      = col_cohort[ii],
      dose        = col_dose[ii],
      enroll      = col_enroll[ii],
      pk          = col_pk[ii],
      tox_prob    = col_tox_prob[ii],
      eff_prob    = col_eff_prob[ii],
      dlt         = col_dlt[ii],
      response    = col_response[ii],
      tox_end     = col_tox_end[ii],
      eff_end     = col_eff_end[ii],
      tox_event   = col_tox_event[ii],
      eff_event   = col_eff_event[ii],
      tox_confirm = col_tox_confirm[ii],
      eff_confirm = col_eff_confirm[ii]
    )
  }

  eliminated <- rep(FALSE, D)
  pk_elim <- rep(FALSE, D)
  pk_terminated <- FALSE
  j <- trial$start_dose
  time_current <- 0
  n_used <- 0L
  cohort <- 0L
  log_rows <- list()

  while (n_used < Nmax) {
    cohort <- cohort + 1L
    g <- tite_pkboin_gen_cohort(cohort, j, time_current, cs,
                                p_true[j], q_true[j], r_true[j],
                                pk_design$CV, pk_design$g_P, tite_design)
    g$patient_id <- seq.int(n_used + 1L, n_used + nrow(g))

    idx <- seq.int(n_used + 1L, n_used + nrow(g))
    col_patient_id[idx] <- g$patient_id
    col_cohort[idx]     <- g$cohort
    col_dose[idx]       <- g$dose
    col_enroll[idx]     <- g$enroll
    col_pk[idx]         <- g$pk
    col_tox_prob[idx]   <- g$tox_prob
    col_eff_prob[idx]   <- g$eff_prob
    col_dlt[idx]        <- g$dlt
    col_response[idx]   <- g$response
    col_tox_end[idx]    <- g$tox_end
    col_eff_end[idx]    <- g$eff_end
    col_tox_event[idx]  <- g$tox_event
    col_eff_event[idx]  <- g$eff_event
    col_tox_confirm[idx]<- g$tox_confirm
    col_eff_confirm[idx]<- g$eff_confirm
    n_used <- n_used + nrow(g)

    # sliced view of the filled portion -- identical to the accumulated
    # rbind() data.frame at this point in the original.
    pdf <- make_pdf(n_used)

    time_next <- tite_pkboin_decision_time(pdf, j, time_current, cs, tite_design)
    st <- tite_pkboin_patient_state(pdf, time_next, tite_design)
    qobs <- tite_pkboin_quasi_obs(st, D)
    pk_s <- tite_pkboin_pk_summary_from_patients(pdf, D)

    el <- boin_elimination(qobs[qobs$n > 0, c("dose", "n", "n1", "n2", "n3", "n4"), drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    pk_res <- pkboin_pk_elimination(pk_s$mean, pk_s$sd, pk_s$n,
                                    pk_design$r_P, pk_design$C_P)
    pk_elim <- pk_elim | pk_res$pk_elim
    dead <- eliminated | pk_elim

    nd <- pkboin_next_dose(j, qobs[, c("dose", "n", "n1", "n2", "n3", "n4")],
                           pk_s$mean, boundaries, u, D, design = design,
                           pk_design = list(zeta1 = zeta1))
    cand <- nd$admissible[!dead[nd$admissible]]

    stop_now <- FALSE
    next_j <- NA_integer_
    rds_next <- NA_real_
    if (isTRUE(pk_res$terminate)) {
      pk_terminated <- TRUE
      stop_now <- TRUE
      decision <- "stop: top-dose PK inefficacious"
    } else if (length(cand) == 0) {
      stop_now <- TRUE
      decision <- "stop: no admissible dose"
    } else {
      next_j <- nd$next_dose
      if (dead[next_j]) next_j <- cand[which.max(nd$scores[as.character(cand)])]
      rds_next <- unname(nd$scores[as.character(next_j)])
      decision <- if (next_j > j) "escalate" else if (next_j < j) "de-escalate" else "stay"
    }

    complete_so_far <- tite_pkboin_complete_obs(pdf, D)
    log_rows[[cohort]] <- data.frame(
      cohort = cohort,
      dose = j,
      decision_time = round(time_next, 2),
      cohort_n = cs,
      cohort_tox = sum(g$dlt),
      cohort_eff = sum(g$response),
      cum_n = complete_so_far$n[j],
      cum_tox = complete_so_far$n2[j] + complete_so_far$n4[j],
      cum_eff = complete_so_far$n1[j] + complete_so_far$n2[j],
      total_n = n_used,
      pending_t_current = qobs$pending_t[j],
      pending_e_current = qobs$pending_e[j],
      ESS_t_current = round(qobs$ESS_t[j], 3),
      ESS_e_current = round(qobs$ESS_e[j], 3),
      p_star = round(qobs$p_star[j], 4),
      q_star = round(qobs$q_star[j], 4),
      r_hat = ifelse(is.na(pk_s$mean[j]), NA_real_, round(pk_s$mean[j], 2)),
      pk_adequate = isTRUE(nd$pk_adequate),
      d_star = nd$d_star,
      d_pk_min = nd$d_pk_min,
      admissible = paste(nd$admissible, collapse = ","),
      rds_next = ifelse(is.na(rds_next), NA_real_, round(rds_next, 4)),
      tox_eff_eliminated = paste(which(eliminated), collapse = ","),
      pk_eliminated = paste(which(pk_elim), collapse = ","),
      decision = decision,
      next_dose = next_j
    )

    if (stop_now) break
    j <- next_j
    time_current <- time_next
  }

  # final complete-data view (only the filled rows)
  patient_df <- make_pdf(n_used)

  complete_obs <- tite_pkboin_complete_obs(patient_df, D)
  pk_s <- tite_pkboin_pk_summary_from_patients(patient_df, D)
  final <- pkboin_select_obd(complete_obs, pk_s$mean, design,
                             list(r_P = pk_design$r_P), u, pk_elim)

  duration <- max(patient_df$tox_end, patient_df$eff_end, na.rm = TRUE)
  list(obd = final$obd,
       alloc = complete_obs$n,
       dlt = sum(complete_obs$n2 + complete_obs$n4),
       eff = sum(complete_obs$n1 + complete_obs$n2),
       duration = duration,
       pk_terminated = pk_terminated,
       n_pk_elim = sum(pk_elim),
       patients = patient_df,
       trajectory = do.call(rbind, log_rows),
       final_obd = final)
}

tite_pkboin_operating_char <- function(p_true, q_true, r_true, design, pk_design,
                                       tite_design, trial, u,
                                       n_rep = 2000, seed = 1) {
  set.seed(seed)
  boundaries <- boin_boundaries(design$phi_T, phi1 = design$phi1, phi2 = design$phi2)
  pk_design$zeta1 <- pkboin_zeta1(pk_design$r_P, pk_design$r_I_mult)
  D <- trial$n_dose

  sel <- integer(n_rep)
  alloc <- matrix(0, n_rep, D)
  dlt_v <- numeric(n_rep)
  eff_v <- numeric(n_rep)
  dur_v <- numeric(n_rep)
  pk_term_v <- logical(n_rep)
  pk_nelim_v <- numeric(n_rep)

  for (r in seq_len(n_rep)) {
    z <- tite_pkboin_one_trial(p_true, q_true, r_true, design, pk_design,
                               tite_design, trial, boundaries, u)
    sel[r] <- if (is.na(z$obd)) 0L else z$obd
    alloc[r, ] <- z$alloc
    dlt_v[r] <- z$dlt
    eff_v[r] <- z$eff
    dur_v[r] <- z$duration
    pk_term_v[r] <- z$pk_terminated
    pk_nelim_v[r] <- z$n_pk_elim
  }

  sel_pct <- sapply(0:D, function(d) mean(sel == d)) * 100
  names(sel_pct) <- c("none", paste0("dose", 1:D))
  mean_alloc <- colMeans(alloc)
  true_obd <- pkboin_true_obd(p_true, q_true, r_true,
                              list(phi_T = design$phi_T, phi_E = design$phi_E),
                              boundaries, u, list(r_P = pk_design$r_P))
  overdose <- p_true > design$phi_T
  correct_sel_pct <- if (is.na(true_obd)) NA_real_ else unname(sel_pct[paste0("dose", true_obd)])
  n_at_obd <- if (is.na(true_obd)) NA_real_ else unname(mean_alloc[true_obd])
  poor_alloc_pct <- if (is.na(true_obd)) NA_real_ else mean(alloc[, true_obd] < trial$n_max / D) * 100

  list(selection_pct = sel_pct,
       mean_alloc = mean_alloc,
       early_stop_pct = mean(sel == 0) * 100,
       true_obd = true_obd,
       overdose = overdose,
       correct_sel_pct = correct_sel_pct,
       n_at_obd = n_at_obd,
       n_at_overdose = sum(mean_alloc[overdose]),
       poor_alloc_pct = poor_alloc_pct,
       mean_dlt = mean(dlt_v),
       mean_eff = mean(eff_v),
       mean_duration_days = mean(dur_v),
       mean_duration_months = mean(dur_v) / 30,
       pk_early_term_pct = mean(pk_term_v) * 100,
       mean_pk_elim = mean(pk_nelim_v),
       zeta1 = pk_design$zeta1)
}

tite_pkboin_one_trial_traj <- function(p_true, q_true, r_true, design, pk_design,
                                       tite_design, trial, boundaries, u) {
  tite_pkboin_one_trial(p_true, q_true, r_true, design, pk_design,
                        tite_design, trial, boundaries, u)
}
