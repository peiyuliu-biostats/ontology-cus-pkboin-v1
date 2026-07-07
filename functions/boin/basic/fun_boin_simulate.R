# =====================================================================
# BOIN-12 operating-characteristics simulation (pure functions)
# ---------------------------------------------------------------------
# Runs the full BOIN-12 trial repeatedly under known true p_true
# (toxicity) / q_true (efficacy) to produce selection %, patient
# allocation, and early-stop %. Depends only on other pure boin_*
# functions (auto-sourced); no dependency on functions/stein/.
#
# Joint-outcome truth model: each simulated cohort's 4 joint category
# counts are drawn from a single multinomial with cell probabilities
# derived from p_true[j]/q_true[j] under the SAME conditional-
# independence assumption used by boin_true_obd() in fun_boin_obd.R
# (stage A5) -- pi1=q(1-p), pi2=qp, pi3=(1-q)(1-p), pi4=(1-q)p. This
# keeps the simulated data and the oracle "true OBD" internally
# consistent. As noted there, this is a simplifying approximation
# (real toxicity/efficacy need not be conditionally independent given
# dose); revisit both together if/when a non-independent joint truth
# model is introduced.
# =====================================================================

boin_joint_pi_indep <- function(p, q) {
  c(q * (1 - p), q * p, (1 - q) * (1 - p), (1 - q) * p)
}

# one simulated BOIN-12 trial -> selected OBD (NA if all eliminated) + allocation
boin_one_trial <- function(p_true, q_true, design, trial, boundaries, u) {
  D  <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  eliminated <- rep(FALSE, D)
  j <- trial$start_dose
  n_used <- 0L

  while (n_used < Nmax) {
    pi <- boin_joint_pi_indep(p_true[j], q_true[j])
    draw <- as.vector(stats::rmultinom(1, cs, pi))
    obs$n1[j] <- obs$n1[j] + draw[1]
    obs$n2[j] <- obs$n2[j] + draw[2]
    obs$n3[j] <- obs$n3[j] + draw[3]
    obs$n4[j] <- obs$n4[j] + draw[4]
    obs$n[j]  <- obs$n[j] + cs
    n_used <- n_used + cs

    el <- boin_elimination(obs[obs$n > 0, , drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated
    if (all(eliminated[1:D][obs$n > 0]) && all(eliminated)) break
    if (eliminated[1]) break

    nd <- boin_next_dose(j, obs, boundaries, u, D, design = design)
    cand <- nd$admissible[!eliminated[nd$admissible]]
    if (length(cand) == 0) break
    j <- nd$next_dose
    if (eliminated[j]) j <- cand[which.max(nd$scores[as.character(cand)])]
  }

  res <- boin_select_obd(obs, design, u)
  # additive per-trial totals for OC summary (DLT = tox marginal, eff =
  # efficacy marginal, summed over all doses); does not affect obd/alloc.
  total_dlt <- sum(obs$n2 + obs$n4)
  total_eff <- sum(obs$n1 + obs$n2)
  list(obd = res$obd, alloc = obs$n, dlt = total_dlt, eff = total_eff)
}

# full operating characteristics over n_rep replications
boin_operating_char <- function(p_true, q_true, design, trial, u, n_rep = 2000,
                                 seed = 1) {
  set.seed(seed)
  boundaries <- boin_boundaries(design$phi_T, phi1 = design$phi1, phi2 = design$phi2)
  D <- trial$n_dose
  sel   <- integer(n_rep)
  alloc <- matrix(0, n_rep, D)
  dlt_v <- numeric(n_rep)
  eff_v <- numeric(n_rep)
  for (r in seq_len(n_rep)) {
    t1 <- boin_one_trial(p_true, q_true, design, trial, boundaries, u)
    sel[r] <- if (is.na(t1$obd)) 0L else t1$obd
    alloc[r, ] <- t1$alloc
    dlt_v[r] <- t1$dlt
    eff_v[r] <- t1$eff
  }
  sel_pct <- sapply(0:D, function(d) mean(sel == d)) * 100
  names(sel_pct) <- c("none", paste0("dose", 1:D))
  mean_alloc <- colMeans(alloc)

  # ---- BOIN12 Fig-3 summary metrics (additive; no decision-logic change) ----
  # true OBD under the same conditional-independence joint truth used by
  # the simulator; NA if none qualifies (all toxic/inefficacious).
  true_obd <- boin_true_obd(p_true, q_true,
                            list(phi_T = design$phi_T, phi_E = design$phi_E),
                            boundaries, u)
  overdose <- p_true > design$phi_T                      # doses above phi_T
  correct_sel_pct <- if (is.na(true_obd)) NA_real_ else unname(sel_pct[paste0("dose", true_obd)])
  n_at_obd        <- if (is.na(true_obd)) NA_real_ else unname(mean_alloc[true_obd])
  n_at_overdose   <- sum(mean_alloc[overdose])
  # poor allocation: trials assigning < n_max/D patients to the true OBD
  poor_thresh <- trial$n_max / D
  poor_alloc_pct <- if (is.na(true_obd)) NA_real_ else mean(alloc[, true_obd] < poor_thresh) * 100

  list(
    selection_pct   = sel_pct,
    mean_alloc      = mean_alloc,
    early_stop_pct  = mean(sel == 0) * 100,
    true_obd        = true_obd,
    overdose        = overdose,
    correct_sel_pct = correct_sel_pct,
    n_at_obd        = n_at_obd,
    n_at_overdose   = n_at_overdose,
    poor_alloc_pct  = poor_alloc_pct,
    mean_dlt        = mean(dlt_v),
    mean_eff        = mean(eff_v)
  )
}

# =====================================================================
# Representative trajectory (pure function; additive, does not alter
# boin_one_trial() / boin_operating_char() above). Runs exactly one
# BOIN-12 trial and records a cohort-by-cohort log for the Data tab.
# =====================================================================
boin_one_trial_traj <- function(p_true, q_true, design, trial, boundaries, u) {
  D  <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  eliminated <- rep(FALSE, D)
  j <- trial$start_dose
  n_used <- 0L
  log_rows <- list()
  cohort_idx <- 0L

  while (n_used < Nmax) {
    cohort_idx <- cohort_idx + 1L
    pi <- boin_joint_pi_indep(p_true[j], q_true[j])
    draw <- as.vector(stats::rmultinom(1, cs, pi))
    obs$n1[j] <- obs$n1[j] + draw[1]
    obs$n2[j] <- obs$n2[j] + draw[2]
    obs$n3[j] <- obs$n3[j] + draw[3]
    obs$n4[j] <- obs$n4[j] + draw[4]
    obs$n[j]  <- obs$n[j] + cs
    n_used <- n_used + cs

    el <- boin_elimination(obs[obs$n > 0, , drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    stop_now <- FALSE
    decision <- "continue"
    next_j <- NA_integer_
    admissible_str <- ""
    rds_next <- NA_real_

    if (all(eliminated[1:D][obs$n > 0]) && all(eliminated)) {
      stop_now <- TRUE; decision <- "stop: all doses eliminated"
    } else if (eliminated[1]) {
      stop_now <- TRUE; decision <- "stop: dose 1 too toxic"
    } else {
      nd <- boin_next_dose(j, obs, boundaries, u, D, design = design)
      admissible_str <- paste(nd$admissible, collapse = ",")
      cand <- nd$admissible[!eliminated[nd$admissible]]
      if (length(cand) == 0) {
        stop_now <- TRUE; decision <- "stop: no admissible dose"
      } else {
        next_j <- nd$next_dose
        if (eliminated[next_j]) next_j <- cand[which.max(nd$scores[as.character(cand)])]
        rds_next <- unname(nd$scores[as.character(next_j)])
        decision <- if (next_j > j) "escalate" else if (next_j < j) "de-escalate" else "stay"
      }
    }

    log_rows[[cohort_idx]] <- data.frame(
      cohort     = cohort_idx,
      dose       = j,
      cohort_n   = cs,
      cohort_tox = draw[2] + draw[4],
      cohort_eff = draw[1] + draw[2],
      cum_n      = obs$n[j],
      cum_tox    = obs$n2[j] + obs$n4[j],
      cum_eff    = obs$n1[j] + obs$n2[j],
      admissible = admissible_str,
      rds_next   = ifelse(is.na(rds_next), NA_real_, round(rds_next, 4)),
      decision   = decision,
      next_dose  = next_j
    )

    if (stop_now) break
    j <- next_j
  }

  res <- boin_select_obd(obs, design, u)
  list(obd = res$obd, alloc = obs$n, trajectory = do.call(rbind, log_rows))
}
