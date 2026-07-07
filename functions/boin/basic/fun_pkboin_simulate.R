# =====================================================================
# PKBOIN-12 operating-characteristics simulation (pure functions)
# ---------------------------------------------------------------------
# Faithful to Sun & Tu (2024, Pharmaceutical Statistics 24:e2444),
# section 3.1 (data-generation model) + Figure 1 / section 2.3 (design).
# Runs the full PKBOIN-12 trial repeatedly under known true
# p_true (toxicity), q_true (efficacy), r_true (mean PK) to produce
# selection %, patient allocation, early-stop %, and PK-specific metrics.
#
# --------------------------- data generation (paper sec 3.1) ---------
# Individual PK value:
#   r_{d,j} ~ truncated-N( r_d, (CV * r_d)^2 ; 0, Inf )
# Individual-level toxicity / efficacy probabilities linked to PK:
#   p_{d,j} = min{ p_d * (1 + g_P * (r_{d,j} - r_d)/r_d), 1 }
#   q_{d,j} = min{ q_d * (1 + g_P * (r_{d,j} - r_d)/r_d), 1 }
# (also floored at 0). Then Y_tox ~ Bernoulli(p_{d,j}),
#   Y_eff ~ Bernoulli(q_{d,j}), independently. g_P = 0 => PK uncorrelated
# with tox/eff (design still benefits by excluding low-PK doses). This is
# why each patient is drawn individually (Bernoulli x Bernoulli), NOT via
# a single dose-level multinomial as in BOIN-12: PK variability makes each
# patient's (p,q) different.
#
# The joint SLOT category for a patient is:
#   Y_tox=0,Y_eff=1 -> slot1 ; Y_tox=1,Y_eff=1 -> slot2 ;
#   Y_tox=0,Y_eff=0 -> slot3 ; Y_tox=1,Y_eff=0 -> slot4.
# so marginal tox = slot2+slot4, eff = slot1+slot2, exactly matching the
# BOIN-12 count convention -- the RDS/elimination code is reused as-is.
#
# Independence note: tox and eff are drawn independently given the shared
# PK draw r_{d,j}. The shared PK induces a positive tox-eff association
# (both rise with r_{d,j} when g_P>0), which is the paper's intended
# correlation mechanism; this differs from BOIN-12's conditional-
# independence multinomial but reduces to it when g_P = 0.
#
# Depends (intra-family only): fun_pkboin_pk.R, fun_pkboin_decision.R,
# fun_pkboin_obd.R, and the reused BOIN-12 fns (boin_boundaries,
# boin_elimination). No functions/stein/* dependency.
# =====================================================================

# ---- truncated-normal PK draw (left-truncated at 0) -----------------
# n draws from N(mean, sd^2) conditioned to (0, Inf). Inverse-CDF method
# so it is exact and vectorised; sd <= 0 returns rep(mean, n).
pkboin_rtruncnorm_pos <- function(n, mean, sd) {
  if (sd <= 0) return(rep(mean, n))
  lo <- stats::pnorm(0, mean = mean, sd = sd)       # P(X <= 0)
  u  <- stats::runif(n, lo, 1)
  stats::qnorm(u, mean = mean, sd = sd)
}

# ---- generate one cohort's joint SLOT counts + PK summary -----------
# Returns list(n1,n2,n3,n4, pk_vals) for `cs` patients at a dose with
# true (p_d, q_d, r_d). pk_vals is the vector of individual PK draws
# (needed to accumulate the per-dose sample mean/SD).
pkboin_gen_cohort <- function(p_d, q_d, r_d, cs, CV, g_P) {
  sd_pk <- CV * r_d
  r_ij  <- pkboin_rtruncnorm_pos(cs, r_d, sd_pk)
  rel   <- if (r_d > 0) (r_ij - r_d) / r_d else rep(0, cs)
  p_ij  <- pmin(pmax(p_d * (1 + g_P * rel), 0), 1)
  q_ij  <- pmin(pmax(q_d * (1 + g_P * rel), 0), 1)
  y_tox <- stats::rbinom(cs, 1, p_ij)
  y_eff <- stats::rbinom(cs, 1, q_ij)
  list(
    n1 = sum(y_tox == 0 & y_eff == 1),   # slot1 eff & no-tox
    n2 = sum(y_tox == 1 & y_eff == 1),   # slot2 eff & tox
    n3 = sum(y_tox == 0 & y_eff == 0),   # slot3 no-eff & no-tox
    n4 = sum(y_tox == 1 & y_eff == 0),   # slot4 no-eff & tox
    pk_vals = r_ij
  )
}

# ---- per-dose PK running accumulators -> mean & SD ------------------
# pk_sum, pk_sumsq, pk_n are length-D vectors accumulated across cohorts.
# Returns list(mean = length-D (NA if n<1), sd = length-D (NA if n<2)).
pkboin_pk_summary <- function(pk_sum, pk_sumsq, pk_n) {
  D <- length(pk_n)
  mean_pk <- ifelse(pk_n >= 1, pk_sum / pk_n, NA_real_)
  var_pk  <- ifelse(pk_n >= 2,
                    (pk_sumsq - (pk_sum^2) / pk_n) / (pk_n - 1), NA_real_)
  sd_pk   <- ifelse(!is.na(var_pk) & var_pk >= 0, sqrt(var_pk), NA_real_)
  list(mean = mean_pk, sd = sd_pk)
}

# ---- one simulated PKBOIN-12 trial ----------------------------------
# design    : list(phi_T, phi_E, CT, CE, phi1, phi2)
# pk_design : list(r_P, r_I_mult, C_P, CV, g_P, zeta1)  (zeta1 optional:
#             recomputed from r_P/r_I_mult if absent)
# trial     : list(n_dose, cohort_size, n_max, start_dose)
# Returns list(obd, alloc, dlt, eff, pk_terminated, n_pk_elim).
pkboin_one_trial <- function(p_true, q_true, r_true, design, pk_design,
                             trial, boundaries, u) {
  D  <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  CV  <- pk_design$CV
  g_P <- pk_design$g_P
  r_P <- pk_design$r_P
  C_P <- pk_design$C_P
  zeta1 <- if (!is.null(pk_design$zeta1)) pk_design$zeta1 else
    pkboin_zeta1(r_P, pk_design$r_I_mult)

  obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  pk_sum   <- numeric(D); pk_sumsq <- numeric(D); pk_n <- numeric(D)
  eliminated <- rep(FALSE, D)     # tox/eff cascade + futility
  pk_elim    <- rep(FALSE, D)     # PK-driven removals (persist to OBD)
  pk_terminated <- FALSE
  j <- trial$start_dose
  n_used <- 0L

  while (n_used < Nmax) {
    g <- pkboin_gen_cohort(p_true[j], q_true[j], r_true[j], cs, CV, g_P)
    obs$n1[j] <- obs$n1[j] + g$n1
    obs$n2[j] <- obs$n2[j] + g$n2
    obs$n3[j] <- obs$n3[j] + g$n3
    obs$n4[j] <- obs$n4[j] + g$n4
    obs$n[j]  <- obs$n[j] + cs
    pk_sum[j]   <- pk_sum[j]   + sum(g$pk_vals)
    pk_sumsq[j] <- pk_sumsq[j] + sum(g$pk_vals^2)
    pk_n[j]     <- pk_n[j]     + cs
    n_used <- n_used + cs

    # tox/eff elimination (cascade tox + futility) -- BOIN-12 rule reused
    el <- boin_elimination(obs[obs$n > 0, , drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    # PK elimination (paper sec 2.3.2)
    pk_summ <- pkboin_pk_summary(pk_sum, pk_sumsq, pk_n)
    obs_pk  <- pk_summ$mean
    pk_res  <- pkboin_pk_elimination(obs_pk, pk_summ$sd, pk_n, r_P, C_P)
    pk_elim <- pk_elim | pk_res$pk_elim
    if (isTRUE(pk_res$terminate)) { pk_terminated <- TRUE; break }

    dead <- eliminated | pk_elim
    if (all(dead)) break
    if (dead[1] && all(dead[obs$n > 0])) {
      # lowest active dose gone and every tried dose dead -> stop
      if (all(dead[seq_len(min(which(!dead)))])) {} # no-op guard
    }
    if (dead[1] && sum(!dead) == 0) break

    nd <- pkboin_next_dose(j, obs, obs_pk, boundaries, u, D,
                           design = design,
                           pk_design = list(zeta1 = zeta1))
    cand <- nd$admissible[!dead[nd$admissible]]
    if (length(cand) == 0) break
    j <- nd$next_dose
    if (dead[j]) j <- cand[which.max(nd$scores[as.character(cand)])]
  }

  res <- pkboin_select_obd(obs, pk_summ$mean, design,
                           list(r_P = r_P), u, pk_elim = pk_elim)
  total_dlt <- sum(obs$n2 + obs$n4)
  total_eff <- sum(obs$n1 + obs$n2)
  list(obd = res$obd, alloc = obs$n, dlt = total_dlt, eff = total_eff,
       pk_terminated = pk_terminated, n_pk_elim = sum(pk_elim))
}

# ---- full operating characteristics over n_rep replications ---------
pkboin_operating_char <- function(p_true, q_true, r_true, design, pk_design,
                                  trial, u, n_rep = 2000, seed = 1) {
  set.seed(seed)
  boundaries <- boin_boundaries(design$phi_T, phi1 = design$phi1, phi2 = design$phi2)
  zeta1 <- pkboin_zeta1(pk_design$r_P, pk_design$r_I_mult)
  pk_design$zeta1 <- zeta1
  D <- trial$n_dose

  sel   <- integer(n_rep)
  alloc <- matrix(0, n_rep, D)
  dlt_v <- numeric(n_rep)
  eff_v <- numeric(n_rep)
  pk_term_v  <- logical(n_rep)
  pk_nelim_v <- numeric(n_rep)

  for (r in seq_len(n_rep)) {
    t1 <- pkboin_one_trial(p_true, q_true, r_true, design, pk_design,
                           trial, boundaries, u)
    sel[r] <- if (is.na(t1$obd)) 0L else t1$obd
    alloc[r, ] <- t1$alloc
    dlt_v[r] <- t1$dlt
    eff_v[r] <- t1$eff
    pk_term_v[r]  <- t1$pk_terminated
    pk_nelim_v[r] <- t1$n_pk_elim
  }

  sel_pct <- sapply(0:D, function(d) mean(sel == d)) * 100
  names(sel_pct) <- c("none", paste0("dose", 1:D))
  mean_alloc <- colMeans(alloc)

  true_obd <- pkboin_true_obd(p_true, q_true, r_true,
                              list(phi_T = design$phi_T, phi_E = design$phi_E),
                              boundaries, u, list(r_P = pk_design$r_P))
  overdose <- p_true > design$phi_T
  correct_sel_pct <- if (is.na(true_obd)) NA_real_ else unname(sel_pct[paste0("dose", true_obd)])
  n_at_obd        <- if (is.na(true_obd)) NA_real_ else unname(mean_alloc[true_obd])
  n_at_overdose   <- sum(mean_alloc[overdose])
  poor_thresh <- trial$n_max / D
  poor_alloc_pct <- if (is.na(true_obd)) NA_real_ else mean(alloc[, true_obd] < poor_thresh) * 100

  list(
    selection_pct     = sel_pct,
    mean_alloc        = mean_alloc,
    early_stop_pct    = mean(sel == 0) * 100,
    true_obd          = true_obd,
    overdose          = overdose,
    correct_sel_pct   = correct_sel_pct,
    n_at_obd          = n_at_obd,
    n_at_overdose     = n_at_overdose,
    poor_alloc_pct    = poor_alloc_pct,
    mean_dlt          = mean(dlt_v),
    mean_eff          = mean(eff_v),
    pk_early_term_pct = mean(pk_term_v) * 100,   # PK-driven termination %
    mean_pk_elim      = mean(pk_nelim_v),        # mean # PK-eliminated doses
    zeta1             = zeta1
  )
}

# =====================================================================
# Representative trajectory (pure; additive) -- one PKBOIN-12 trial with
# a cohort-by-cohort log for the Data tab. Adds r_hat, pk_adequate,
# d_star, d_pk_min columns beyond boin_one_trial_traj()'s log.
# =====================================================================
pkboin_one_trial_traj <- function(p_true, q_true, r_true, design, pk_design,
                                  trial, boundaries, u) {
  D  <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  CV  <- pk_design$CV
  g_P <- pk_design$g_P
  r_P <- pk_design$r_P
  C_P <- pk_design$C_P
  zeta1 <- if (!is.null(pk_design$zeta1)) pk_design$zeta1 else
    pkboin_zeta1(r_P, pk_design$r_I_mult)

  obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  pk_sum <- numeric(D); pk_sumsq <- numeric(D); pk_n <- numeric(D)
  eliminated <- rep(FALSE, D); pk_elim <- rep(FALSE, D)
  j <- trial$start_dose; n_used <- 0L
  log_rows <- list(); cohort_idx <- 0L
  pk_summ <- list(mean = rep(NA_real_, D), sd = rep(NA_real_, D))

  while (n_used < Nmax) {
    cohort_idx <- cohort_idx + 1L
    g <- pkboin_gen_cohort(p_true[j], q_true[j], r_true[j], cs, CV, g_P)
    obs$n1[j] <- obs$n1[j] + g$n1; obs$n2[j] <- obs$n2[j] + g$n2
    obs$n3[j] <- obs$n3[j] + g$n3; obs$n4[j] <- obs$n4[j] + g$n4
    obs$n[j]  <- obs$n[j] + cs
    pk_sum[j]   <- pk_sum[j]   + sum(g$pk_vals)
    pk_sumsq[j] <- pk_sumsq[j] + sum(g$pk_vals^2)
    pk_n[j]     <- pk_n[j]     + cs
    n_used <- n_used + cs

    el <- boin_elimination(obs[obs$n > 0, , drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated
    pk_summ <- pkboin_pk_summary(pk_sum, pk_sumsq, pk_n)
    obs_pk  <- pk_summ$mean
    pk_res  <- pkboin_pk_elimination(obs_pk, pk_summ$sd, pk_n, r_P, C_P)
    pk_elim <- pk_elim | pk_res$pk_elim

    stop_now <- FALSE; decision <- "continue"; next_j <- NA_integer_
    admissible_str <- ""; rds_next <- NA_real_
    d_star_v <- NA_integer_; d_pk_min_v <- NA_integer_
    pk_adeq <- NA
    r_hat_j <- obs_pk[j]

    if (isTRUE(pk_res$terminate)) {
      stop_now <- TRUE; decision <- "stop: top-dose PK inefficacious (terminate)"
    } else {
      dead <- eliminated | pk_elim
      if (all(dead)) {
        stop_now <- TRUE; decision <- "stop: all doses eliminated"
      } else if (dead[1] && sum(!dead) == 0) {
        stop_now <- TRUE; decision <- "stop: no admissible dose"
      } else {
        nd <- pkboin_next_dose(j, obs, obs_pk, boundaries, u, D,
                               design = design, pk_design = list(zeta1 = zeta1))
        admissible_str <- paste(nd$admissible, collapse = ",")
        d_star_v <- nd$d_star; d_pk_min_v <- nd$d_pk_min; pk_adeq <- nd$pk_adequate
        cand <- nd$admissible[!dead[nd$admissible]]
        if (length(cand) == 0) {
          stop_now <- TRUE; decision <- "stop: no admissible dose"
        } else {
          next_j <- nd$next_dose
          if (dead[next_j]) next_j <- cand[which.max(nd$scores[as.character(cand)])]
          rds_next <- unname(nd$scores[as.character(next_j)])
          decision <- if (next_j > j) "escalate" else if (next_j < j) "de-escalate" else "stay"
        }
      }
    }

    log_rows[[cohort_idx]] <- data.frame(
      cohort     = cohort_idx,
      dose       = j,
      cohort_n   = cs,
      cohort_tox = g$n2 + g$n4,
      cohort_eff = g$n1 + g$n2,
      cum_n      = obs$n[j],
      cum_tox    = obs$n2[j] + obs$n4[j],
      cum_eff    = obs$n1[j] + obs$n2[j],
      r_hat      = ifelse(is.na(r_hat_j), NA_real_, round(r_hat_j, 1)),
      pk_adequate = pk_adeq,
      d_star     = d_star_v,
      d_pk_min   = d_pk_min_v,
      admissible = admissible_str,
      rds_next   = ifelse(is.na(rds_next), NA_real_, round(rds_next, 4)),
      decision   = decision,
      next_dose  = next_j
    )

    if (stop_now) break
    j <- next_j
  }

  res <- pkboin_select_obd(obs, pk_summ$mean, design,
                           list(r_P = r_P), u, pk_elim = pk_elim)
  list(obd = res$obd, alloc = obs$n, trajectory = do.call(rbind, log_rows),
       zeta1 = zeta1)
}
