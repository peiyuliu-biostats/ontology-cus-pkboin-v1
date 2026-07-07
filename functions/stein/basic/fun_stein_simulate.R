# =====================================================================
# STEIN operating-characteristics simulation (pure function)
# ---------------------------------------------------------------------
# Runs the full STEIN trial repeatedly under known true p/q to produce
# selection %, patient allocation, and early-stop %. Self-contained;
# depends only on the other pure STEIN functions (auto-sourced).
# =====================================================================

# one simulated STEIN trial -> selected OBD (NA if all eliminated) + allocation
stein_one_trial <- function(p_true, q_true, design, trial, boundaries) {
  D  <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  obs <- data.frame(dose = 1:D, n = 0L, n_dlt = 0L, n_eff = 0L)
  eliminated <- rep(FALSE, D)
  j <- trial$start_dose
  n_used <- 0L
  bd <- c(boundaries, list(phi0 = design$phi0))

  while (n_used < Nmax) {
    # enroll a cohort at dose j
    dlt <- rbinom(1, cs, p_true[j])
    eff <- rbinom(1, cs, q_true[j])
    obs$n[j]     <- obs$n[j] + cs
    obs$n_dlt[j] <- obs$n_dlt[j] + dlt
    obs$n_eff[j] <- obs$n_eff[j] + eff
    n_used <- n_used + cs

    # elimination check
    el <- stein_elimination(obs[obs$n > 0, , drop = FALSE],
                            design$phi0, design$psi1, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated
    if (all(eliminated[1:D][obs$n > 0]) && all(eliminated)) break
    if (eliminated[1]) break   # lowest dose too toxic -> stop

    # next dose among non-eliminated
    nd <- stein_next_dose(j, obs, bd, D)
    cand <- nd$admissible[!eliminated[nd$admissible]]
    if (length(cand) == 0) break
    j <- nd$next_dose
    if (eliminated[j]) j <- cand[which.max(nd$scores[as.character(cand)])]
  }

  # final OBD
  res <- stein_select_obd(obs, design)
  list(obd = res$obd, alloc = obs$n)
}

# full operating characteristics over n_rep replications
stein_operating_char <- function(p_true, q_true, design, trial, n_rep = 2000,
                                 seed = 1) {
  set.seed(seed)
  bd <- stein_boundaries(design$phi0, design$psi1, design$psi2,
                         phi1 = design$phi1, phi2 = design$phi2)
  D <- trial$n_dose
  sel   <- integer(n_rep)
  alloc <- matrix(0, n_rep, D)
  for (r in seq_len(n_rep)) {
    t1 <- stein_one_trial(p_true, q_true, design, trial, bd)
    sel[r] <- if (is.na(t1$obd)) 0L else t1$obd
    alloc[r, ] <- t1$alloc
  }
  sel_pct <- sapply(0:D, function(d) mean(sel == d)) * 100
  names(sel_pct) <- c("none", paste0("dose", 1:D))
  list(
    selection_pct   = sel_pct,
    mean_alloc      = colMeans(alloc),
    early_stop_pct  = mean(sel == 0) * 100
  )
}

# =====================================================================
# Representative trajectory (pure function; additive, does not alter
# stein_one_trial / stein_operating_char above).
# ---------------------------------------------------------------------
# Runs exactly one STEIN trial and records a cohort-by-cohort log
# (dose, cohort/cumulative counts, decision) for display in the Data
# tab. Static table, not an animation, per design decision.
# =====================================================================
stein_one_trial_traj <- function(p_true, q_true, design, trial, boundaries) {
  D  <- trial$n_dose
  cs <- trial$cohort_size
  Nmax <- trial$n_max
  obs <- data.frame(dose = 1:D, n = 0L, n_dlt = 0L, n_eff = 0L)
  eliminated <- rep(FALSE, D)
  j <- trial$start_dose
  n_used <- 0L
  bd <- c(boundaries, list(phi0 = design$phi0))
  log_rows <- list()
  cohort_idx <- 0L

  while (n_used < Nmax) {
    cohort_idx <- cohort_idx + 1L
    dlt <- rbinom(1, cs, p_true[j])
    eff <- rbinom(1, cs, q_true[j])
    obs$n[j]     <- obs$n[j] + cs
    obs$n_dlt[j] <- obs$n_dlt[j] + dlt
    obs$n_eff[j] <- obs$n_eff[j] + eff
    n_used <- n_used + cs

    el <- stein_elimination(obs[obs$n > 0, , drop = FALSE],
                            design$phi0, design$psi1, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    stop_now <- FALSE
    decision <- "continue"
    next_j <- NA_integer_

    if (all(eliminated[1:D][obs$n > 0]) && all(eliminated)) {
      stop_now <- TRUE; decision <- "stop: all doses eliminated"
    } else if (eliminated[1]) {
      stop_now <- TRUE; decision <- "stop: dose 1 too toxic"
    } else {
      nd <- stein_next_dose(j, obs, bd, D)
      cand <- nd$admissible[!eliminated[nd$admissible]]
      if (length(cand) == 0) {
        stop_now <- TRUE; decision <- "stop: no admissible dose"
      } else {
        next_j <- nd$next_dose
        if (eliminated[next_j]) next_j <- cand[which.max(nd$scores[as.character(cand)])]
        decision <- if (next_j > j) "escalate" else if (next_j < j) "de-escalate" else "stay"
      }
    }

    log_rows[[cohort_idx]] <- data.frame(
      cohort     = cohort_idx,
      dose       = j,
      cohort_n   = cs,
      cohort_dlt = dlt,
      cohort_eff = eff,
      cum_n      = obs$n[j],
      cum_dlt    = obs$n_dlt[j],
      cum_eff    = obs$n_eff[j],
      decision   = decision,
      next_dose  = next_j
    )

    if (stop_now) break
    j <- next_j
  }

  res <- stein_select_obd(obs, design)
  list(obd = res$obd, alloc = obs$n, trajectory = do.call(rbind, log_rows))
}
