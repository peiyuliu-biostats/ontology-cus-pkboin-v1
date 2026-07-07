# =====================================================================
# STEIN trial-conduct replay from uploaded data (pure function)
# ---------------------------------------------------------------------
# Given validated cohort-level data (cohort, dose, n, n_dlt, n_eff,
# already ordered by cohort), replays the STEIN decision rule using the
# *actual* observed data -- no simulation, no randomness -- so the
# Trial Conduct tab can show, after every cohort: the admissible set,
# each candidate's posterior efficacy score, elimination status, and
# the next-dose recommendation.
#
# Does not need start_dose/cohort_size/n_max: those govern trial
# *generation* (simulate mode only), not analysis of data already
# collected. Reuses the same pure decision-rule functions as the
# simulate engine (fun_stein_decision.R), so the rule applied here is
# identical to the one used during simulation -- no logic duplicated.
# =====================================================================

stein_replay_uploaded <- function(cohort_df, design, n_dose) {
  D <- n_dose
  obs <- data.frame(dose = 1:D, n = 0L, n_dlt = 0L, n_eff = 0L)
  eliminated <- rep(FALSE, D)
  bd <- c(stein_boundaries(design$phi0, design$psi1, design$psi2,
                           phi1 = design$phi1, phi2 = design$phi2),
         list(phi0 = design$phi0))
  log_rows <- vector("list", nrow(cohort_df))

  for (i in seq_len(nrow(cohort_df))) {
    r <- cohort_df[i, ]
    j <- r$dose
    obs$n[j]     <- obs$n[j] + r$n
    obs$n_dlt[j] <- obs$n_dlt[j] + r$n_dlt
    obs$n_eff[j] <- obs$n_eff[j] + r$n_eff

    el <- stein_elimination(obs[obs$n > 0, , drop = FALSE],
                            design$phi0, design$psi1, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    nd <- stein_next_dose(j, obs, bd, D)
    admissible <- nd$admissible[!eliminated[nd$admissible]]
    next_dose <- if (length(admissible) == 0) {
      NA_integer_
    } else {
      admissible[which.max(nd$scores[as.character(admissible)])]
    }

    log_rows[[i]] <- data.frame(
      cohort         = r$cohort,
      dose           = j,
      cohort_n       = r$n,
      cohort_dlt     = r$n_dlt,
      cohort_eff     = r$n_eff,
      cum_n          = obs$n[j],
      cum_dlt        = obs$n_dlt[j],
      cum_eff        = obs$n_eff[j],
      admissible_set = paste(nd$admissible, collapse = ","),
      eliminated_now = paste(which(eliminated), collapse = ","),
      next_dose      = next_dose
    )
  }

  list(
    log        = do.call(rbind, log_rows),
    obs        = obs,
    eliminated = eliminated
  )
}
