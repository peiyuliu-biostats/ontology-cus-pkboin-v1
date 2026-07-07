# =====================================================================
# BOIN-12 trial-conduct replay from uploaded data (pure function)
# ---------------------------------------------------------------------
# Given validated cohort-level joint-count data (cohort, dose, n, n1,
# n2, n3, n4, already ordered by cohort), replays the BOIN-12 interim
# rule using the *actual* observed data -- no simulation, no
# randomness -- so the Trial Conduct tab can show, after every cohort:
# the admissible set, each admissible dose's RDS (rank-based
# desirability score), the N* branch state, the decision type
# (escalate / stay / de-escalate / fast-escalate / stop), elimination
# status, and the next-dose recommendation.
#
# Does not need start_dose/cohort_size/n_max (those govern trial
# *generation* in simulate mode only). Reuses boin_elimination() and
# boin_next_dose() from fun_boin_decision.R -- the exact same interim
# rule used by the simulate engine -- so nothing is duplicated.
# =====================================================================

boin_replay_uploaded <- function(cohort_df, design, boundaries, u, n_dose) {
  D <- n_dose
  obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  eliminated <- rep(FALSE, D)
  log_rows <- vector("list", nrow(cohort_df))

  for (i in seq_len(nrow(cohort_df))) {
    r <- cohort_df[i, ]
    j <- r$dose
    obs$n[j]  <- obs$n[j]  + r$n
    obs$n1[j] <- obs$n1[j] + r$n1
    obs$n2[j] <- obs$n2[j] + r$n2
    obs$n3[j] <- obs$n3[j] + r$n3
    obs$n4[j] <- obs$n4[j] + r$n4

    el <- boin_elimination(obs[obs$n > 0, , drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    nd <- boin_next_dose(j, obs, boundaries, u, D, design = design)
    admissible <- nd$admissible[!eliminated[nd$admissible]]
    next_dose <- if (length(admissible) == 0) {
      NA_integer_
    } else {
      admissible[which.max(nd$scores[as.character(admissible)])]
    }

    # BOIN12 transparency (all derived from the already-computed nd; no
    # decision logic added): N* state at current dose, per-dose RDS in
    # the admissible set, RDS of the recommended dose, and the decision
    # type consistent with next_dose vs current dose.
    n_cur    <- obs$n[j]
    n_ge_Ns  <- if (n_cur >= 6L) "yes" else "no"
    rds_str  <- paste(sprintf("d%d=%.3f", nd$admissible, unname(nd$scores)), collapse = "  ")
    rds_next <- if (is.na(next_dose)) NA_real_ else unname(nd$scores[as.character(next_dose)])
    higher_used_j <- (j + 1 <= D) && (obs$n[j + 1] > 0)
    p_cur    <- if (n_cur > 0) (obs$n2[j] + obs$n4[j]) / n_cur else 0
    decision <- if (is.na(next_dose)) {
      "stop: no admissible dose"
    } else if (next_dose > j) {
      if (p_cur <= boundaries$lambda_e && n_cur >= 9L && !higher_used_j) "fast-escalate (n>=9)" else "escalate"
    } else if (next_dose < j) {
      "de-escalate"
    } else {
      "stay"
    }

    log_rows[[i]] <- data.frame(
      cohort         = r$cohort,
      dose           = j,
      cohort_n       = r$n,
      cohort_tox     = r$n2 + r$n4,
      cohort_eff     = r$n1 + r$n2,
      cum_n          = obs$n[j],
      cum_tox        = obs$n2[j] + obs$n4[j],
      cum_eff        = obs$n1[j] + obs$n2[j],
      n_ge_Nstar     = n_ge_Ns,
      admissible_set = paste(nd$admissible, collapse = ","),
      rds_by_dose    = rds_str,
      rds_next       = ifelse(is.na(rds_next), NA_real_, round(rds_next, 4)),
      eliminated_now = paste(which(eliminated), collapse = ","),
      decision       = decision,
      next_dose      = next_dose
    )
  }

  list(
    log        = do.call(rbind, log_rows),
    obs        = obs,
    eliminated = eliminated
  )
}
