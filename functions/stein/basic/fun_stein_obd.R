# =====================================================================
# STEIN final OBD selection (pure function)
# ---------------------------------------------------------------------
# 1. toxicity isotonic (PAVA) -> p_tilde
# 2. efficacy unimodal + AIC model averaging -> q_tilde
# 3. utility U = q_tilde - w1*p_tilde - w2*p_tilde*I(p_tilde > phi0)
#    OBD = argmax U among tried, non-eliminated doses.
# Depends on fun_stein_pava.R and fun_stein_decision.R (auto-sourced).
# =====================================================================

stein_select_obd <- function(obs, design) {
  # obs: data.frame(dose, n, n_dlt, n_eff) for tried doses (n > 0)
  # design: list with phi0, psi1, w1, w2, CT, CE
  tried <- obs[obs$n > 0, , drop = FALSE]
  tried <- tried[order(tried$dose), ]
  if (nrow(tried) == 0) {
    return(list(obd = NA_integer_, summary = tried))
  }

  phat <- tried$n_dlt / tried$n
  p_tilde <- stein_pava_increasing(phat, tried$n)

  ma <- stein_efficacy_model_avg(tried$n_eff, tried$n)
  q_tilde <- ma$qtilde

  U <- q_tilde - design$w1 * p_tilde -
    design$w2 * p_tilde * as.numeric(p_tilde > design$phi0)

  elim <- stein_elimination(tried, design$phi0, design$psi1, design$CT, design$CE)
  U_eff <- ifelse(elim$eliminated, -Inf, U)

  obd <- if (all(is.infinite(U_eff))) NA_integer_ else tried$dose[which.max(U_eff)]

  summary <- data.frame(
    dose     = tried$dose,
    n        = tried$n,
    p_hat    = phat,
    p_tilde  = p_tilde,
    q_hat    = tried$n_eff / tried$n,
    q_tilde  = q_tilde,
    utility  = U,
    eliminated = elim$eliminated
  )
  list(obd = obd, summary = summary, eff_weights = ma$weights)
}

# =====================================================================
# "True" OBD (oracle) from the scenario's true p/q curves (additive;
# does not alter stein_select_obd above, which selects from *observed*
# trial data). Used only in the Scenario/Data tabs to mark which dose
# is truly optimal under the assumed truth, for comparison against the
# simulation's observed selection percentages.
# ---------------------------------------------------------------------
# A dose is admissible if it is not overtly toxic (p_true < phiU) and
# not futile (q_true > psi1); OBD is the admissible dose maximizing the
# same utility form used for observed-data selection, evaluated at the
# true rates directly (no isotonic smoothing needed since truth is
# already the population curve).
# =====================================================================
stein_true_obd <- function(p_true, q_true, design, boundaries) {
  U <- q_true - design$w1 * p_true -
    design$w2 * p_true * as.numeric(p_true > design$phi0)
  admissible <- (p_true < boundaries$phiU) & (q_true > design$psi1)
  if (!any(admissible)) return(NA_integer_)
  cand <- which(admissible)
  cand[which.max(U[cand])]
}
