# =====================================================================
# STEIN dose-finding decision rules (pure functions)
# ---------------------------------------------------------------------
# Given current dose, observed counts, and boundaries, returns the local
# admissible set, the posterior-probability ranking Pr(q > psi | data),
# elimination flags, and the recommended next dose. Follows Lin & Yin
# (2017), Section 2.3, with Beta(1,1) priors.
# =====================================================================

# local admissible set A_j based on current-dose toxicity proportion
stein_admissible_set <- function(j, phat_j, phiL, phiU, n_dose) {
  if (phat_j <= phiL) {
    cand <- c(j - 1, j, j + 1)
  } else if (phat_j < phiU) {
    cand <- c(j - 1, j)
  } else {
    cand <- c(j - 1)
  }
  cand <- cand[cand >= 1 & cand <= n_dose]
  sort(unique(cand))
}

# posterior Pr(q > psi | y, n) under Beta(1,1) prior -> Beta(1+y, 1+n-y)
stein_prob_eff_above <- function(y, n, psi) {
  pbeta(psi, 1 + y, 1 + n - y, lower.tail = FALSE)
}

# posterior Pr(p > phi0 | data) for toxicity elimination (Beta(1,1) prior)
stein_prob_tox_above <- function(n_dlt, n, phi0) {
  pbeta(phi0, 1 + n_dlt, 1 + n - n_dlt, lower.tail = FALSE)
}

# posterior Pr(q <= psi1 | data) for efficacy (futility) elimination
stein_prob_eff_below <- function(n_eff, n, psi1) {
  pbeta(psi1, 1 + n_eff, 1 + n - n_eff, lower.tail = TRUE)
}

# elimination status per dose given observed data and cutoffs
stein_elimination <- function(obs, phi0, psi1, CT, CE) {
  # obs: data.frame(dose, n, n_dlt, n_eff)
  elim_tox <- with(obs, stein_prob_tox_above(n_dlt, n, phi0) > CT)
  elim_eff <- with(obs, stein_prob_eff_below(n_eff, n, psi1) > CE)
  data.frame(
    dose = obs$dose,
    elim_tox = elim_tox,
    elim_eff = elim_eff,
    eliminated = elim_tox | elim_eff
  )
}

# recommend next dose: within admissible set, pick largest Pr(q>psi|data);
# ties -> lower dose. Fully observed toxicity de-escalation handled by set.
stein_next_dose <- function(j, obs, boundaries, n_dose) {
  phi0  <- boundaries$phi0
  phiL  <- boundaries$phiL
  phiU  <- boundaries$phiU
  psi   <- boundaries$psi
  cur <- obs[obs$dose == j, , drop = FALSE]
  phat_j <- if (nrow(cur) == 1 && cur$n > 0) cur$n_dlt / cur$n else 0
  A <- stein_admissible_set(j, phat_j, phiL, phiU, n_dose)
  # posterior efficacy for each admissible dose (no data -> Beta(1,1))
  score <- vapply(A, function(d) {
    row <- obs[obs$dose == d, , drop = FALSE]
    if (nrow(row) == 1 && row$n > 0) {
      stein_prob_eff_above(row$n_eff, row$n, psi)
    } else {
      1 - psi   # Beta(1,1): Pr(q > psi) = 1 - psi
    }
  }, numeric(1))
  best <- A[which.max(score)]   # which.max returns first on ties -> lower dose
  list(admissible = A, scores = setNames(score, A), next_dose = best)
}
