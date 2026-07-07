# =====================================================================
# PKBOIN-12 pharmacokinetic (PK) outcome layer -- pure functions
# ---------------------------------------------------------------------
# Faithful to Sun & Tu (2024, Pharmaceutical Statistics 24:e2444),
# section 2.3 ("Proposed PKBOIN-12 Design"). This file holds ONLY the
# PK-specific primitives that BOIN-12 has no equivalent of:
#   * the PK cutoff zeta1
#   * the continuous-PK posterior and its two decision probabilities
#   * the PK elimination test  Pr(r_d < r_P | r_hat_d, n_d) > C_P
#   * d_PK,min (lowest dose with observed mean PK above zeta1)
#
# It is deliberately kept SEPARATE from fun_pkboin_decision.R /
# fun_pkboin_obd.R so the pure PK maths can be unit-checked in isolation
# and so nothing here depends on the BOIN-12 tox/eff machinery. It has
# ZERO dependency on functions/stein/* and does NOT read reactiveValues.
#
# --------------------------- PK model (paper sec 2.3.1) --------------
# Individual PK value at dose d:  r_{d,j} ~ N(r_d, sigma_d^2), i.i.d.
# Prior on the dose-level mean:   r_d ~ truncated-N(0, sigma0^2; 0, Inf),
#   sigma0 large (paper default 10000).
# Posterior mean (paper eq.):
#   r_d | . ~ truncated-N( n_d r_hat_d / (sigma_d^2 (1/sigma0^2 + n_d/sigma_d^2)),
#                          1 / (1/sigma0^2 + n_d/sigma_d^2) ; 0, Inf ).
# When 1/sigma0^2 is ignored (sigma0 large), the MAP estimate is r_hat_d
# and the posterior of r_d is approximately N(r_hat_d, sigma_d^2 / n_d).
# The paper's PK decision probabilities are computed under exactly this
# large-sigma0 normal approximation, so we use pnorm() with
#   mean = r_hat_d,  sd = sigma_d / sqrt(n_d).
# sigma_d is the (unknown) individual-level PK SD at dose d. In the
# simulator we know the data-generating SD (= CV * r_d); on real/observed
# data it is estimated by the sample SD of the PK values at dose d. Both
# are passed in as `sigma_d` so this function stays a pure primitive.
#
# --------------------------- cutoff zeta1 (paper sec 2.3.1) ----------
# Inefficacious PK value r_I = r_I_mult * r_P (paper default r_I_mult=0.6).
# Optimal cutoff minimising incorrect marginal PK decisions:
#   zeta1 = (r_P + r_I)/2 = (r_P + r_I_mult r_P)/2 = 0.8 r_P (default).
# =====================================================================

# ---- PK cutoff zeta1 -------------------------------------------------
# r_P: target PK value ; r_I_mult: multiplier for the inefficacious PK
# value r_I = r_I_mult * r_P. Returns zeta1 = (r_P + r_I) / 2.
pkboin_zeta1 <- function(r_P, r_I_mult = 0.6) {
  stopifnot(is.numeric(r_P), length(r_P) == 1, r_P > 0,
            is.numeric(r_I_mult), r_I_mult >= 0, r_I_mult <= 1)
  r_I <- r_I_mult * r_P
  (r_P + r_I) / 2
}

# ---- posterior Pr(r_d < r_P | r_hat_d, n_d) --------------------------
# Large-sigma0 normal approximation: r_d | . ~ N(r_hat, sigma_d^2 / n_d).
# Returns the left-tail posterior probability that the true dose-level
# mean PK is below the target r_P. Used by the PK elimination criterion.
# Guards: n_d < 1 or non-positive/NA sigma_d -> NA (test not evaluable).
pkboin_prob_pk_below <- function(r_hat, sigma_d, n_d, r_P) {
  if (is.na(r_hat) || is.na(sigma_d) || is.na(n_d) ||
      n_d < 1 || sigma_d <= 0) {
    return(NA_real_)
  }
  se <- sigma_d / sqrt(n_d)
  stats::pnorm(r_P, mean = r_hat, sd = se, lower.tail = TRUE)
}

# ---- PK elimination flag for one dose --------------------------------
# Paper rule (sec 2.3.2, "(PK)"):
#   if Pr(r_d < r_P | r_hat_d, n_d) > C_P and n_d >= n_pk_min, the dose
#   is flagged as having inefficacious PK exposure.
# n_pk_min defaults to 6 (paper's "n_d >= 6" guard). The *consequence*
# of the flag (eliminate lower dose vs terminate the trial for the top
# dose) is handled in fun_pkboin_decision.R::pkboin_pk_elimination(),
# because it depends on the full dose ladder; this returns only the
# per-dose Boolean test so it can be unit-checked alone. NA prob -> FALSE
# (cannot eliminate on an unevaluable test).
pkboin_pk_flag <- function(r_hat, sigma_d, n_d, r_P, C_P, n_pk_min = 6L) {
  if (is.na(n_d) || n_d < n_pk_min) return(FALSE)
  p <- pkboin_prob_pk_below(r_hat, sigma_d, n_d, r_P)
  if (is.na(p)) return(FALSE)
  p > C_P
}

# ---- d_PK,min : lowest dose with observed mean PK above zeta1 --------
# obs_pk: numeric vector of observed mean PK per dose (length D); doses
# with no data must be NA (untried). Returns the smallest dose index d
# with r_hat_d > zeta1, or NA_integer_ if none qualifies (paper: then
# d_PK,min "does not exist").
pkboin_d_pk_min <- function(obs_pk, zeta1) {
  ok <- which(!is.na(obs_pk) & obs_pk > zeta1)
  if (length(ok) == 0) return(NA_integer_)
  as.integer(min(ok))
}
