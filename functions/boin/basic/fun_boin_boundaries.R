# =====================================================================
# BOIN-12 toxicity guardrail boundaries (pure functions, no Shiny dep)
# ---------------------------------------------------------------------
# Stage A3. Standard two-sided BOIN escalation/de-escalation boundaries
# derived from the target toxicity probability phi_T and its lower/
# upper anchors phi1 < phi_T < phi2, following the original BOIN design
# (Liu & Yuan, 2015, JRSS-C 64:507-523), which BOIN-12 (Lin, Zhou &
# Yuan) retains unchanged as the toxicity safety guardrail underneath
# its utility-based dose comparison.
#
# lambda_e = log((1-phi1)/(1-phi_T)) / log( phi_T(1-phi1) / (phi1(1-phi_T)) )
# lambda_d = log((1-phi_T)/(1-phi2)) / log( phi2(1-phi_T) / (phi_T(1-phi2)) )
# Both are independent of sample size (same likelihood-ratio boundary
# construction as STEIN's phiL/phiU, just anchored at different points
# with different default multiples).
#
# Default anchors follow the BOIN design's standard recommendation:
#   phi1 = 0.6 * phi_T   (highest toxicity considered subtherapeutic)
#   phi2 = 1.4 * phi_T   (lowest toxicity considered overly toxic)
# These 0.6/1.4 multiples are the commonly-cited BOIN defaults; if your
# reference implementation/paper uses table-optimized (non-0.6/1.4)
# anchors for a specific phi_T, override via the phi1/phi2 arguments
# below rather than editing the defaults, so the generic function stays
# reusable.
#
# NOTE: this file is fully independent of functions/stein/* -- the
# generic LR-boundary math is duplicated (not shared) so editing this
# file can never change STEIN's fun_stein_boundaries.R behaviour.
#
# TODO (validation, stage A/B): numerically cross-check lambda_e/
# lambda_d against a published BOIN-12 worked example or Liu & Yuan
# (2015) Table 1 for the same phi_T -- no R runtime is available in
# this authoring environment, so this has not yet been executed.
# =====================================================================

# generic likelihood-ratio boundary between two anchors a < b, both in
# (0,1); returns the boundary proportion t in (a, b) at which the two
# point-hypothesis likelihoods (p = a vs p = b) are equal.
boin_lr_boundary <- function(a, b) {
  stopifnot(a > 0, b < 1, a < b)
  num <- log((1 - a) / (1 - b))
  den <- log((b * (1 - a)) / (a * (1 - b)))
  num / den
}

# toxicity escalation (lambda_e) / de-escalation (lambda_d) boundaries
# from phi_T and its anchors phi1 < phi_T < phi2.
boin_phi_bounds <- function(phi_T, phi1, phi2) {
  stopifnot(phi1 < phi_T, phi_T < phi2, phi1 > 0, phi2 < 1)
  list(
    lambda_e = boin_lr_boundary(phi1, phi_T),
    lambda_d = boin_lr_boundary(phi_T, phi2)
  )
}

# convenience: full boundary set from the design parameters. Mirrors
# stein_boundaries()'s calling convention (phi1/phi2 optionally
# overridden; otherwise derived from the *_mult defaults) so the two
# families of tab modules (stein_* vs boin_*) can share the same
# calling pattern in the UI layer.
boin_boundaries <- function(phi_T, phi1_mult = 0.6, phi2_mult = 1.4,
                             phi1 = NULL, phi2 = NULL) {
  if (is.null(phi1)) phi1 <- phi1_mult * phi_T
  if (is.null(phi2)) phi2 <- phi2_mult * phi_T
  b <- boin_phi_bounds(phi_T, phi1, phi2)
  list(
    phi1 = phi1, phi2 = phi2,
    lambda_e = b$lambda_e, lambda_d = b$lambda_d
  )
}
