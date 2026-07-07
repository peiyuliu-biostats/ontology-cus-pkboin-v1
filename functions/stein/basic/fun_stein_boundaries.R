# =====================================================================
# STEIN decision boundaries (pure functions, no Shiny dependency)
# ---------------------------------------------------------------------
# Derives the toxicity interval boundaries (phiL, phiU) and the efficacy
# cutoff (psi) by minimizing misclassification probability, following
# Lin & Yin (2017), Statistics in Medicine 36:4106-4120.
#
# The generic optimal boundary between two point hypotheses p = a vs p = b
# (a < b) is the sample proportion t where the two likelihoods are equal:
#   t = log((1-b)/(1-a)) / log( (b(1-a)) / (a(1-b)) )
# This is independent of sample size.
# =====================================================================

# generic likelihood-ratio boundary between two anchors a < b.
# Orientation follows Lin & Yin (2017) exactly so the boundary is the
# positive proportion in (a, b):
#   t = log((1-a)/(1-b)) / log( (b(1-a)) / (a(1-b)) )
# Verified against the paper's BKM120 illustration (phi0=0.33 ->
# phiL=0.2876, phiU=0.3706; psi=0.5609 for psi1=0.30, psi2=0.80).
stein_lr_boundary <- function(a, b) {
  # a < b, both in (0,1); returns the boundary proportion in (a, b)
  num <- log((1 - a) / (1 - b))
  den <- log((b * (1 - a)) / (a * (1 - b)))
  num / den
}

# toxicity lower/upper thresholds from phi0 and its anchors phi1 < phi0 < phi2
stein_phi_bounds <- function(phi0, phi1, phi2) {
  stopifnot(phi1 < phi0, phi0 < phi2, phi1 > 0, phi2 < 1)
  phiL <- stein_lr_boundary(phi1, phi0)   # boundary between phi1 and phi0
  phiU <- stein_lr_boundary(phi0, phi2)   # boundary between phi0 and phi2
  list(phiL = phiL, phiU = phiU)
}

# efficacy cutoff psi from psi1 < psi2
stein_psi_cutoff <- function(psi1, psi2) {
  stopifnot(psi1 < psi2, psi1 > 0, psi2 < 1)
  stein_lr_boundary(psi1, psi2)
}

# convenience: full boundary set from the design parameters
stein_boundaries <- function(phi0, psi1, psi2,
                             phi1_mult = 0.75, phi2_mult = 1.25,
                             phi1 = NULL, phi2 = NULL) {
  if (is.null(phi1)) phi1 <- phi1_mult * phi0
  if (is.null(phi2)) phi2 <- phi2_mult * phi0
  tb <- stein_phi_bounds(phi0, phi1, phi2)
  list(
    phi1 = phi1, phi2 = phi2,
    phiL = tb$phiL, phiU = tb$phiU,
    psi  = stein_psi_cutoff(psi1, psi2)
  )
}
