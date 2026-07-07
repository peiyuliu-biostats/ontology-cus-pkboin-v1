# =====================================================================
# BOIN-12 final OBD selection (pure function)
# ---------------------------------------------------------------------
# Faithful to Lin, Zhou, Yan, Li & Yuan (2020, JCO PO), Table 2 step 3:
#   (a) estimate the MTD by isotonic (PAVA) smoothing of the observed
#       toxicity rates -> p_tilde, then d_MTD = argmin_d |p_tilde_d - phi_T|;
#   (b) the OBD is the dose with the highest estimated utility AMONG the
#       doses NOT higher than d_MTD (i.e. dose <= d_MTD) that are also
#       not eliminated.
# The MTD-anchor in (b) was previously missing: OBD was chosen over all
# tried non-eliminated doses, which could return a dose above the MTD.
#
# Kept deliberately SEPARATE from boin_next_dose() (fun_boin_decision.R):
# interim RDS-based cohort allocation and final OBD determination are two
# distinct mechanisms that can pick different doses from the same data,
# and stay visible as separate app steps (Trial Conduct vs OBD tabs),
# mirroring STEIN's stein_next_dose() vs stein_select_obd().
#
# Depends (intra-family only) on boin_elimination() /
# boin_utility_posterior_mean() (fun_boin_decision.R) and
# boin_pava_increasing() (fun_boin_pava.R). No functions/stein/* dep.
#
# UTILITY METRIC: the per-dose point utility reported/ranked here is the
# posterior-mean expected utility EU_d (Dirichlet(1,1,1,1) prior), the
# point estimate that underlies the interim RDS's standardized
# desirability u_d = EU_d/100. Interim allocation ranks doses by RDS
# = Pr(u_d > u_b | .) (a probability), while the final OBD ranks by the
# EU_d point estimate restricted to <= d_MTD; both are the SAME utility
# scale, differing only in probability-rank vs point-estimate, exactly
# as the paper distinguishes the two steps.
# =====================================================================

# obs: data.frame(dose, n, n1, n2, n3, n4); tried doses (n > 0) only need
#      be present, untried absent.
# design: list with phi_T, phi_E, CT, CE
# u: SLOT utility vector c(u1, u2, u3, u4)
# Returns list(obd, d_mtd, summary). summary columns (unchanged names,
# plus two additive columns d_mtd / admissible_final) are:
#   dose, n, p_hat, p_tilde, q_hat, utility, eliminated,
#   d_mtd (constant per call), admissible_final (dose<=d_MTD & !elim).
boin_select_obd <- function(obs, design, u) {
  tried <- obs[obs$n > 0, , drop = FALSE]
  tried <- tried[order(tried$dose), ]
  if (nrow(tried) == 0) {
    return(list(obd = NA_integer_, d_mtd = NA_integer_, summary = tried))
  }

  n_tox <- tried$n2 + tried$n4
  n_eff <- tried$n1 + tried$n2
  p_hat <- n_tox / tried$n
  q_hat <- n_eff / tried$n

  # (a) isotonic toxicity -> p_tilde ; d_MTD = argmin |p_tilde - phi_T|
  #     among TRIED doses. Ties -> lower dose (which.min first-match),
  #     the conservative BOIN convention.
  p_tilde <- boin_pava_increasing(p_hat, tried$n)
  d_mtd_idx <- which.min(abs(p_tilde - design$phi_T))
  d_mtd     <- tried$dose[d_mtd_idx]

  # per-dose point utility (posterior-mean EU_d; same scale as interim
  # RDS's underlying u_d = EU_d/100)
  U <- vapply(seq_len(nrow(tried)), function(i) {
    boin_utility_posterior_mean(tried$n1[i], tried$n2[i], tried$n3[i], tried$n4[i], u)
  }, numeric(1))

  # elimination (cascade tox + futility) -- same rule as interim
  elim <- boin_elimination(tried, design$phi_T, design$phi_E, design$CT, design$CE)

  # (b) OBD = argmax EU among {dose <= d_MTD, not eliminated}
  admissible_final <- (tried$dose <= d_mtd) & (!elim$eliminated)
  U_eff <- ifelse(admissible_final, U, -Inf)
  obd <- if (all(is.infinite(U_eff))) NA_integer_ else tried$dose[which.max(U_eff)]

  summary <- data.frame(
    dose             = tried$dose,
    n                = tried$n,
    p_hat            = p_hat,
    p_tilde          = p_tilde,
    q_hat            = q_hat,
    utility          = U,
    eliminated       = elim$eliminated,
    d_mtd            = d_mtd,
    admissible_final = admissible_final
  )
  list(obd = obd, d_mtd = d_mtd, summary = summary)
}

# =====================================================================
# "True" OBD (oracle) from scenario truths p_true (toxicity) and
# q_true (efficacy). BOIN-12's utility is defined on the *joint*
# outcome, so an oracle needs a joint truth model; this uses the SAME
# conditional-independence assumption as boin_one_trial()/simulate
# (pi1=q(1-p), pi2=qp, pi3=(1-q)(1-p), pi4=(1-q)p; SLOT order), kept
# internally consistent with the simulated data. Explicit approximation
# (real tox/eff need not be conditionally independent given dose);
# revisit if a non-independent joint truth model is introduced.
#
# Oracle admissibility now also mirrors the estimated-OBD MTD-anchor:
# a dose is an OBD candidate only if it is safe (p_true < phi2),
# efficacious (q_true > phi_E), AND not above the true MTD
# (d_MTD_true = argmin_d |p_true - phi_T|). Among candidates, argmax
# joint-utility EU. Returns NA if none qualify.
# =====================================================================
boin_true_obd <- function(p_true, q_true, design, boundaries, u) {
  pi1 <- q_true * (1 - p_true)
  pi2 <- q_true * p_true
  pi3 <- (1 - q_true) * (1 - p_true)
  pi4 <- (1 - q_true) * p_true
  U <- u[1] * pi1 + u[2] * pi2 + u[3] * pi3 + u[4] * pi4

  d_mtd_true <- which.min(abs(p_true - design$phi_T))

  admissible <- (p_true < boundaries$phi2) &
                (q_true > design$phi_E) &
                (seq_along(p_true) <= d_mtd_true)
  if (!any(admissible)) return(NA_integer_)
  cand <- which(admissible)
  cand[which.max(U[cand])]
}

# =====================================================================
# Scenario/OC truth-derivation helper (pure; display-only).
# ---------------------------------------------------------------------
# From per-dose true (p_true, q_true) and the design (utility u, phi_T,
# phi_E), derive the full BOIN-12 utility structure the Scenario / OC
# tabs need to *display* -- it changes no decision logic and is a
# deterministic function of already-existing inputs:
#   pi1..pi4 : joint-outcome truth under conditional independence
#              (same assumption as the simulator/oracle; SLOT order
#              pi1=eff&notox, pi2=eff&tox, pi3=noeff&notox, pi4=noeff&tox)
#   EU_d     : true expected utility = sum(u * pi)  (0..100 scale)
#   RDS_true : asymptotic desirability rank at the truth. RDS on finite
#              data is Pr(u_d > u_b | .); as n -> infinity the posterior
#              collapses to the point u_d = EU_d/100, so the asymptotic
#              RDS is the indicator 1{EU_d > u_b}. Reported as 1/0 with
#              u_b the utility benchmark, so it aligns with the interim
#              RDS scale (higher u_d than benchmark = desirable).
#   d_mtd    : argmin_d |p_true - phi_T|
#   obd      : boin_true_obd() (safe & efficacious & <= d_mtd, argmax EU)
# Returns list(table = data.frame(...), d_mtd, obd, u_b).
boin_scenario_truth <- function(p_true, q_true, design, u) {
  boundaries <- boin_boundaries(design$phi_T, phi1 = design$phi1, phi2 = design$phi2)
  pi1 <- q_true * (1 - p_true)
  pi2 <- q_true * p_true
  pi3 <- (1 - q_true) * (1 - p_true)
  pi4 <- (1 - q_true) * p_true
  EU  <- u[1] * pi1 + u[2] * pi2 + u[3] * pi3 + u[4] * pi4
  u_b <- boin_utility_benchmark(u, design$phi_T, design$phi_E)
  rds_true <- as.integer(EU > u_b)

  d_mtd <- which.min(abs(p_true - design$phi_T))
  obd   <- boin_true_obd(p_true, q_true,
                         list(phi_T = design$phi_T, phi_E = design$phi_E),
                         boundaries, u)

  D <- length(p_true)
  tab <- data.frame(
    dose     = seq_len(D),
    p_true   = p_true,
    q_true   = q_true,
    pi1      = round(pi1, 3),
    pi2      = round(pi2, 3),
    pi3      = round(pi3, 3),
    pi4      = round(pi4, 3),
    EU_d     = round(EU, 2),
    RDS_true = rds_true,
    is_MTD   = ifelse(seq_len(D) == d_mtd, "\u00d7", ""),
    is_OBD   = ifelse(!is.na(obd) & seq_len(D) == obd, "\u2605", "")
  )
  list(table = tab, d_mtd = d_mtd, obd = obd, u_b = u_b)
}
