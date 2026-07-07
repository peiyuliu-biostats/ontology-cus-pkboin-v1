# =====================================================================
# PKBOIN-12 final OBD selection (pure function)
# ---------------------------------------------------------------------
# Faithful to Sun & Tu (2024, Pharmaceutical Statistics 24:e2444),
# section 2.3.3 (OBD Selection), the three-step procedure:
#   1. determine the MTD by isotonic (PAVA) regression on observed
#      toxicity rates p_hat -> p_tilde, d_MTD = argmin_d |p_tilde - phi_T|;
#   2. determine the minimum efficacious PK-exposure dose d*_PK,min by
#      isotonic regression on observed mean PK r_hat -> r_tilde,
#      d*_PK,min = argmin_d |r_tilde - r_P|;
#   3. OBD = the dose in {d*_PK,min, ..., d_MTD} that is NOT eliminated
#      and has the highest estimated utility.
#
# This is a STRICT EXTENSION of boin_select_obd() (fun_boin_obd.R):
# steps 1 and 3 reuse the SAME isotonic-MTD, utility (posterior-mean EU),
# and elimination machinery as BOIN-12; PKBOIN-12 only inserts step 2,
# which RAISES the lower end of the final admissible set from dose 1
# (BOIN-12's default) to d*_PK,min. Setting r_P very low (so r_tilde > r_P
# everywhere -> d*_PK,min = 1) recovers boin_select_obd() exactly, which
# is the paper's stated graceful degradation to BOIN-12.
#
# Depends (intra-family only): fun_boin_pava.R (boin_pava_increasing),
# fun_boin_decision.R (boin_elimination, boin_utility_posterior_mean).
# No functions/stein/* dependency.
# =====================================================================

# obs   : data.frame(dose, n, n1, n2, n3, n4); tried doses (n>0) present.
# obs_pk: observed mean PK per dose. IMPORTANT indexing: this vector is
#         length n_dose = max(obs$dose) and indexed by DOSE LEVEL (obs_pk[d]),
#         NA for untried doses -- NOT aligned to the tried-only rows. The
#         function realigns it to the tried doses internally.
# design: list(phi_T, phi_E, CT, CE)
# pk_design: list(r_P)  (target PK value)
# u     : SLOT utility c(u1..u4)
# pk_elim: OPTIONAL logical length-n_dose of doses already PK-eliminated
#          DURING the trial (from pkboin_pk_elimination); merged into the
#          final elimination so trial-time PK removals persist to OBD
#          selection, matching paper step 3 ("has not been eliminated
#          during the trial"). Doses may be NA-indexed by dose level.
# Returns list(obd, d_mtd, d_pk_min, summary). summary adds r_hat,
# r_tilde, d_pk_min, pk_eliminated columns to boin_select_obd()'s frame.
pkboin_select_obd <- function(obs, obs_pk, design, pk_design, u,
                              pk_elim = NULL) {
  tried <- obs[obs$n > 0, , drop = FALSE]
  tried <- tried[order(tried$dose), ]
  if (nrow(tried) == 0) {
    return(list(obd = NA_integer_, d_mtd = NA_integer_,
                d_pk_min = NA_integer_, summary = tried))
  }

  n_tox <- tried$n2 + tried$n4
  n_eff <- tried$n1 + tried$n2
  p_hat <- n_tox / tried$n
  q_hat <- n_eff / tried$n

  # realign PK vector (indexed by dose level) to the tried doses
  r_hat_tried <- obs_pk[tried$dose]

  # (1) isotonic toxicity -> p_tilde ; d_MTD = argmin |p_tilde - phi_T|
  p_tilde <- boin_pava_increasing(p_hat, tried$n)
  d_mtd_idx <- which.min(abs(p_tilde - design$phi_T))
  d_mtd     <- tried$dose[d_mtd_idx]

  # (2) isotonic PK -> r_tilde ; d*_PK,min = argmin |r_tilde - r_P|.
  #     PK is assumed monotone increasing in dose, same as toxicity, so
  #     we PAVA-smooth r_hat with sample-size weights. Untried-dose NAs
  #     among tried rows cannot occur (tried rows all have n>0); if the
  #     PK value is nonetheless missing we drop to r_P (neutral).
  r_P <- if (!is.null(pk_design)) pk_design$r_P else NA_real_
  if (is.null(pk_design) || is.na(r_P)) {
    # no PK design -> behave exactly like BOIN-12 (floor at lowest tried)
    d_pk_min <- tried$dose[1]
    r_tilde  <- rep(NA_real_, nrow(tried))
  } else {
    r_in <- r_hat_tried
    r_in[is.na(r_in)] <- r_P
    r_tilde <- boin_pava_increasing(r_in, tried$n)
    d_pk_idx <- which.min(abs(r_tilde - r_P))
    d_pk_min <- tried$dose[d_pk_idx]
  }

  # per-dose point utility (posterior-mean EU_d) -- same as BOIN-12
  U <- vapply(seq_len(nrow(tried)), function(i) {
    boin_utility_posterior_mean(tried$n1[i], tried$n2[i], tried$n3[i], tried$n4[i], u)
  }, numeric(1))

  # tox/eff elimination (cascade tox + futility) -- same rule as BOIN-12
  elim <- boin_elimination(tried, design$phi_T, design$phi_E, design$CT, design$CE)

  # merge trial-time PK eliminations (indexed by dose level) onto tried rows
  pk_elim_tried <- rep(FALSE, nrow(tried))
  if (!is.null(pk_elim)) {
    pe <- pk_elim[tried$dose]
    pe[is.na(pe)] <- FALSE
    pk_elim_tried <- as.logical(pe)
  }
  eliminated_all <- elim$eliminated | pk_elim_tried

  # (3) OBD = argmax EU among {d*_PK,min <= dose <= d_MTD, not eliminated}
  admissible_final <- (tried$dose >= d_pk_min) & (tried$dose <= d_mtd) &
                      (!eliminated_all)
  U_eff <- ifelse(admissible_final, U, -Inf)
  obd <- if (all(is.infinite(U_eff))) NA_integer_ else tried$dose[which.max(U_eff)]

  summary <- data.frame(
    dose             = tried$dose,
    n                = tried$n,
    p_hat            = p_hat,
    p_tilde          = p_tilde,
    q_hat            = q_hat,
    r_hat            = r_hat_tried,
    r_tilde          = r_tilde,
    utility          = U,
    eliminated       = elim$eliminated,
    pk_eliminated    = pk_elim_tried,
    d_mtd            = d_mtd,
    d_pk_min         = d_pk_min,
    admissible_final = admissible_final
  )
  list(obd = obd, d_mtd = d_mtd, d_pk_min = d_pk_min, summary = summary)
}

# =====================================================================
# "True" OBD (oracle) for PKBOIN-12 from scenario truths p_true, q_true,
# r_true (per-dose true mean PK). Extends boin_true_obd() by RESTRICTING
# the oracle admissible set to doses with adequate true PK exposure,
# mirroring the estimated three-step selection:
#   * d_MTD_true  = argmin_d |p_true - phi_T|
#   * d_PK,min_true = argmin_d |r_true - r_P|  (r_true monotone in dose)
#   A dose is an oracle OBD candidate iff it is safe (p_true < phi2),
#   efficacious (q_true > phi_E), has adequate PK
#   (d_PK,min_true <= dose <= d_MTD_true). Among candidates, argmax joint
#   utility EU (conditional-independence joint truth, SLOT order, SAME
#   assumption as boin_true_obd / the simulator). Returns NA if none.
# When r_P is very low so d_PK,min_true = 1, this collapses to
# boin_true_obd() -- graceful degradation to BOIN-12.
# =====================================================================
pkboin_true_obd <- function(p_true, q_true, r_true, design, boundaries, u,
                            pk_design) {
  pi1 <- q_true * (1 - p_true)
  pi2 <- q_true * p_true
  pi3 <- (1 - q_true) * (1 - p_true)
  pi4 <- (1 - q_true) * p_true
  U <- u[1] * pi1 + u[2] * pi2 + u[3] * pi3 + u[4] * pi4

  d_mtd_true <- which.min(abs(p_true - design$phi_T))

  r_P <- pk_design$r_P
  # r_true monotone increasing -> d_PK,min_true = argmin |r_true - r_P|
  d_pk_true <- which.min(abs(r_true - r_P))

  admissible <- (p_true < boundaries$phi2) &
                (q_true > design$phi_E) &
                (seq_along(p_true) >= d_pk_true) &
                (seq_along(p_true) <= d_mtd_true)
  if (!any(admissible)) return(NA_integer_)
  cand <- which(admissible)
  cand[which.max(U[cand])]
}
