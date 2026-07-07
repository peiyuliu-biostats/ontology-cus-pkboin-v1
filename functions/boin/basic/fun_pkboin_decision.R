# =====================================================================
# PKBOIN-12 interim dose-finding decision rules (pure functions)
# ---------------------------------------------------------------------
# Faithful to Sun & Tu (2024, Pharmaceutical Statistics 24:e2444),
# Figure 1 (flowchart) + section 2.3.2 (dose-finding algorithm). This
# is a STRICT EXTENSION of BOIN-12 (fun_boin_decision.R): the RDS/utility
# scoring, the toxicity boundaries lambda_e/lambda_d, and the safety /
# efficacy elimination tests are REUSED UNCHANGED from the BOIN-12 files.
# PKBOIN-12 changes only two things during dose-finding:
#   (1) the admissible dose set can expand its LOWER end when the current
#       dose has adequate PK exposure (r_hat_d > zeta1);
#   (2) an extra PK elimination criterion removes low-exposure doses.
#
# Notation map (paper -> this codebase, already established in BOIN-12):
#   lambda_1 (paper) = lambda_e (escalation boundary)
#   lambda_2 (paper) = lambda_d (de-escalation boundary)
#   N* = 6 sample-size cutoff ; the ">=9 & next dose unused" fast-escalate
#   rule is identical to BOIN-12's.
#
# SLOT ENCODING and RDS machinery: identical to fun_boin_decision.R
# (slot1=eff&notox u1, slot2=eff&tox u2, slot3=noeff&notox u3,
#  slot4=noeff&tox u4). We call boin_rds() / boin_utility_benchmark() /
# boin_admissible_set() / boin_elimination() directly -- no duplication,
# so PKBOIN-12 can never silently diverge from BOIN-12's tox/eff/utility
# logic. Only the PK-specific pieces live here.
#
# Depends (intra-family only): fun_boin_decision.R (boin_rds,
# boin_utility_benchmark, boin_admissible_set, boin_elimination),
# fun_pkboin_pk.R (pkboin_zeta1, pkboin_pk_flag, pkboin_d_pk_min).
# Auto-sourced; no functions/stein/* dependency.
# =====================================================================

# ---- expanded admissible set (paper Fig 1 / sec 2.3.2) --------------
# Returns the candidate dose set A for the next cohort.
#   j          : current dose
#   phat_tox_j : observed tox rate at j
#   r_hat_j    : observed mean PK at j (NA if none)
#   lambda_e   : escalation boundary  (paper lambda_1)
#   lambda_d   : de-escalation bound  (paper lambda_2)
#   n_dose     : total doses D
#   n_cur      : patients at current dose j
#   higher_used: has dose j+1 ever been tried?
#   N_star     : sample-size cutoff (paper N* = 6)
#   zeta1      : PK cutoff
#   d_pk_min   : lowest dose with r_hat > zeta1 (NA if none) --
#                pass pkboin_d_pk_min(obs_pk, zeta1).
#
# Logic:
#   * If r_hat_j <= zeta1 (or r_hat_j / d_pk_min is NA), PK gives no
#     extra exposure information at the current dose -> the admissible
#     set is EXACTLY BOIN-12's (delegate to boin_admissible_set), i.e.
#     Step 3 == Step 2 of BOIN-12 (paper: "the dosing options in Step 3
#     are the same as those in Step 2 of BOIN12").
#   * If r_hat_j > zeta1 (adequate PK), the lower end of the set is
#     lowered from (j-1) to d_star = min(j-1, d_pk_min), following
#     paper Step 4:
#        (a) phat >= lambda_d            -> A = {d_star, ..., j-1}
#        (b) phat <= lambda_e & n>=9
#              & j+1 unused & j<n_dose   -> A = {j+1}   (fast escalate)
#        (c) lambda_e < phat < lambda_d,
#              n>=N*                     -> A = {d_star, ..., j}
#              n< N*                     -> A = {d_star, ..., j+1}
#        (d) phat <= lambda_e            -> A = {d_star, ..., j+1}
#     (b) is BOIN-12's fast-escalation branch, unchanged and taking
#     priority over (d) exactly as in fun_boin_decision.R. When d_star
#     == j-1 (d_pk_min absent or >= j-1) the expanded set collapses to
#     BOIN-12's own set, so this is a genuine superset rule.
pkboin_admissible_set <- function(j, phat_tox_j, r_hat_j,
                                   lambda_e, lambda_d, n_dose,
                                   n_cur = 0L, higher_used = TRUE,
                                   N_star = 6L, zeta1 = NULL,
                                   d_pk_min = NA_integer_) {
  pk_adequate <- !is.null(zeta1) && !is.na(r_hat_j) && (r_hat_j > zeta1)

  if (!pk_adequate) {
    # identical to BOIN-12
    return(boin_admissible_set(j, phat_tox_j, lambda_e, lambda_d, n_dose,
                               n_cur = n_cur, higher_used = higher_used,
                               N_star = N_star))
  }

  # adequate PK: lower the floor to d_star = min(j-1, d_pk_min)
  d_star <- if (is.na(d_pk_min)) (j - 1L) else min(j - 1L, d_pk_min)

  if (phat_tox_j >= lambda_d) {
    cand <- seq.int(d_star, j - 1L)                     # (a)
  } else if (phat_tox_j <= lambda_e && n_cur >= 9L &&
             !higher_used && (j + 1L) <= n_dose) {
    cand <- (j + 1L)                                    # (b) fast escalate
  } else if (phat_tox_j > lambda_e && phat_tox_j < lambda_d &&
             n_cur >= N_star) {
    cand <- seq.int(d_star, j)                          # (c) n>=N*
  } else {
    cand <- seq.int(d_star, j + 1L)                     # (c) n<N* or (d)
  }
  cand <- cand[cand >= 1L & cand <= n_dose]
  sort(unique(as.integer(cand)))
}

# ---- PK elimination over the full dose ladder (paper sec 2.3.2) -----
# obs_pk    : observed mean PK per dose (length D; NA if untried)
# sigma_pk  : per-dose PK SD used in the posterior (length D; NA if
#             untried / not estimable) -- sample SD on data, CV*r_d in
#             the simulator.
# n_by_dose : n per dose (length D)
# r_P, C_P  : target PK value + PK elimination cutoff
# n_pk_min  : n guard (paper 6)
# Returns list(pk_elim, terminate):
#   pk_elim   : logical length-D; TRUE where the dose is removed for
#               inefficacious PK. Per paper: if dose d (2 <= d < D) is
#               flagged, "eliminate the lowest uneliminated dose level
#               among {1,...,d-1}" -- i.e. the flag prunes ONE low dose
#               from the bottom of the still-active ladder. We implement
#               this cumulatively: each flagged interior dose removes the
#               current lowest active dose. For d = 1 there is no lower
#               dose to prune, so the PK flag does not remove any dose.
#   terminate : TRUE if the TOP dose d = D is flagged -> all doses
#               ineffective, trial terminates (paper).
# This function performs ONLY the PK-driven removals; tox/eff cascade
# elimination stays in boin_elimination() and is combined by the caller.
pkboin_pk_elimination <- function(obs_pk, sigma_pk, n_by_dose,
                                  r_P, C_P, n_pk_min = 6L) {
  D <- length(obs_pk)
  flag <- vapply(seq_len(D), function(d) {
    pkboin_pk_flag(obs_pk[d], sigma_pk[d], n_by_dose[d], r_P, C_P, n_pk_min)
  }, logical(1))

  pk_elim   <- rep(FALSE, D)
  terminate <- FALSE

  # top-dose flag terminates the whole trial
  if (D >= 1L && isTRUE(flag[D])) {
    terminate <- TRUE
    pk_elim[] <- TRUE
    return(list(pk_elim = pk_elim, terminate = terminate, flag = flag))
  }

  # interior flags each prune the lowest still-active lower dose.
  # A d=1 flag has no lower dose to remove under the paper rule.
  if (D >= 3L) {
    for (d in seq.int(2L, D - 1L)) {
      if (!isTRUE(flag[d])) next
      lower_active <- which(!pk_elim & seq_len(D) <= (d - 1L))
      if (length(lower_active) > 0) {
        pk_elim[min(lower_active)] <- TRUE
      }
    }
  }
  list(pk_elim = pk_elim, terminate = terminate, flag = flag)
}

# ---- recommend next dose (PKBOIN-12) --------------------------------
# Mirrors boin_next_dose()'s contract but adds PK. Arguments:
#   j          : current dose
#   obs        : data.frame(dose, n, n1..n4) for tried doses
#   obs_pk     : observed mean PK per dose (length n_dose; NA untried)
#   boundaries : list(lambda_e, lambda_d)
#   u          : SLOT utility c(u1..u4)
#   n_dose     : D
#   design     : list(phi_T, phi_E) for the RDS benchmark
#   pk_design  : list(zeta1) (PK cutoff)  -- if NULL, this degrades
#                exactly to boin_next_dose() (no PK expansion).
# Returns list(admissible, scores, next_dose, d_pk_min, d_star,
#              pk_adequate) -- superset of boin_next_dose()'s fields, so
#              existing callers reading $admissible/$scores/$next_dose
#              keep working. Scoring is BOIN-12 RDS, UNCHANGED (RDS does
#              not depend on PK); ties -> lower dose (which.max first).
pkboin_next_dose <- function(j, obs, obs_pk, boundaries, u, n_dose,
                             design = NULL, pk_design = NULL) {
  lambda_e <- boundaries$lambda_e
  lambda_d <- boundaries$lambda_d
  pT <- if (!is.null(design)) design$phi_T else 0.35
  qE <- if (!is.null(design)) design$phi_E else 0.25
  u_b <- boin_utility_benchmark(u, pT, qE)
  zeta1 <- if (!is.null(pk_design)) pk_design$zeta1 else NULL

  cur   <- obs[obs$dose == j, , drop = FALSE]
  n_cur <- if (nrow(cur) == 1) cur$n else 0
  tox_cur    <- if (nrow(cur) == 1) cur$n2 + cur$n4 else 0
  phat_tox_j <- if (n_cur > 0) tox_cur / n_cur else 0

  hi <- obs[obs$dose == (j + 1), , drop = FALSE]
  higher_used <- (nrow(hi) == 1 && hi$n > 0)

  r_hat_j  <- if (length(obs_pk) >= j) obs_pk[j] else NA_real_
  d_pk_min <- if (!is.null(zeta1)) pkboin_d_pk_min(obs_pk, zeta1) else NA_integer_
  pk_adequate <- !is.null(zeta1) && !is.na(r_hat_j) && (r_hat_j > zeta1)
  d_star <- if (pk_adequate) {
    if (is.na(d_pk_min)) (j - 1L) else min(j - 1L, d_pk_min)
  } else (j - 1L)

  A <- pkboin_admissible_set(j, phat_tox_j, r_hat_j, lambda_e, lambda_d,
                             n_dose, n_cur = n_cur, higher_used = higher_used,
                             zeta1 = zeta1, d_pk_min = d_pk_min)

  score <- vapply(A, function(d) {
    row <- obs[obs$dose == d, , drop = FALSE]
    if (nrow(row) == 1 && row$n > 0) {
      boin_rds(row$n1, row$n2, row$n3, row$n4, u, pT, qE, u_b = u_b)
    } else {
      boin_rds(0, 0, 0, 0, u, pT, qE, u_b = u_b)  # untried: prior RDS
    }
  }, numeric(1))

  best <- A[which.max(score)]
  list(admissible = A, scores = setNames(score, A), next_dose = best,
       d_pk_min = d_pk_min, d_star = as.integer(d_star),
       pk_adequate = pk_adequate)
}
