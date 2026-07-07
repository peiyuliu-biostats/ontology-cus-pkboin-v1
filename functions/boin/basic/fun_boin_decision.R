# =====================================================================
# BOIN-12 interim dose-finding decision rules (pure functions)
# ---------------------------------------------------------------------
# Faithful to Lin, Zhou, Yan, Li & Yuan (2020, JCO PO 4:1393-1402),
# Table 2 (decision rule) + Fig 1 (flowchart) + the quasi-beta-binomial
# / rank-based desirability score (RDS) machinery. This replaces the
# earlier Dirichlet posterior-mean utility score, which was NOT the
# paper's mechanism.
#
# SLOT ENCODING (canonical; see initial_boin_rv.R). Physical count
# slots and the utility vector u = c(u1,u2,u3,u4) are in SLOT order:
#   slot1 = efficacy & no-toxicity   (u1, best;  paper O1)
#   slot2 = efficacy & toxicity      (u2;        paper O3)
#   slot3 = no-efficacy & no-toxicity(u3;        paper O2)
#   slot4 = no-efficacy & toxicity   (u4, worst; paper O4)
# Toxicity marginal count = n2+n4 ; efficacy marginal count = n1+n2.
#
# The interim cohort-allocation mechanism here is deliberately SEPARATE
# from final OBD selection (fun_boin_obd.R); the two can recommend
# different doses on the same data and must stay visible as distinct
# steps in the app.
#
# --------------- quasi-beta-binomial / RDS (paper sec "Methods") -----
# For a dose d with joint counts (n1,n2,n3,n4), n_d = sum:
#   standardized desirability     u_d      = EU_d / 100
#     where EU_d = sum(u_k * n_k) / n_d  (mean utility, 0..100 scale)
#   quasi-binomial event count    x_d      = sum(u_k * n_k) / 100
#                                          = u_d * n_d
#   posterior (Beta(1,1) prior)   u_d | .  ~ Beta(1 + x_d, 1 + n_d - x_d)
#   utility benchmark             u_b      = u_bar + (100 - u_bar)/2
#     where (paper) u_bar = u1*(1-pT)*qE + u3_paper*(1-pT)*(1-qE)
#                          + u2_paper*pT*qE  , all on the 0..100 scale.
#     Mapping paper-O utilities to our SLOTS (u2_paper=slot3=u[3],
#     u3_paper=slot2=u[2]); u4_paper=slot4 has coefficient 0 and drops.
#   RDS  = Pr(u_d > u_b/100 | n_d, x_d)  (higher = more desirable).
# The dose with the higher RDS is preferred; RDS replaces the old
# posterior-mean utility as the score returned by boin_next_dose().
# =====================================================================

# ---- marginal elimination tests (Beta(1,1) prior) -------------------

# posterior Pr(tox rate > phi_T | data); tox count = n2+n4
boin_prob_tox_above <- function(n_tox, n, phi_T) {
  pbeta(phi_T, 1 + n_tox, 1 + n - n_tox, lower.tail = FALSE)
}

# posterior Pr(eff rate <= phi_E | data); eff count = n1+n2
boin_prob_eff_below <- function(n_eff, n, phi_E) {
  pbeta(phi_E, 1 + n_eff, 1 + n - n_eff, lower.tail = TRUE)
}

# ---- elimination status per dose, WITH cascade toxicity rule --------
# obs: data.frame(dose, n, n1, n2, n3, n4). Returns per-dose flags plus
# a cascaded `eliminated`:
#   * safety (paper): if Pr(p_d > phi_T | .) > CT then dose d AND every
#     dose above d are eliminated (monotone toxicity in dose). This is
#     the cascade that was missing before.
#   * efficacy/futility: if Pr(q_d < phi_E | .) > CE then only dose d is
#     eliminated (efficacy is not monotone in dose, so no cascade).
# Doses with n = 0 (untried) yield NA tests here and are treated as
# not-yet-eliminable (elim flags FALSE); a tox cascade from a lower
# tried dose can still eliminate an untried higher dose.
boin_elimination <- function(obs, phi_T, phi_E, CT, CE) {
  obs <- obs[order(obs$dose), , drop = FALSE]
  n_tox <- obs$n2 + obs$n4
  n_eff <- obs$n1 + obs$n2

  has_data <- obs$n > 0
  elim_tox_self <- rep(FALSE, nrow(obs))
  elim_eff      <- rep(FALSE, nrow(obs))
  elim_tox_self[has_data] <-
    boin_prob_tox_above(n_tox[has_data], obs$n[has_data], phi_T) > CT
  elim_eff[has_data] <-
    boin_prob_eff_below(n_eff[has_data], obs$n[has_data], phi_E) > CE

  # cascade: the lowest dose flagged too-toxic eliminates it + all above
  elim_tox <- elim_tox_self
  hit <- which(elim_tox_self)
  if (length(hit) > 0) {
    lowest_tox <- min(obs$dose[hit])
    elim_tox <- obs$dose >= lowest_tox
  }

  data.frame(
    dose       = obs$dose,
    elim_tox   = elim_tox,
    elim_eff   = elim_eff,
    eliminated = elim_tox | elim_eff
  )
}

# ---- quasi-beta-binomial RDS for one dose ---------------------------
# Returns Pr(u_d > u_b/100 | n_d, x_d) in [0,1]. u is the SLOT utility
# vector c(u1,u2,u3,u4) on the 0..100 scale; pT = phi_T, qE = phi_E.
# For an untried dose (n = 0) the posterior is the Beta(1,1) prior and
# RDS = Pr(Uniform(0,1) > u_b/100) = 1 - u_b/100 (paper's prior-only
# desirability), so untried admissible doses are comparable to tried
# ones on the same RDS scale.
boin_utility_benchmark <- function(u, pT, qE) {
  # paper u_bar = u_{O1}(1-pT)qE + u_{O2}(1-pT)(1-qE) + u_{O3} pT qE
  # SLOT mapping: O1=u[1], O2=u[3], O3=u[2]
  u_bar <- u[1] * (1 - pT) * qE +
           u[3] * (1 - pT) * (1 - qE) +
           u[2] * pT * qE
  u_bar + (100 - u_bar) / 2
}

boin_rds <- function(n1, n2, n3, n4, u, pT, qE, u_b = NULL) {
  n <- n1 + n2 + n3 + n4
  if (is.null(u_b)) u_b <- boin_utility_benchmark(u, pT, qE)
  ub_std <- u_b / 100
  x_d <- sum(u * c(n1, n2, n3, n4)) / 100          # quasi-event count
  # posterior u_d | . ~ Beta(1 + x_d, 1 + n - x_d)
  pbeta(ub_std, 1 + x_d, 1 + n - x_d, lower.tail = FALSE)
}

# ---- backward-compatible posterior-mean utility (still used by
#      fun_boin_obd.R final-OBD scoring; NOT used for interim RDS) ----
# u = c(u1,u2,u3,u4); Dirichlet(1,1,1,1) posterior mean utility.
boin_utility_posterior_mean <- function(n1, n2, n3, n4, u) {
  n <- n1 + n2 + n3 + n4
  pi_hat <- (1 + c(n1, n2, n3, n4)) / (4 + n)
  sum(u * pi_hat)
}

# ---- local admissible set (paper Table 2 / Fig 1) -------------------
# Toxicity guardrail + N* branching + n>=9 fast-escalation.
#   j          : current dose
#   phat_tox_j : observed tox rate at j
#   lambda_e   : escalation boundary  (paper lambda_1)
#   lambda_d   : de-escalation bound  (paper lambda_2)
#   n_dose     : total doses
#   n_cur      : patients at current dose j (for the >=9 rule & N*)
#   higher_used: has dose j+1 ever been tried? (for the >=9 rule)
#   N_star     : sample-size cutoff (paper N* = 6)
# Rules:
#   (a) phat >= lambda_d               -> {j-1}                (de-escalate)
#   (b) phat <= lambda_e & n_cur >= 9
#         & j+1 not yet used & j<n_dose -> {j+1}   (fast escalation, Table 2/Fig1)
#   (c) lambda_e < phat < lambda_d
#         & n_cur >= N_star            -> {j-1, j}
#   (d) otherwise                      -> {j-1, j, j+1}
# Defaults reproduce a pure-guardrail call if the extra args are omitted
# (n_cur = 0 disables the >=9 branch; N_star large disables (c)).
boin_admissible_set <- function(j, phat_tox_j, lambda_e, lambda_d, n_dose,
                                 n_cur = 0L, higher_used = TRUE, N_star = 6L) {
  if (phat_tox_j >= lambda_d) {
    cand <- c(j - 1)
  } else if (phat_tox_j <= lambda_e && n_cur >= 9L &&
             !higher_used && (j + 1) <= n_dose) {
    cand <- c(j + 1)
  } else if (phat_tox_j > lambda_e && phat_tox_j < lambda_d &&
             n_cur >= N_star) {
    cand <- c(j - 1, j)
  } else {
    cand <- c(j - 1, j, j + 1)
  }
  cand <- cand[cand >= 1 & cand <= n_dose]
  sort(unique(cand))
}

# ---- recommend next dose (RDS-based) --------------------------------
# j: current dose; obs: data.frame(dose, n, n1..n4) for doses tried so
# far (untried doses may be absent or n = 0); boundaries: list with
# lambda_e, lambda_d; u: SLOT utility c(u1..u4); design: list with
# phi_T, phi_E (for the RDS benchmark). n_dose: total doses.
# Returns list(admissible, scores, next_dose) -- SAME contract as
# before; `scores` are now RDS values (higher = better), still argmax-
# selected, so every existing caller (simulate/replay/conduct) works
# unchanged. Ties -> lower dose (which.max first-match), as before.
boin_next_dose <- function(j, obs, boundaries, u, n_dose, design = NULL) {
  lambda_e <- boundaries$lambda_e
  lambda_d <- boundaries$lambda_d
  pT <- if (!is.null(design)) design$phi_T else 0.35
  qE <- if (!is.null(design)) design$phi_E else 0.25
  u_b <- boin_utility_benchmark(u, pT, qE)

  cur   <- obs[obs$dose == j, , drop = FALSE]
  n_cur <- if (nrow(cur) == 1) cur$n else 0
  tox_cur    <- if (nrow(cur) == 1) cur$n2 + cur$n4 else 0
  phat_tox_j <- if (n_cur > 0) tox_cur / n_cur else 0

  hi <- obs[obs$dose == (j + 1), , drop = FALSE]
  higher_used <- (nrow(hi) == 1 && hi$n > 0)

  A <- boin_admissible_set(j, phat_tox_j, lambda_e, lambda_d, n_dose,
                           n_cur = n_cur, higher_used = higher_used)

  score <- vapply(A, function(d) {
    row <- obs[obs$dose == d, , drop = FALSE]
    if (nrow(row) == 1 && row$n > 0) {
      boin_rds(row$n1, row$n2, row$n3, row$n4, u, pT, qE, u_b = u_b)
    } else {
      boin_rds(0, 0, 0, 0, u, pT, qE, u_b = u_b)  # untried: prior RDS
    }
  }, numeric(1))

  best <- A[which.max(score)]
  list(admissible = A, scores = setNames(score, A), next_dose = best)
}

# ---- RDS look-up table (paper Table 3 view) -------------------------
# Reproduces the BOIN-12 rank-based-desirability lookup for display.
# The paper tabulates RDS over (n, n_tox, n_eff). That marginal indexing
# is EXACT only when u2 + u3 = 100 (the case in which the expected
# utility collapses to EU = u2(1-p) + u3 q, a function of the marginals
# n_tox, n_eff alone). In general (u2 + u3 != 100) RDS depends on the
# full joint split (n1..n4), not just the marginals, so a marginal table
# is not well defined; the caller is told to fall back to the formula.
#
# Returns a data.frame with columns n, n_tox, n_eff, x, RDS for every
# reachable (n_tox, n_eff) with n_tox + n_eff <= 2n (feasible joint
# split existing), for each n in `ns`. Rows are the paper's Table-3
# layout. `exact` (attribute) is FALSE when u2+u3 != 100.
#
# When u2+u3 == 100, the joint split is irrelevant to EU, so we use the
# canonical split maximising overlap: n_both = max(0, n_tox+n_eff-n),
# n2 = n_both, n1 = n_eff - n_both, n4 = n_tox - n_both, n3 = rest.
boin_rds_table <- function(u, pT, qE, ns) {
  u_b <- boin_utility_benchmark(u, pT, qE)
  exact <- isTRUE(all.equal(u[2] + u[3], 100))
  rows <- list()
  k <- 0L
  for (n in ns) {
    for (n_tox in 0:n) {
      for (n_eff in 0:n) {
        # feasible joint split must exist: overlap n_both in
        # [max(0, n_tox+n_eff-n), min(n_tox, n_eff)]
        lo <- max(0L, n_tox + n_eff - n)
        hi <- min(n_tox, n_eff)
        if (lo > hi) next
        n_both <- lo                     # canonical (max non-overlap)
        n2 <- n_both                     # eff & tox
        n1 <- n_eff - n_both             # eff & no-tox
        n4 <- n_tox - n_both             # no-eff & tox
        n3 <- n - n1 - n2 - n4           # no-eff & no-tox
        x <- sum(u * c(n1, n2, n3, n4)) / 100
        rds <- pbeta(u_b / 100, 1 + x, 1 + n - x, lower.tail = FALSE)
        k <- k + 1L
        rows[[k]] <- data.frame(n = n, n_tox = n_tox, n_eff = n_eff,
                                x = round(x, 3), RDS = round(rds, 4))
      }
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "exact") <- exact
  attr(out, "u_b") <- u_b
  out
}
