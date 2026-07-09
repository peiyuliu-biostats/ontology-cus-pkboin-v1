# maps predicted Y to [0,1] via a linear-interpolated empirical CDF of observed Y.
# Yhat outside the observed range is clamped to the nearest boundary (0 or 1),
# per the agreed upload-mode rule. used by both the main curve and the bootstrap.
# optional lb/ub give truncated support (decision a): the observed Y used to build the
# ECDF is restricted to [lb, ub], so the mapping reflects a truncated response
# distribution rather than piling out-of-bound predictions at the untruncated edges.
# NA/NULL side = no truncation on that side. only the mapping baseline is affected;
# the CUS core is untouched.
continuous_ecdf_map <- function(Y_obs, Yhat, lb = NA_real_, ub = NA_real_) {
  Y_obs <- Y_obs[is.finite(Y_obs)]
  # truncated support: keep only observed Y within [lb, ub] before building the ECDF
  if (!is.null(lb) && !is.na(lb)) Y_obs <- Y_obs[Y_obs >= lb]
  if (!is.null(ub) && !is.na(ub)) Y_obs <- Y_obs[Y_obs <= ub]
  n <- length(Y_obs)
  if (n == 0) return(rep(0.5, length(Yhat)))           # no data in support -> neutral
  ys <- sort(Y_obs)
  if (ys[1] == ys[n]) return(rep(0.5, length(Yhat)))   # degenerate (all equal) -> neutral
  h <- (seq_len(n) - 1) / (n - 1)                       # heights 0 .. 1
  # collapse duplicate Y values to a single knot (keep the max height) so the
  # interpolation x is strictly increasing
  knot_x <- unique(ys)
  knot_y <- as.numeric(tapply(h, match(ys, knot_x), max))
  # linear interpolation between ECDF knots; rule = 2 clamps to 0/1 outside range
  m <- approx(x = knot_x, y = knot_y, xout = Yhat, rule = 2)$y
  m
}

# fits the three continuous ER regressions on (PK, Y) and returns their
# intercept/slope. invalid-domain rows are dropped (log-linear needs PK > 0;
# exponential needs Y > 0); a failed/degenerate fit returns 0 slope (flat ER).
fit_continuous_coef <- function(pk, y) {
  out <- list(lin_a = 0, lin_b = 0, log_a = 0, log_b = 0, exp_a = 0, exp_b = 0)
  safe_lm <- function(xx, yy) {
    ok <- is.finite(xx) & is.finite(yy)
    xx <- xx[ok]; yy <- yy[ok]
    if (length(xx) < 2 || length(unique(xx)) < 2) return(c(0, 0))
    co <- tryCatch(coef(lm(yy ~ xx)), error = function(e) c(0, 0))
    if (any(!is.finite(co))) co <- c(0, 0)
    c(co[1], co[2])
  }
  # linear: Y ~ PK
  c_lin <- safe_lm(pk, y);                          out$lin_a <- c_lin[1]; out$lin_b <- c_lin[2]
  # log-linear: Y ~ log(PK), needs PK > 0
  c_log <- safe_lm(ifelse(pk > 0, log(pk), NA), y); out$log_a <- c_log[1]; out$log_b <- c_log[2]
  # exponential: log(Y) ~ PK, needs Y > 0
  c_exp <- safe_lm(pk, ifelse(y > 0, log(y), NA));  out$exp_a <- c_exp[1]; out$exp_b <- c_exp[2]
  out
}

# TRUE if a vector is binary (only 0/1, ignoring NA) -> logistic/Emax applicable
is_binary_endpoint <- function(y) {
  u <- unique(y[is.finite(y)])
  length(u) > 0 && all(u %in% c(0, 1))
}
