# =====================================================================
# BOIN weighted isotonic regression (PAVA) -- pure function
# ---------------------------------------------------------------------
# Stage A5 helper. Weighted monotone non-decreasing isotonic fit via
# pool-adjacent-violators, used to smooth the marginal toxicity curve
# for final OBD selection (fun_boin_obd.R). Same algorithm as STEIN's
# fun_stein_pava.R::stein_pava_increasing, but duplicated (not shared)
# so functions/boin/ has zero source-order or edit dependency on
# functions/stein/ -- consistent with the isolation convention already
# used for fun_boin_boundaries.R (boin_lr_boundary vs stein_lr_boundary).
# =====================================================================

# phat: observed proportions ; w: weights (sample sizes)
boin_pava_increasing <- function(phat, w) {
  n <- length(phat)
  if (n == 0) return(numeric(0))
  vals <- phat
  wts  <- w
  idx <- as.list(seq_len(n))
  repeat {
    viol <- which(diff(vals) < 0)
    if (length(viol) == 0) break
    j <- viol[1]
    new_w <- wts[j] + wts[j + 1]
    new_v <- (vals[j] * wts[j] + vals[j + 1] * wts[j + 1]) / new_w
    idx[[j]] <- c(idx[[j]], idx[[j + 1]])
    idx[[j + 1]] <- NULL
    vals[j] <- new_v
    wts[j]  <- new_w
    vals <- vals[-(j + 1)]
    wts  <- wts[-(j + 1)]
  }
  out <- numeric(n)
  for (k in seq_along(idx)) out[idx[[k]]] <- vals[k]
  out
}
