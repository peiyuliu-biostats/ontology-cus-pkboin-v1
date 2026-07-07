# =====================================================================
# STEIN isotonic regression + unimodal model averaging (pure functions)
# ---------------------------------------------------------------------
# Toxicity: monotone (non-decreasing) isotonic regression via PAVA.
# Efficacy: unimodal isotonic regression with AIC model averaging over
#           all J possible peak locations (Lin & Yin 2017).
# Uses stats::isoreg (base R) so no extra package dependency is added.
# =====================================================================

# weighted monotone non-decreasing isotonic fit via PAVA
# phat: observed proportions ; w: weights (sample sizes)
stein_pava_increasing <- function(phat, w) {
  # stats::isoreg is unweighted; expand-and-fit is stable for integer-ish
  # weights but to stay exact with weights we implement weighted PAVA.
  n <- length(phat)
  if (n == 0) return(numeric(0))
  y <- phat
  wt <- w
  # pool-adjacent-violators
  level_val <- y
  level_wt  <- wt
  idx <- as.list(seq_len(n))
  i <- 1
  vals <- y
  wts  <- wt
  # iterative pooling
  repeat {
    viol <- which(diff(vals) < 0)
    if (length(viol) == 0) break
    j <- viol[1]
    # pool blocks j and j+1
    new_w <- wts[j] + wts[j + 1]
    new_v <- (vals[j] * wts[j] + vals[j + 1] * wts[j + 1]) / new_w
    idx[[j]] <- c(idx[[j]], idx[[j + 1]])
    idx[[j + 1]] <- NULL
    vals[j] <- new_v
    wts[j]  <- new_w
    vals <- vals[-(j + 1)]
    wts  <- wts[-(j + 1)]
  }
  # expand pooled levels back to original positions
  out <- numeric(n)
  for (k in seq_along(idx)) out[idx[[k]]] <- vals[k]
  out
}

# unimodal isotonic fit with peak fixed at position k:
#   increasing on 1..k, decreasing on k..J
stein_unimodal_fit_at <- function(qhat, w, k) {
  n <- length(qhat)
  left  <- seq_len(k)
  right <- k:n
  fit_left  <- stein_pava_increasing(qhat[left], w[left])
  # decreasing = reverse, increasing-fit, reverse back
  fit_right <- rev(stein_pava_increasing(rev(qhat[right]), rev(w[right])))
  out <- numeric(n)
  out[left]  <- fit_left
  out[right] <- fit_right
  # peak position k is shared; average the two estimates there by weight
  # (both fits assign a value to k); use the more constrained max to keep
  # unimodality — take the peak as max of the two boundary values.
  out[k] <- max(fit_left[k], fit_right[1])
  out
}

# pseudo-likelihood of a fitted efficacy curve given counts
stein_pseudo_loglik <- function(qfit, y, n) {
  q <- pmin(pmax(qfit, 1e-8), 1 - 1e-8)
  sum(y * log(q) + (n - y) * log(1 - q))
}

# full unimodal model-averaged efficacy estimate
# yhat: efficacy events per dose ; n: sample size per dose
stein_efficacy_model_avg <- function(y, n) {
  J <- length(y)
  qhat <- ifelse(n > 0, y / n, 0)
  fits <- lapply(seq_len(J), function(k) stein_unimodal_fit_at(qhat, n, k))
  # AIC = -2 loglik + 2J ; J identical across peaks so weights reduce to
  # exp(loglik) normalized (equivalent to AIC weights).
  ll <- vapply(fits, function(f) stein_pseudo_loglik(f, y, n), numeric(1))
  ll <- ll - max(ll)                 # stabilize
  wts <- exp(ll)
  wts <- wts / sum(wts)
  qtilde <- Reduce(`+`, Map(function(f, wt) f * wt, fits, wts))
  list(qtilde = qtilde, weights = wts, fits = fits)
}
