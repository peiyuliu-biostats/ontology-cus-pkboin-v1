fun_Emax_est <- function(data, PKname, EPname, theta_init = c(-1, 1, 1))
{
  # theta_init = c(E0, Emax, EC50)
  optim(
    par     = theta_init,
    fn      = negloglik_emax,
    x       = data[[PKname]],
    y       = data[[EPname]],
    method  = "BFGS",
    hessian = TRUE
  )
}

negloglik_emax <- function(theta, x, y) {
  E0    <- theta[1]        # baseline log-odds
  Emax  <- theta[2]        # max effect on log-odds
  EC50  <- exp(theta[3])   # keep EC50 > 0 by exponentiating
  lp <- E0 + Emax * x / (EC50 + x)
  p  <- 1 / (1 + exp(-lp))
  # Numerical safety
  p  <- pmin(pmax(p, 1e-8), 1 - 1e-8)
  -sum(dbinom(y, size = 1, prob = p, log = TRUE))
}