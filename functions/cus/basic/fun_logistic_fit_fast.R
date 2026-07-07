fun_logistic_fit_fast <- function(y, x, max_iter = 20, tol = 1e-7) {
  n <- length(y)
  X <- cbind(1, x)
  X <- as.matrix(X)  
  
  beta <- rep(0, ncol(X))
  
  for (iter in 1:max_iter) {
    eta <- X %*% beta
    p <- 1 / (1 + exp(-eta))
    
    W <- p * (1 - p)
    W[W < 1e-8] <- 1e-8
    W <- as.numeric(W)  
    
    z <- eta + (y - p) / W
    
    WX <- X * W        
    XtWX <- t(X) %*% WX
    
    beta_new <- solve(XtWX, t(X) %*% (W * z))
    
    if (max(abs(beta_new - beta)) < tol) break
    beta <- beta_new
  }
  beta
}