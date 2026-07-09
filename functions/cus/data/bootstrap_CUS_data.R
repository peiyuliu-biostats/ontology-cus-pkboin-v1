# this is for upload data only
bootstrap_CUS_data <- function(all_rv, bootsize = 5000)
  {
    applicable <- (all_rv$overall_setting$simu_or_not == 2) &       # Upload data 
      (nrow(all_rv$ER_data_list$ER_rawdt) > 0)                     # Have uploaded raw data 

    if(applicable == FALSE) {
      return(list(warning = T))
    }
    
    if(Sys.info()[["sysname"]] == "Windows") {    # not use parallel
      bootsize = 500
    }
    
    # Preparation #### 
    raw_dt <- all_rv$ER_data_list$ER_rawdt
    
    PKmin <- min(raw_dt$PK)
    PKmax <- max(raw_dt$PK)
    
    rng <- PKmax - PKmin
    pow <- ifelse(rng == 0, 0, floor(log10(rng)))
    step <- 10^(pow - 2)
    
    # PK for CUS  
    PK_gen <- seq(from = floor(PKmin / step) * step, to = ceiling(PKmax / step) * step, 
                  by = step)
    
    eff_num <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    
    ## Weight #### 
    weight = NULL 
    if(!is.na(eff_num) & eff_num > 0) {
      weight = c(weight, all_rv$eff_endpoint_setting$eff_weight[1:eff_num])
    }
    if(!is.na(safe_num) & safe_num > 0) {
      weight = c(weight, all_rv$safe_endpoint_setting$safe_weight[1:safe_num])
    }
    per_weight <- weight/sum(weight)
    
    ## Utility Function Parameters (Endpoint Rate -> Score) #### 
    
    ### Sigmoid Utility 
    if(all_rv$overall_setting$utility_type == 1) {    # Sigmoid utility 
      if(all_rv$overall_setting$individual_Sshape_utility == 2) {  # unified Sigmoid utility 
        eff_beta = rep(all_rv$utility_Sshape_setting$eff_beta, eff_num)
        eff_shape = rep(all_rv$utility_Sshape_setting$eff_shape, eff_num)
        safe_beta = rep(all_rv$utility_Sshape_setting$safe_beta, safe_num)
        safe_shape = rep(all_rv$utility_Sshape_setting$safe_shape, safe_num)
      } else {                                              # unique Sigmoid utility 
        eff_beta = all_rv$inidividual_utility_Sshape_setting$eff_beta
        eff_shape = all_rv$inidividual_utility_Sshape_setting$eff_shape 
        safe_beta = all_rv$inidividual_utility_Sshape_setting$safe_beta
        safe_shape = all_rv$inidividual_utility_Sshape_setting$safe_shape
      }
    } else if(all_rv$overall_setting$utility_type == 2) {     # Stepwise utility 
      if(all_rv$overall_setting$individual_stepwise_utility == 2) {   # unified Stepwise utility 
        eff_knot_num = rep(all_rv$utility_stepwise_setting$eff_knot_num, eff_num)
        eff_measure = replicate(eff_num, all_rv$utility_stepwise_setting$eff_measure, simplify = FALSE)
        eff_score = replicate(eff_num, all_rv$utility_stepwise_setting$eff_score, simplify = FALSE)
        safe_knot_num = rep(all_rv$utility_stepwise_setting$safe_knot_num, safe_num)
        safe_measure = replicate(safe_num, all_rv$utility_stepwise_setting$safe_measure, simplify = FALSE)
        safe_score = replicate(safe_num, all_rv$utility_stepwise_setting$safe_score, simplify = FALSE)
      } else {                                                 # unique Stepwise utility 
        eff_knot_num = all_rv$individual_utility_stepwise_setting$eff_knot_num
        eff_measure = all_rv$individual_utility_stepwise_setting$eff_measure
        eff_score = all_rv$individual_utility_stepwise_setting$eff_score
        safe_knot_num = all_rv$individual_utility_stepwise_setting$safe_knot_num
        safe_measure = all_rv$individual_utility_stepwise_setting$safe_measure
        safe_score = all_rv$individual_utility_stepwise_setting$safe_score
      }
    }
    
    # Bootstrap #### 
    start <- Sys.time()
    bootsample <- 
      sapply(1:bootsize, FUN = function(x) {set.seed(x); 
        sample(1:nrow(raw_dt), size = nrow(raw_dt), replace = T)})   # nrow * bootsize
    
    # Rate Calculation (Exposure -> Rate) #### 
    boot_prob_eff_list <- list()
    boot_prob_safe_list <- list()
    boot_score_list <- list()

    ## Rate Calculation for Efficacy #### 
    if(!is.na(eff_num) & eff_num > 0) {
      for(i in 1:eff_num) {
        # per-endpoint model (B scheme): type picks the group's shared regression; fall back to scalar
        eff_mdl <- {
          tv <- all_rv$overall_setting$eff_type_vec
          if(!is.null(tv) && length(tv) >= i && !is.na(tv[i])) {
            if(tv[i] == "cont") all_rv$overall_setting$eff_cont_model else all_rv$overall_setting$eff_bin_model
          } else all_rv$overall_setting$eff_PK_model
        }
        # PK -> Endpoint Probability 
        if(eff_mdl == 1) {   # Logistic 
          boot_coef_eff <- sapply(1:bootsize, function(k) {
            fun_logistic_fit_fast(y = raw_dt[[paste0("EFF", i)]][bootsample[,k]],
                                  x = raw_dt$PK[bootsample[,k]])
          })  # 2 * bootsize
          
          boot_eff_inter <- boot_coef_eff[1, ]
          boot_eff_slope <-  boot_coef_eff[2, ]
          
          # Use PK_gen here ! 
          boot_prob_eff <- sapply(1:bootsize, FUN = function(x) 
            1/(1 + exp(pmin(pmax(-boot_eff_inter[x] - boot_eff_slope[x] * PK_gen, -709), 709))))
          # length(PK_gen) * bootsize 
        } else if(eff_mdl == 2) {    # Emax
          if(Sys.info()[["sysname"]] == "Windows") {
            boot_coef_eff <- lapply(1:bootsize, FUN = function(k) {
              optim(par = c(-1, 1, 0), fn = negloglik_emax, 
                    x = raw_dt$PK[bootsample[,k]], y = raw_dt[[paste0("EFF", i)]][bootsample[,k]],
                    method  = "BFGS", hessian = TRUE)$par}) %>% 
              do.call(rbind, .)   # bootsize * 3
          } else {
            boot_coef_eff <- mclapply(1:bootsize, FUN = function(k) {
              optim(par = c(-1, 1, 0), fn = negloglik_emax, 
                    x = raw_dt$PK[bootsample[,k]], y = raw_dt[[paste0("EFF", i)]][bootsample[,k]],
                    method  = "BFGS", hessian = TRUE)$par}, mc.cores = 8) %>% 
              do.call(rbind, .)   # bootsize * 3
          }
          lp_matrix <- (boot_coef_eff[,2] %*% matrix(PK_gen, nrow = 1))/(outer(exp(boot_coef_eff[,3]), PK_gen, "+")) + 
            boot_coef_eff[,1]
          boot_prob_eff <- t(1/(1 + exp(-lp_matrix)))  # length(PK_gen)  * bootsize 
        } else if(eff_mdl %in% c(3, 4, 5)) {   # continuous: linear / log-linear / exponential
          # each resample: refit continuous coef on resampled (PK, Y), then map
          # Yhat on PK_gen through the empirical CDF of the resampled Y (rebuilt per resample).
          # matches the point estimate in initial_PK_data.R (fit_continuous_coef + continuous_ecdf_map).
          mdl <- as.character(eff_mdl)
          # optional user response bounds (fixed inputs, not resampled): read once.
          lb <- all_rv$eff_endpoint_setting$eff_resp_lb[i]
          ub <- all_rv$eff_endpoint_setting$eff_resp_ub[i]
          boot_prob_eff <- sapply(1:bootsize, FUN = function(k) {
            yk <- raw_dt[[paste0("EFF", i)]][bootsample[,k]]
            pkk <- raw_dt$PK[bootsample[,k]]
            cf <- fit_continuous_coef(pkk, yk)
            ab <- switch(mdl,
                         "3" = c(cf$lin_a, cf$lin_b),
                         "4" = c(cf$log_a, cf$log_b),
                         "5" = c(cf$exp_a, cf$exp_b))
            Yhat <- switch(mdl,
                           "3" = ab[1] + ab[2] * PK_gen,
                           "4" = ab[1] + ab[2] * log(PK_gen),
                           "5" = exp(pmin(pmax(ab[1] + ab[2] * PK_gen, -709), 709)))
            if (!is.null(lb) && !is.na(lb)) Yhat <- pmax(Yhat, lb)
            if (!is.null(ub) && !is.na(ub)) Yhat <- pmin(Yhat, ub)
            pmin(pmax(continuous_ecdf_map(yk, Yhat, lb, ub), 1e-6), 1 - 1e-6)
          })  # length(PK_gen) * bootsize
        } 
        
        # Endpoint -> Score 
        if(all_rv$overall_setting$utility_type == 1) {         # Sigmoid
          boot_score_list[[i]] <- 1/(1 + ((boot_prob_eff * (1-eff_shape[i]))/(eff_shape[i]*(1-boot_prob_eff)))^(-eff_beta[i]))
        } else if(all_rv$overall_setting$utility_type == 2) {           # Stepwise 
          # eff_knot_num <- all_rv$utility_stepwise_setting$eff_knot_num
          stepwise_param <- data.frame(measurement = eff_measure[[i]][1:eff_knot_num[i]], 
                                       score = eff_score[[i]][1:eff_knot_num[i]]) %>% 
            na.omit() %>%
            arrange(measurement) %>%
            distinct(measurement, .keep_all = TRUE)
          measurement <- stepwise_param$measurement
          score <- stepwise_param$score

          f_step <- stepfun(measurement, c(score, max(1, score)), right = TRUE)
          boot_score_list[[i]] <- sapply(1:bootsize, FUN = function(x) f_step(boot_prob_eff[,x]))
        }
        # length(PK_gen) * bootsize 
      } 
    }
    
    ## Rate Calculation for Safety #### 
    if (!is.na(safe_num) & safe_num > 0) {
      for (i in 1:safe_num) {
        # per-endpoint model (B scheme): type picks the group's shared regression; fall back to scalar
        safe_mdl <- {
          tv <- all_rv$overall_setting$safe_type_vec
          if(!is.null(tv) && length(tv) >= i && !is.na(tv[i])) {
            if(tv[i] == "cont") all_rv$overall_setting$safe_cont_model else all_rv$overall_setting$safe_bin_model
          } else all_rv$overall_setting$safe_PK_model
        }
        # PK -> Endpoint Probability 
        if(safe_mdl == 1) {   # Logistic 
          boot_coef_safe <- sapply(1:bootsize, function(k) {
            fun_logistic_fit_fast(y = raw_dt[[paste0("SAFE", i)]][bootsample[,k]],
                                  x = raw_dt$PK[bootsample[,k]])
          })  # 2 * bootsize
          
          boot_safe_inter <- boot_coef_safe[1, ]
          boot_safe_slope <-  boot_coef_safe[2, ]
          
          # Use PK_gen here ! 
          boot_prob_safe <- sapply(1:bootsize, FUN = function(x) 
            1/(1 + exp(pmin(pmax(-boot_safe_inter[x] - boot_safe_slope[x] * PK_gen, -709), 709))))
          # length(PK_gen)  * bootsize 
        } else if(safe_mdl == 2) {    # Emax
          if(Sys.info()[["sysname"]] == "Windows") {
            boot_coef_safe <- lapply(1:bootsize, FUN = function(k) {
              optim(par = c(-1, 1, 0), fn = negloglik_emax, 
                    x = raw_dt$PK[bootsample[,k]], y = raw_dt[[paste0("SAFE", i)]][bootsample[,k]],
                    method  = "BFGS", hessian = TRUE)$par}) %>% 
              do.call(rbind, .)   # bootsize * 3
          } else {
            boot_coef_safe <- mclapply(1:bootsize, FUN = function(k) {
              optim(par = c(-1, 1, 0), fn = negloglik_emax, 
                    x = raw_dt$PK[bootsample[,k]], y = raw_dt[[paste0("SAFE", i)]][bootsample[,k]],
                    method  = "BFGS", hessian = TRUE)$par}, mc.cores = 8) %>% 
              do.call(rbind, .)   # bootsize * 3
          }
          lp_matrix <- (boot_coef_safe[,2] %*% matrix(PK_gen, nrow = 1))/(outer(exp(boot_coef_safe[,3]), PK_gen, "+")) + 
            boot_coef_safe[,1]
          boot_prob_safe <- t(1/(1 + exp(-lp_matrix)))  # length(PK_gen)  * bootsize 
        } else if(safe_mdl %in% c(3, 4, 5)) {   # continuous: linear / log-linear / exponential
          # each resample: refit continuous coef on resampled (PK, Y), then map
          # Yhat on PK_gen through the empirical CDF of the resampled Y (rebuilt per resample).
          # matches the point estimate in initial_PK_data.R (fit_continuous_coef + continuous_ecdf_map).
          mdl <- as.character(safe_mdl)
          # optional user response bounds (fixed inputs, not resampled): read once.
          lb <- all_rv$safe_endpoint_setting$safe_resp_lb[i]
          ub <- all_rv$safe_endpoint_setting$safe_resp_ub[i]
          boot_prob_safe <- sapply(1:bootsize, FUN = function(k) {
            yk <- raw_dt[[paste0("SAFE", i)]][bootsample[,k]]
            pkk <- raw_dt$PK[bootsample[,k]]
            cf <- fit_continuous_coef(pkk, yk)
            ab <- switch(mdl,
                         "3" = c(cf$lin_a, cf$lin_b),
                         "4" = c(cf$log_a, cf$log_b),
                         "5" = c(cf$exp_a, cf$exp_b))
            Yhat <- switch(mdl,
                           "3" = ab[1] + ab[2] * PK_gen,
                           "4" = ab[1] + ab[2] * log(PK_gen),
                           "5" = exp(pmin(pmax(ab[1] + ab[2] * PK_gen, -709), 709)))
            if (!is.null(lb) && !is.na(lb)) Yhat <- pmax(Yhat, lb)
            if (!is.null(ub) && !is.na(ub)) Yhat <- pmin(Yhat, ub)
            pmin(pmax(continuous_ecdf_map(yk, Yhat, lb, ub), 1e-6), 1 - 1e-6)
          })  # length(PK_gen) * bootsize
        }
        
        # Endpoint -> Score 
        if (all_rv$overall_setting$utility_type == 1) {   # Sigmoid
          boot_score_list[[eff_num + i]] <- 
            1 - 1/(1 + ((boot_prob_safe * (1-safe_shape[i]))/(safe_shape[i]*(1-boot_prob_safe)))^(-safe_beta[i]))
        } else if (all_rv$overall_setting$utility_type == 2) {   # Stepwise
          # safe_knot_num <- all_rv$utility_stepwise_setting$safe_knot_num
          stepwise_param <- data.frame(measurement = safe_measure[[i]][1:safe_knot_num[i]], 
                                       score = safe_score[[i]][1:safe_knot_num[i]]) %>% 
            na.omit() %>%
            arrange(measurement) %>%
            distinct(measurement, .keep_all = TRUE)
          measurement <- stepwise_param$measurement
          score <- stepwise_param$score
          f_step <- stepfun(measurement, c(score[1], score), right = TRUE)
          
          boot_score_list[[eff_num + i]] <- sapply(1:bootsize, FUN = function(x) f_step(boot_prob_safe[,x]))
        }
      }
    }
    
    # Bootstrap CUS #### 
    if(length(weight) > 0) {
      if(all_rv$overall_setting$cus_agg_type == 2) {   # linear weighted average: sum(w_i * s_i)
        boot_CUS <- Reduce(`+`, Map(function(M, w) M * w, boot_score_list, per_weight))     # length(PK_gen) * bootsize 
      } else {                                         # multiplicative (default): exp(sum(w_i * log(s_i)))
        boot_CUS <- Reduce(`+`, Map(function(M, w) log(M) * w, boot_score_list, per_weight)) %>% exp() # length(PK_gen) * bootsize 
      }
    }
    
    boot_CUS_UB <- apply(boot_CUS, MARGIN = 1, FUN = function(x) quantile(x, prob = 0.975))
    boot_CUS_LB <- apply(boot_CUS, MARGIN = 1, FUN = function(x) quantile(x, prob = 0.025))
    
    boot_PK_est <-
      PK_gen[sapply(1:bootsize, FUN = function(x)
      {
        oneCUS <- boot_CUS[,x]
        min(which(oneCUS == max(oneCUS, na.rm = T)))
      }
    )]
    boot_PK_est_LB <- quantile(boot_PK_est, prob = 0.025, na.rm = T)
    boot_PK_est_UB <- quantile(boot_PK_est, prob = 0.975, na.rm = T)

    end <- Sys.time()
    print(end - start)
    return(list(warning = FALSE, boot_CUS = boot_CUS, PK_gen = PK_gen,
                boot_CUS_LB = boot_CUS_LB, boot_CUS_UB = boot_CUS_UB,
                boot_PK_est_LB = boot_PK_est_LB, boot_PK_est_UB = boot_PK_est_UB))
}

