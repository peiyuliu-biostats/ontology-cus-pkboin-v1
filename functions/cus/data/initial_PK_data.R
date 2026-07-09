# this is for simulated data 
initial_PK_data <- 
  function(PK_setting, endpoint_num_setting, eff_endpoint_setting, safe_endpoint_setting, 
           utility_Sshape_setting, inidividual_utility_Sshape_setting,
           utility_stepwise_setting, individual_utility_stepwise_setting,
           overall_setting, ER_data_list)
{
  # collects endpoints whose stepwise input had duplicate measurement values,
  # so the caller can surface a user-visible notification (see server.R)
  dup_warnings <- character(0)

  # use real data or simulated data 
  use_real <- (overall_setting$simu_or_not == 2) & (nrow(ER_data_list$ER_rawdt) > 0)
  
  # ER dataset 
  if(use_real == TRUE) {    # Upload 
    PK <- ER_data_list$ER_rawdt$PK
    PKmin <- min(PK)
    PKmax <- max(PK)
  } else {  # Simu 
    PKmin <- PK_setting$PK_min
    PKmax <- PK_setting$PK_max
  }
  
  ## Score Dataset 
  rng <- PKmax - PKmin
  pow <- ifelse(rng == 0, 0, floor(log10(rng)))
  step <- 10^(pow - 2)
  
  Score_dt <- 
    data.frame(PK = seq(from = floor(PKmin / step) * step, 
                        to = ceiling(PKmax / step) * step, 
                        by = step))
  ER_dt <- Score_dt
  
  eff_num <- coalesce(endpoint_num_setting$eff_num, 0)
  safe_num <- coalesce(endpoint_num_setting$safe_num, 0)
  
  # Weight #### 
  weight = NULL 
  if(!is.na(eff_num) & eff_num > 0) {
    weight = c(weight, eff_endpoint_setting$eff_weight[1:eff_num])
  }
  if(!is.na(safe_num) & safe_num > 0) {
    weight = c(weight, safe_endpoint_setting$safe_weight[1:safe_num])
  }
  per_weight <- weight/sum(weight)
  
  # Utility Function Parameters (Endpoint Rate -> Score) #### 
  
  ### Sigmoid Utility 
  if(overall_setting$utility_type == 1) {    # Sigmoid utility 
    if(overall_setting$individual_Sshape_utility == 2) {  # unified Sigmoid utility 
      eff_beta = rep(utility_Sshape_setting$eff_beta, eff_num)
      eff_shape = rep(utility_Sshape_setting$eff_shape, eff_num)
      safe_beta = rep(utility_Sshape_setting$safe_beta, safe_num)
      safe_shape = rep(utility_Sshape_setting$safe_shape, safe_num)
    } else {                                              # unique Sigmoid utility 
      eff_beta = inidividual_utility_Sshape_setting$eff_beta
      eff_shape = inidividual_utility_Sshape_setting$eff_shape 
      safe_beta = inidividual_utility_Sshape_setting$safe_beta
      safe_shape = inidividual_utility_Sshape_setting$safe_shape
    }
  } else if(overall_setting$utility_type == 2) {     # Stepwise utility
    if(overall_setting$individual_stepwise_utility == 2) {   # unified Stepwise utility 
      eff_knot_num = rep(utility_stepwise_setting$eff_knot_num, eff_num)
      eff_measure = replicate(eff_num, utility_stepwise_setting$eff_measure, simplify = FALSE)
      eff_score = replicate(eff_num, utility_stepwise_setting$eff_score, simplify = FALSE)
      safe_knot_num = rep(utility_stepwise_setting$safe_knot_num, safe_num)
      safe_measure = replicate(safe_num, utility_stepwise_setting$safe_measure, simplify = FALSE)
      safe_score = replicate(safe_num, utility_stepwise_setting$safe_score, simplify = FALSE)
    } else {                                                 # unique Stepwise utility 
      eff_knot_num = individual_utility_stepwise_setting$eff_knot_num
      eff_measure = individual_utility_stepwise_setting$eff_measure
      eff_score = individual_utility_stepwise_setting$eff_score
      safe_knot_num = individual_utility_stepwise_setting$safe_knot_num
      safe_measure = individual_utility_stepwise_setting$safe_measure
      safe_score = individual_utility_stepwise_setting$safe_score
    }

  }
  
  # Rate Calculation (Exposure -> Rate) #### 
  
  if(!is.na(eff_num) & eff_num > 0) {
    for(i in 1:eff_num) {
      # per-endpoint model (mixed endpoints, B scheme): the endpoint's type picks the
      # group's shared regression. type_vec is set by the user (simulate) or by the data
      # (upload, read-only). fall back to the scalar eff_PK_model if the vec is unset.
      eff_mdl <- if(!is.null(overall_setting$eff_type_vec) &&
                    length(overall_setting$eff_type_vec) >= i &&
                    !is.na(overall_setting$eff_type_vec[i])) {
        if(overall_setting$eff_type_vec[i] == "cont") overall_setting$eff_cont_model
        else overall_setting$eff_bin_model
      } else overall_setting$eff_PK_model
      # prob_eff
      if(eff_mdl == 1) {   # Sigmoid 
        eff_inter <- eff_endpoint_setting$eff_intercept[i]
        eff_slope <-  eff_endpoint_setting$eff_slope[i]
        prob_eff <- 1/(1 + exp(pmin(pmax(-eff_inter - eff_slope * Score_dt$PK, -709), 709)))
      } else if(eff_mdl == 2) {    # Emax           
        eff_baseline <- eff_endpoint_setting$eff_baseline[i]
        eff_Emax <- eff_endpoint_setting$eff_Emax[i]
        eff_EC50 <- eff_endpoint_setting$eff_EC50[i]
        eff_hill <- eff_endpoint_setting$eff_hill[i]
        E <- eff_baseline+eff_Emax*(Score_dt$PK^eff_hill)/(eff_EC50^eff_hill+Score_dt$PK^eff_hill)
        prob_eff <- exp(E)/(1+exp(E))
      } else if(eff_mdl %in% c(3, 4, 5)) {   # continuous: linear / log-linear / exponential
        if(use_real == TRUE) {   # Upload: use fitted continuous coefficients + empirical CDF of observed Y
          ab <- switch(as.character(eff_mdl),
                       "3" = c(eff_endpoint_setting$eff_lin_a[i], eff_endpoint_setting$eff_lin_b[i]),
                       "4" = c(eff_endpoint_setting$eff_log_a[i], eff_endpoint_setting$eff_log_b[i]),
                       "5" = c(eff_endpoint_setting$eff_exp_a[i], eff_endpoint_setting$eff_exp_b[i]))
          eff_inter <- ab[1]; eff_slope <- ab[2]
          Yhat <- switch(as.character(eff_mdl),
                         "3" = eff_inter + eff_slope * Score_dt$PK,
                         "4" = eff_inter + eff_slope * log(Score_dt$PK),
                         "5" = exp(pmin(pmax(eff_inter + eff_slope * Score_dt$PK, -709), 709)))
          Yobs <- ER_data_list$ER_rawdt[[paste0("EFF", i)]]
          # optional user response bounds -> truncated support (decision a): restrict the
          # observed-Y ECDF baseline to [lb, ub] and clip Yhat into [lb, ub] so predictions
          # beyond the support map to the truncated CDF's ends. NA side = no truncation.
          lb <- eff_endpoint_setting$eff_resp_lb[i]
          ub <- eff_endpoint_setting$eff_resp_ub[i]
          if (!is.null(lb) && !is.na(lb)) Yhat <- pmax(Yhat, lb)
          if (!is.null(ub) && !is.na(ub)) Yhat <- pmin(Yhat, ub)
          prob_eff <- continuous_ecdf_map(Yobs, Yhat, lb, ub)
        } else {                 # Simulate: user-typed intercept/slope + min-max mapping
          eff_inter <- eff_endpoint_setting$eff_intercept[i]
          eff_slope <- eff_endpoint_setting$eff_slope[i]
          Yhat <- switch(as.character(eff_mdl),
                         "3" = eff_inter + eff_slope * Score_dt$PK,        # linear
                         "4" = eff_inter + eff_slope * log(Score_dt$PK),   # log-linear
                         "5" = exp(pmin(pmax(eff_inter + eff_slope * Score_dt$PK, -709), 709)))  # exponential
          # optional truncated support: clip predicted Y into [lb, ub] before the min-max
          # map. NA side = no bound. this bounds the response to the user's plausible range,
          # matching the upload-mode bounds behaviour. only the mapping input is affected;
          # the CUS core is untouched.
          lb <- eff_endpoint_setting$eff_resp_lb[i]
          ub <- eff_endpoint_setting$eff_resp_ub[i]
          if (!is.null(lb) && !is.na(lb)) Yhat <- pmax(Yhat, lb)
          if (!is.null(ub) && !is.na(ub)) Yhat <- pmin(Yhat, ub)
          Yrng <- max(Yhat) - min(Yhat)
          # min-max map predicted Y to [0,1]; flat ER (range 0) -> 0.5
          prob_eff <- if(Yrng == 0) rep(0.5, length(Yhat)) else (Yhat - min(Yhat)) / Yrng
        }
        # clamp away from exact 0/1 so the downstream sigmoid utility ratio stays finite
        prob_eff <- pmin(pmax(prob_eff, 1e-6), 1 - 1e-6)
      }
      
      ER_dt[[paste("Efficacy_V", i, sep = "")]]  <- prob_eff
      
      # Score_Eff
      if(overall_setting$utility_type == 1) {         # Sigmoid
        Score_dt[[paste("Efficacy_V", i, sep = "")]] <- 
          1/(1 + ((prob_eff * (1-eff_shape[i]))/(eff_shape[i]*(1-prob_eff)))^(-eff_beta[i]))
      } else if(overall_setting$utility_type == 2) {           # Stepwise 
        # eff_knot_num <- utility_stepwise_setting$eff_knot_num
        stepwise_param_raw <- data.frame(measurement = eff_measure[[i]][1:eff_knot_num[i]], 
                                         score = eff_score[[i]][1:eff_knot_num[i]]) %>% 
          na.omit() %>%                          # remove missing values
          arrange(measurement)                   # stepfun needs increasing x
        stepwise_param <- stepwise_param_raw %>%
          distinct(measurement, .keep_all = TRUE)  # strictly increasing (no ties)
        if (nrow(stepwise_param) < nrow(stepwise_param_raw)) {
          warning(paste0("efficacy endpoint ", i,
                         ": duplicate measurement value(s); kept the first, dropped the rest"))
          dup_warnings <- c(dup_warnings, paste0("Efficacy endpoint ", i))
        }
        measurement <- stepwise_param$measurement
        score <- stepwise_param$score

        f_step <- stepfun(measurement, c(score, max(1, score)), right = TRUE)
        Score_dt[[paste("Efficacy_V", i, sep = "")]] <- f_step(prob_eff)
      }
    }
  }
  
  if (!is.na(safe_num) & safe_num > 0) {
    for (i in 1:safe_num) {
      # per-endpoint model (mixed endpoints, B scheme): the endpoint's type picks the
      # group's shared regression. type_vec is set by the user (simulate) or by the data
      # (upload, read-only). fall back to the scalar safe_PK_model if the vec is unset.
      safe_mdl <- if(!is.null(overall_setting$safe_type_vec) &&
                     length(overall_setting$safe_type_vec) >= i &&
                     !is.na(overall_setting$safe_type_vec[i])) {
        if(overall_setting$safe_type_vec[i] == "cont") overall_setting$safe_cont_model
        else overall_setting$safe_bin_model
      } else overall_setting$safe_PK_model
      
      # ---- Probability model (sigmoid or Emax) ----
      if (safe_mdl == 1) {      # Sigmoid
        safe_inter <- safe_endpoint_setting$safe_intercept[i]
        safe_slope <- safe_endpoint_setting$safe_slope[i]
        prob_safe <- 1 / (1 + exp(pmin(pmax(-safe_inter - safe_slope * Score_dt$PK, -709), 709)))
        
      } else if (safe_mdl == 2) {  # Emax
        safe_baseline <- safe_endpoint_setting$safe_baseline[i]
        safe_Emax <- safe_endpoint_setting$safe_Emax[i]
        safe_EC50 <- safe_endpoint_setting$safe_EC50[i]
        safe_hill <- safe_endpoint_setting$safe_hill[i]
        E <- safe_baseline + safe_Emax * (Score_dt$PK^safe_hill) / (safe_EC50^safe_hill + Score_dt$PK^safe_hill)
        prob_safe <- exp(E) / (1 + exp(E))
      } else if (safe_mdl %in% c(3, 4, 5)) {  # continuous: linear / log-linear / exponential
        if(use_real == TRUE) {   # Upload: use fitted continuous coefficients + empirical CDF of observed Y
          ab <- switch(as.character(safe_mdl),
                       "3" = c(safe_endpoint_setting$safe_lin_a[i], safe_endpoint_setting$safe_lin_b[i]),
                       "4" = c(safe_endpoint_setting$safe_log_a[i], safe_endpoint_setting$safe_log_b[i]),
                       "5" = c(safe_endpoint_setting$safe_exp_a[i], safe_endpoint_setting$safe_exp_b[i]))
          safe_inter <- ab[1]; safe_slope <- ab[2]
          Yhat <- switch(as.character(safe_mdl),
                         "3" = safe_inter + safe_slope * Score_dt$PK,
                         "4" = safe_inter + safe_slope * log(Score_dt$PK),
                         "5" = exp(pmin(pmax(safe_inter + safe_slope * Score_dt$PK, -709), 709)))
          Yobs <- ER_data_list$ER_rawdt[[paste0("SAFE", i)]]
          # optional user response bounds -> truncated support (decision a): restrict the
          # observed-Y ECDF baseline to [lb, ub] and clip Yhat into [lb, ub].
          lb <- safe_endpoint_setting$safe_resp_lb[i]
          ub <- safe_endpoint_setting$safe_resp_ub[i]
          if (!is.null(lb) && !is.na(lb)) Yhat <- pmax(Yhat, lb)
          if (!is.null(ub) && !is.na(ub)) Yhat <- pmin(Yhat, ub)
          prob_safe <- continuous_ecdf_map(Yobs, Yhat, lb, ub)
        } else {                 # Simulate: user-typed intercept/slope + min-max mapping
          safe_inter <- safe_endpoint_setting$safe_intercept[i]
          safe_slope <- safe_endpoint_setting$safe_slope[i]
          Yhat <- switch(as.character(safe_mdl),
                         "3" = safe_inter + safe_slope * Score_dt$PK,        # linear
                         "4" = safe_inter + safe_slope * log(Score_dt$PK),   # log-linear
                         "5" = exp(pmin(pmax(safe_inter + safe_slope * Score_dt$PK, -709), 709)))  # exponential
          # optional truncated support: clip predicted Y into [lb, ub] before the min-max map
          lb <- safe_endpoint_setting$safe_resp_lb[i]
          ub <- safe_endpoint_setting$safe_resp_ub[i]
          if (!is.null(lb) && !is.na(lb)) Yhat <- pmax(Yhat, lb)
          if (!is.null(ub) && !is.na(ub)) Yhat <- pmin(Yhat, ub)
          Yrng <- max(Yhat) - min(Yhat)
          # min-max map predicted Y to [0,1]; flat ER (range 0) -> 0.5. direction (1-m) handled by utility below
          prob_safe <- if(Yrng == 0) rep(0.5, length(Yhat)) else (Yhat - min(Yhat)) / Yrng
        }
        prob_safe <- pmin(pmax(prob_safe, 1e-6), 1 - 1e-6)
      }
      
      ER_dt[[paste0("Safety_V", i)]] <- prob_safe
      
      # ---- Utility model (sigmoid or stepwise) ----
      if (overall_setting$utility_type == 1) {   # Sigmoid
        Score_dt[[paste0("Safety_V", i)]] <- 
          1 - 1 / (1 + ((prob_safe * (1 - safe_shape[i])) / (safe_shape[i] * (1 - prob_safe)))^(-safe_beta[i]))
        
      } else if (overall_setting$utility_type == 2) {   # Stepwise
        # safe_knot_num <- utility_stepwise_setting$safe_knot_num
        stepwise_param_raw <- data.frame(measurement = safe_measure[[i]][1:safe_knot_num[i]], 
                                         score = safe_score[[i]][1:safe_knot_num[i]]) %>% 
          na.omit() %>%                          # remove missing values
          arrange(measurement)                   # stepfun needs increasing x
        stepwise_param <- stepwise_param_raw %>%
          distinct(measurement, .keep_all = TRUE)  # strictly increasing (no ties)
        if (nrow(stepwise_param) < nrow(stepwise_param_raw)) {
          warning(paste0("safety endpoint ", i,
                         ": duplicate measurement value(s); kept the first, dropped the rest"))
          dup_warnings <- c(dup_warnings, paste0("Safety endpoint ", i))
        }
        measurement <- stepwise_param$measurement
        score <- stepwise_param$score

        f_step <- stepfun(measurement, c(score[1], score), right = TRUE)
        Score_dt[[paste0("Safety_V", i)]] <- f_step(prob_safe)
      }
    }
  }
  
  if(length(weight) > 0) {
    score_mat <- as.matrix(Score_dt[,-1])
    if(overall_setting$cus_agg_type == 2) {   # linear weighted average: sum(w_i * s_i)
      Score_dt$CUS = sweep(score_mat, MARGIN = 2, FUN = "*", STATS = per_weight) %>% 
        apply(., MARGIN = 1, FUN = sum)
    } else {                                  # multiplicative (default): exp(sum(w_i * log(s_i)))
      Score_dt$CUS = sweep(as.matrix(log(Score_dt[,-1])), MARGIN = 2, FUN = "*", STATS = per_weight) %>% 
        apply(., MARGIN = 1, FUN = sum) %>% exp()
    }
  }
  
  return(list(ER_dt = ER_dt %>% arrange(PK), Score_dt = Score_dt %>% arrange(PK),
              dup_warnings = unique(dup_warnings)))
}