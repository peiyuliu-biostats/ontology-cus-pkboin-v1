update_ER_settings <- function(new_data, all_rv)
{
  all_rv$ER_data_list$ER_rawdt <- new_data     # save the data 
  
  # update number of endpoints 
  eff_num <- sum(str_detect(colnames(new_data), "EFF"))
  safe_num <- sum(str_detect(colnames(new_data), "SAFE"))
  all_rv$endpoint_num_setting$eff_num <- eff_num
  all_rv$endpoint_num_setting$safe_num <- safe_num
  
  # update the limits of PK variable 
  PKmin <- min(new_data$PK, na.rm = T)
  PKmax <- max(new_data$PK, na.rm = T)
  all_rv$PK_setting$PK_min <- floor(PKmin * 10) / 10
  all_rv$PK_setting$PK_max <- ceiling(PKmax * 10) / 10
  
  # save the logistic regression results 
  if(eff_num > 0) {
    for(i in 1:eff_num) {
      # logistic / Emax only apply to binary (0/1) endpoints; skip for continuous data
      if(is_binary_endpoint(new_data[[paste0("EFF", i)]])) {
        # Sigmoid Regression ----- # 
        eff_fit <- glm(as.formula(paste("EFF", i, " ~ PK", sep = "")), 
                       data = new_data, family = binomial)
        # save model
        all_rv$ER_data_list$eff_logistic_reg[[i]] <- eff_fit
        # save estimated intercept and slope
        all_rv$eff_endpoint_setting$eff_intercept[i] <- round(coef(eff_fit)[1],2)
        all_rv$eff_endpoint_setting$eff_slope[i] <- round(coef(eff_fit)[2],2)
        
        # Emax Regression ----- # 
        eff_Emax_fit <- fun_Emax_est(new_data, "PK", paste0("EFF", i))
        
        # save model 
        all_rv$ER_data_list$eff_Emax_reg[[i]] <- eff_Emax_fit
        
        all_rv$eff_endpoint_setting$eff_baseline[i] <- round(eff_Emax_fit$par[1], 2)
        all_rv$eff_endpoint_setting$eff_Emax[i] <- round(eff_Emax_fit$par[2], 2)
        all_rv$eff_endpoint_setting$eff_EC50[i] <- round(exp(eff_Emax_fit$par[3]), 2)  # !
        all_rv$eff_endpoint_setting$eff_hill[i] <- 1
        # Note: hill is fixed as 1 in Emax regression model 
      }

      # Continuous Regression (linear / log-linear / exponential) ----- #
      yv <- new_data[[paste0("EFF", i)]]
      pkv <- new_data$PK
      cf <- fit_continuous_coef(pkv, yv)
      all_rv$eff_endpoint_setting$eff_lin_a[i] <- cf$lin_a
      all_rv$eff_endpoint_setting$eff_lin_b[i] <- cf$lin_b
      all_rv$eff_endpoint_setting$eff_log_a[i] <- cf$log_a
      all_rv$eff_endpoint_setting$eff_log_b[i] <- cf$log_b
      all_rv$eff_endpoint_setting$eff_exp_a[i] <- cf$exp_a
      all_rv$eff_endpoint_setting$eff_exp_b[i] <- cf$exp_b
    }
  }
  
  if(safe_num > 0) {
    for(i in 1:safe_num) {
      # logistic / Emax only apply to binary (0/1) endpoints; skip for continuous data
      if(is_binary_endpoint(new_data[[paste0("SAFE", i)]])) {
        # Sigmoid Regression ----- # 
        safe_fit <- glm(as.formula(paste("SAFE", i, " ~ PK", sep = "")), 
                        data = new_data, family = binomial)
        # save model 
        all_rv$ER_data_list$safe_logistic_reg[[i]] <- safe_fit
        # save estimated intercept and slope
        all_rv$safe_endpoint_setting$safe_intercept[i] <- round(coef(safe_fit)[1],2)
        all_rv$safe_endpoint_setting$safe_slope[i] <- round(coef(safe_fit)[2],2)
        
        # Emax Regression ----- # 
        safe_Emax_fit <- fun_Emax_est(new_data, "PK", paste0("SAFE", i))
        
        # save model 
        all_rv$ER_data_list$safe_Emax_reg[[i]] <- safe_Emax_fit
        
        all_rv$safe_endpoint_setting$safe_baseline[i] <- round(safe_Emax_fit$par[1], 2)
        all_rv$safe_endpoint_setting$safe_Emax[i] <- round(safe_Emax_fit$par[2], 2)
        all_rv$safe_endpoint_setting$safe_EC50[i] <- round(exp(safe_Emax_fit$par[3]), 2) # !
        all_rv$safe_endpoint_setting$safe_hill[i] <- 1
        # Note: hill is fixed as 1 in Emax regression model 
      }

      # Continuous Regression (linear / log-linear / exponential) ----- #
      yv <- new_data[[paste0("SAFE", i)]]
      pkv <- new_data$PK
      cf <- fit_continuous_coef(pkv, yv)
      all_rv$safe_endpoint_setting$safe_lin_a[i] <- cf$lin_a
      all_rv$safe_endpoint_setting$safe_lin_b[i] <- cf$lin_b
      all_rv$safe_endpoint_setting$safe_log_a[i] <- cf$log_a
      all_rv$safe_endpoint_setting$safe_log_b[i] <- cf$log_b
      all_rv$safe_endpoint_setting$safe_exp_a[i] <- cf$exp_a
      all_rv$safe_endpoint_setting$safe_exp_b[i] <- cf$exp_b
    }
  }
  
  # determine uploaded-data endpoint type per group (all-binary -> TRUE, else continuous -> FALSE)
  if (eff_num > 0) {
    all_rv$overall_setting$eff_data_binary <-
      all(sapply(1:eff_num, function(i) is_binary_endpoint(new_data[[paste0("EFF", i)]])))
  } else {
    all_rv$overall_setting$eff_data_binary <- NA
  }
  if (safe_num > 0) {
    all_rv$overall_setting$safe_data_binary <-
      all(sapply(1:safe_num, function(i) is_binary_endpoint(new_data[[paste0("SAFE", i)]])))
  } else {
    all_rv$overall_setting$safe_data_binary <- NA
  }

  # per-endpoint type vector (B scheme, upload): detected from the data, READ-ONLY.
  # "bin" if the column is 0/1, else "cont". the shared per-type regression is what the
  # user selects in the sidebar; default it here (cont -> Linear, bin -> Logistic) so the
  # first render computes with a valid model. mirrors how simulate uses these fields.
  ev <- all_rv$overall_setting$eff_type_vec
  if (eff_num > 0) {
    for (i in 1:eff_num)
      ev[i] <- if (is_binary_endpoint(new_data[[paste0("EFF", i)]])) "bin" else "cont"
    all_rv$overall_setting$eff_type_vec <- ev
  }
  sv <- all_rv$overall_setting$safe_type_vec
  if (safe_num > 0) {
    for (i in 1:safe_num)
      sv[i] <- if (is_binary_endpoint(new_data[[paste0("SAFE", i)]])) "bin" else "cont"
    all_rv$overall_setting$safe_type_vec <- sv
  }
  # reset shared per-type regression to defaults for the newly uploaded dataset
  all_rv$overall_setting$eff_cont_model  <- 3
  all_rv$overall_setting$eff_bin_model   <- 1
  all_rv$overall_setting$safe_cont_model <- 3
  all_rv$overall_setting$safe_bin_model  <- 1

  # reset optional continuous response bounds (NA = no clipping) for the new dataset
  all_rv$eff_endpoint_setting$eff_resp_lb   <- rep(NA_real_, 10)
  all_rv$eff_endpoint_setting$eff_resp_ub   <- rep(NA_real_, 10)
  all_rv$safe_endpoint_setting$safe_resp_lb <- rep(NA_real_, 10)
  all_rv$safe_endpoint_setting$safe_resp_ub <- rep(NA_real_, 10)

  all_rv$triggers$update_ER_dataset <- Sys.time()
}