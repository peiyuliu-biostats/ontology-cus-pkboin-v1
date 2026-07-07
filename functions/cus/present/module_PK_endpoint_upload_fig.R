module_UI_PK_endpoint_upload_fig <- function(id)
{
  ns <- NS(id)
  tagList(
    uiOutput(ns("UI_sigmoid_endpoint_upload_fig"))
  )
}


module_server_PK_endpoint_upload_fig <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  output$UI_sigmoid_endpoint_upload_fig <- renderUI({
    # upload 
    req(all_rv$overall_setting$simu_or_not == 2 &           # upload 
        nrow(all_rv$ER_data_list$ER_rawdt) > 0)             # have uploaded data 
    
    eff_num <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    
    nrow_needed <- ceiling((eff_num + safe_num) / 2)
    height = paste0(nrow_needed * 300, "px")
    
    plotOutput(ns("endpoint_sigmoid_CI_ggplot"), width = "800px", height = height)
  })
  
  output$endpoint_sigmoid_CI_ggplot <- renderPlot({
    
    rawdt <- all_rv$ER_data_list$ER_rawdt
    eff_num  <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    
    longdt <- rawdt %>%
      pivot_longer(
        cols = matches("^(EFF|SAFE)"),   
        names_to = "Endpoint",
        values_to = "Result"
      ) %>% 
      filter(
        (eff_num  > 0 & str_detect(Endpoint, "^EFF")  & as.numeric(str_extract(Endpoint, "\\d+"))  <= eff_num) |
          (safe_num > 0 & str_detect(Endpoint, "^SAFE") & as.numeric(str_extract(Endpoint, "\\d+")) <= safe_num)
      )
    
    # add label ---- # 
    longdt$Type <- ifelse(str_detect(longdt$Endpoint, "^EFF"), "eff", "safe")

    # per-endpoint model resolver (B scheme): the endpoint's detected type picks the
    # group's shared regression. falls back to the group scalar if the type vec is unset.
    os <- all_rv$overall_setting
    eff_mdl_i <- function(i) {
      tv <- os$eff_type_vec
      if (!is.null(tv) && length(tv) >= i && !is.na(tv[i]))
        (if (tv[i] == "cont") os$eff_cont_model else os$eff_bin_model)
      else os$eff_PK_model
    }
    safe_mdl_i <- function(i) {
      tv <- os$safe_type_vec
      if (!is.null(tv) && length(tv) >= i && !is.na(tv[i]))
        (if (tv[i] == "cont") os$safe_cont_model else os$safe_bin_model)
      else os$safe_PK_model
    }
    ep_model <- function(ep) {
      i <- as.numeric(str_extract(ep, "\\d+"))
      if (str_detect(ep, "^EFF")) eff_mdl_i(i) else safe_mdl_i(i)
    }

    # plotted y: binary endpoints use the raw 0/1 result; continuous endpoints use the
    # mapped m = ECDF(observed Y) so the panel y-axis is the [0,1] probability/score.
    # decided PER ENDPOINT from its own resolved model (supports mixed groups).
    longdt$plot_y <- longdt$Result
    for (ep in unique(longdt$Endpoint)) {
      if (ep_model(ep) %in% c(3, 4, 5)) {
        idx <- longdt$Endpoint == ep
        yobs <- longdt$Result[idx]
        longdt$plot_y[idx] <- continuous_ecdf_map(yobs, yobs)  # rank-map observed Y to [0,1]
      }
    }
    
    # add Emax variable ---- # 
    longdt$Emax_est <- NA
    longdt$Emax_LB <- NA
    longdt$Emax_UB <- NA
    
    # ---- plot the figure ---- # 
    
    p <- ggplot(longdt, aes(x = PK, y = plot_y)) +
      geom_jitter(height = 0.02, width = 0.02, alpha = 0.4)
    
    # ---- efficacy curves: per endpoint, by its resolved model ----
    # endpoints resolved to each model type (supports mixed groups)
    eff_log_eps  <- if (eff_num > 0) which(sapply(1:eff_num, function(i) eff_mdl_i(i) == 1)) else integer(0)
    eff_emax_eps <- if (eff_num > 0) which(sapply(1:eff_num, function(i) eff_mdl_i(i) == 2)) else integer(0)

    # logistic endpoints: per-endpoint glm smooth WITH SE band (matches original look)
    if (length(eff_log_eps) > 0) {
      p <- p + geom_smooth(
        data = longdt %>% filter(Endpoint %in% paste0("EFF", eff_log_eps)),
        aes(group = Endpoint),
        method = "glm",
        method.args = list(family = "binomial"),
        color = "steelblue",
        fill = "lightblue",
        se = TRUE
      )
    }

    # Emax endpoints: estimate + bootstrap CI (unchanged math; just restricted to Emax endpoints)
    if (length(eff_emax_eps) > 0) {
      for(i in eff_emax_eps) {
          # Emax estimate 
          E0_est <- all_rv$eff_endpoint_setting$eff_baseline[i]
          Emax_est <- all_rv$eff_endpoint_setting$eff_Emax[i]
          EC50_est <- all_rv$eff_endpoint_setting$eff_EC50[i]
          PK <- longdt$PK[longdt$Endpoint == paste0("EFF", i)]
          lp <- E0_est + Emax_est * PK / (EC50_est + PK)
          longdt$Emax_est[longdt$Endpoint == paste0("EFF", i)] <- 1 / (1 + exp(-lp))
            
          # Emax UB and LB (bootstrap)
          if(Sys.info()[["sysname"]] == "Windows") {
            theta_sim <- lapply(1:100, FUN = function(x) {
              set.seed(x)
              index = sample(1:length(PK), size = length(PK), replace = T)
              optim(
                par     = c(E0_est, Emax_est, log(EC50_est)),
                fn      = negloglik_emax,
                x       = rawdt$PK[index],
                y       = rawdt[[paste0("EFF", i)]][index],
                method  = "BFGS",
                hessian = TRUE
              )$par
            }) %>% do.call(rbind, .)
          } else {
            theta_sim <- mclapply(1:500, FUN = function(x) {
              set.seed(x)
              index = sample(1:length(PK), size = length(PK), replace = T)
              optim(
                par     = c(E0_est, Emax_est, log(EC50_est)),
                fn      = negloglik_emax,
                x       = rawdt$PK[index],
                y       = rawdt[[paste0("EFF", i)]][index],
                method  = "BFGS",
                hessian = TRUE
              )$par
            }, mc.cores = 8) %>% do.call(rbind, .)
          }

          lp_matrix <- (theta_sim[,2] %*% matrix(PK, nrow = 1))/(outer(exp(theta_sim[,3]), PK, "+")) + theta_sim[,1]
          prob_matrix <- 1/(1 + exp(-lp_matrix))
          longdt$Emax_UB[longdt$Endpoint == paste0("EFF", i)] <- 
            as.vector(apply(prob_matrix, MARGIN = 2, FUN = function(x) quantile(x, prob = 0.975)))
          longdt$Emax_LB[longdt$Endpoint == paste0("EFF", i)] <- 
            as.vector(apply(prob_matrix, MARGIN = 2, FUN = function(x) quantile(x, prob = 0.025)))
        }
        p <- p +
          geom_ribbon(
            data = longdt %>% filter(Endpoint %in% paste0("EFF", eff_emax_eps)) %>% arrange(Endpoint, PK),
            aes(x = PK, ymin = Emax_LB, ymax = Emax_UB, group = Endpoint),
            inherit.aes = FALSE,
            fill = "lightblue",
            alpha = 0.25
          ) +
          geom_line(
            data = longdt %>% filter(Endpoint %in% paste0("EFF", eff_emax_eps)) %>% arrange(Endpoint, PK),
            aes(x = PK, y = Emax_est, group = Endpoint),
            inherit.aes = FALSE,
            color = "steelblue",
            linewidth = 1
          )
    }
    
    # ---- safety curves: per endpoint, by its resolved model ----
    safe_log_eps  <- if (safe_num > 0) which(sapply(1:safe_num, function(i) safe_mdl_i(i) == 1)) else integer(0)
    safe_emax_eps <- if (safe_num > 0) which(sapply(1:safe_num, function(i) safe_mdl_i(i) == 2)) else integer(0)

    if (length(safe_log_eps) > 0) {
      p <- p + geom_smooth(
        data = longdt %>% filter(Endpoint %in% paste0("SAFE", safe_log_eps)),
        aes(group = Endpoint),
        method = "glm",
        method.args = list(family = "binomial"),
        color = "darkred",
        fill = "pink",
        se = TRUE
      )
    }

    if (length(safe_emax_eps) > 0) {
      for(i in safe_emax_eps) {
          # Emax estimate 
          E0_est <- all_rv$safe_endpoint_setting$safe_baseline[i]
          Emax_est <- all_rv$safe_endpoint_setting$safe_Emax[i]
          EC50_est <- all_rv$safe_endpoint_setting$safe_EC50[i]
          PK <- longdt$PK[longdt$Endpoint == paste0("SAFE", i)]
          lp <- E0_est + Emax_est * PK / (EC50_est + PK)
          longdt$Emax_est[longdt$Endpoint == paste0("SAFE", i)] <- 1 / (1 + exp(-lp))
          
          # Emax UB and LB (bootstrap)
          if(Sys.info()[["sysname"]] == "Windows") {
            theta_sim <- lapply(1:100, FUN = function(x) {
              set.seed(x)
              index = sample(1:length(PK), size = length(PK), replace = T)
              optim(
                par     = c(E0_est, Emax_est, log(EC50_est)),
                fn      = negloglik_emax,
                x       = rawdt$PK[index],
                y       = rawdt[[paste0("SAFE", i)]][index],
                method  = "BFGS",
                hessian = TRUE
              )$par
            }) %>% do.call(rbind, .)
          } else {
            theta_sim <- mclapply(1:500, FUN = function(x) {
              set.seed(x)
              index = sample(1:length(PK), size = length(PK), replace = T)
              optim(
                par     = c(E0_est, Emax_est, log(EC50_est)),
                fn      = negloglik_emax,
                x       = rawdt$PK[index],
                y       = rawdt[[paste0("SAFE", i)]][index],
                method  = "BFGS",
                hessian = TRUE
              )$par
            }, mc.cores = 8) %>% do.call(rbind, .)
          }

          lp_matrix <- (theta_sim[,2] %*% matrix(PK, nrow = 1))/(outer(exp(theta_sim[,3]), PK, "+")) + theta_sim[,1]
          prob_matrix <- 1/(1 + exp(-lp_matrix))
          longdt$Emax_UB[longdt$Endpoint == paste0("SAFE", i)] <- 
            as.vector(apply(prob_matrix, MARGIN = 2, FUN = function(x) quantile(x, prob = 0.975)))
          longdt$Emax_LB[longdt$Endpoint == paste0("SAFE", i)] <- 
            as.vector(apply(prob_matrix, MARGIN = 2, FUN = function(x) quantile(x, prob = 0.025)))
        }
        p <- p +
          geom_ribbon(
            data = longdt %>% filter(Endpoint %in% paste0("SAFE", safe_emax_eps)) %>% arrange(Endpoint, PK),
            aes(x = PK, ymin = Emax_LB, ymax = Emax_UB, group = Endpoint),
            inherit.aes = FALSE,
            fill = "pink",
            alpha = 0.25
          ) +
          geom_line(
            data = longdt %>% filter(Endpoint %in% paste0("SAFE", safe_emax_eps)) %>% arrange(Endpoint, PK),
            aes(x = PK, y = Emax_est, group = Endpoint),
            inherit.aes = FALSE,
            color = "darkred",
            linewidth = 1
          )
    }

    # ---- continuous endpoints: m(PK) curve = ECDF(predicted Y) mapped to [0,1] ----
    cont_curve <- function(model, a, b, pk_grid, yobs) {
      Yhat <- switch(as.character(model),
                     "3" = a + b * pk_grid,
                     "4" = a + b * log(pk_grid),
                     "5" = exp(pmin(pmax(a + b * pk_grid, -709), 709)))
      continuous_ecdf_map(yobs, Yhat)
    }
    pk_grid <- seq(min(rawdt$PK, na.rm = TRUE), max(rawdt$PK, na.rm = TRUE), length.out = 200)
    curve_df <- data.frame()
    if (eff_num > 0) {
      for (i in 1:eff_num) {
        m_i <- eff_mdl_i(i)
        if (!(m_i %in% c(3, 4, 5))) next
        s <- all_rv$eff_endpoint_setting
        ab <- switch(as.character(m_i),
                     "3" = c(s$eff_lin_a[i], s$eff_lin_b[i]),
                     "4" = c(s$eff_log_a[i], s$eff_log_b[i]),
                     "5" = c(s$eff_exp_a[i], s$eff_exp_b[i]))
        m <- cont_curve(m_i, ab[1], ab[2], pk_grid, rawdt[[paste0("EFF", i)]])
        curve_df <- rbind(curve_df, data.frame(PK = pk_grid, m = m,
                                               Endpoint = paste0("EFF", i), col = "eff"))
      }
    }
    if (safe_num > 0) {
      for (i in 1:safe_num) {
        m_i <- safe_mdl_i(i)
        if (!(m_i %in% c(3, 4, 5))) next
        s <- all_rv$safe_endpoint_setting
        ab <- switch(as.character(m_i),
                     "3" = c(s$safe_lin_a[i], s$safe_lin_b[i]),
                     "4" = c(s$safe_log_a[i], s$safe_log_b[i]),
                     "5" = c(s$safe_exp_a[i], s$safe_exp_b[i]))
        m <- cont_curve(m_i, ab[1], ab[2], pk_grid, rawdt[[paste0("SAFE", i)]])
        curve_df <- rbind(curve_df, data.frame(PK = pk_grid, m = m,
                                               Endpoint = paste0("SAFE", i), col = "safe"))
      }
    }
    if (nrow(curve_df) > 0) {
      p <- p +
        geom_line(data = curve_df %>% filter(col == "eff"),
                  aes(x = PK, y = m, group = Endpoint),
                  inherit.aes = FALSE, color = "steelblue", linewidth = 1) +
        geom_line(data = curve_df %>% filter(col == "safe"),
                  aes(x = PK, y = m, group = Endpoint),
                  inherit.aes = FALSE, color = "darkred", linewidth = 1)
    }

    # dynamic title: mixed groups may use several models, so list the distinct ones present
    model_label <- function(m) vapply(m, function(x) switch(as.character(x),
                                      "1" = "Logistic", "2" = "Emax",
                                      "3" = "Linear", "4" = "Log-linear", "5" = "Exponential", ""),
                                      character(1))
    mdls <- c(if (eff_num > 0) sapply(1:eff_num, eff_mdl_i),
              if (safe_num > 0) sapply(1:safe_num, safe_mdl_i))
    labs_present <- unique(model_label(sort(unique(mdls))))
    plot_title <- paste0(paste(labs_present, collapse = " / "),
                         " Regression for Endpoints")

    p +
      facet_wrap(~ Endpoint, ncol = 2) +
      labs(
        title = plot_title,
        x = "PK",
        y = "Probability"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        strip.text   = element_text(size = 16, face = "bold"),
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
      )
  })
  
}