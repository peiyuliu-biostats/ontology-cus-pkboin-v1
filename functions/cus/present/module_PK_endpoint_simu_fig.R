module_UI_PK_endpoint_simu_fig <- function(id)
{
  ns <- NS(id)
  tagList(
    uiOutput(ns("UI_PK_endpoint_simu_fig"))
  )

}

module_server_PK_endpoint_simu_fig <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  output$UI_PK_endpoint_simu_fig <- renderUI({
    # upload 
    req(all_rv$overall_setting$simu_or_not == 1           # simulate 
    )
    plotOutput(ns("PK_endpoint_ggplot"), width = "700px", height = "600px")
  })
  
  output$PK_endpoint_ggplot <- renderPlot({
    x <- seq(from = all_rv$PK_setting$PK_min, 
             to = all_rv$PK_setting$PK_max, length = 500)
    
    plotdt <- data.frame(x = NULL, y = NULL, endpoint = NULL)
    eff_num <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    
    if(!is.na(eff_num) & eff_num > 0) {
      for(i in 1:eff_num) {
        # per-endpoint model (B scheme): type picks the group's shared regression; fall back to scalar
        mdl <- if(!is.null(all_rv$overall_setting$eff_type_vec) &&
                  length(all_rv$overall_setting$eff_type_vec) >= i &&
                  !is.na(all_rv$overall_setting$eff_type_vec[i])) {
          if(all_rv$overall_setting$eff_type_vec[i] == "cont") all_rv$overall_setting$eff_cont_model
          else all_rv$overall_setting$eff_bin_model
        } else all_rv$overall_setting$eff_PK_model
        if(mdl == 1) {            # Logistic
          eff_slope <- all_rv$eff_endpoint_setting$eff_slope[i]
          eff_intercept <- all_rv$eff_endpoint_setting$eff_intercept[i]
          tmp <- data.frame(x = x, 
                            y = exp(eff_intercept + eff_slope * x)/(1 + exp(eff_intercept + eff_slope * x)),
                            endpoint = paste0("Efficacy_", i))
          plotdt <- rbind(plotdt, tmp)
        } else if(mdl == 2) {     # Emax
          eff_baseline <- all_rv$eff_endpoint_setting$eff_baseline[i]
          eff_Emax <- all_rv$eff_endpoint_setting$eff_Emax[i]
          eff_EC50 <- all_rv$eff_endpoint_setting$eff_EC50[i]
          eff_hill <- all_rv$eff_endpoint_setting$eff_hill[i]
          E <- eff_baseline+eff_Emax*(x^eff_hill)/(eff_EC50^eff_hill+x^eff_hill)
          tmp <- data.frame(x = x, 
                            y = exp(E)/(1+exp(E)),
                            endpoint = paste0("Efficacy_", i))
          plotdt <- rbind(plotdt, tmp)
        } else if(mdl %in% c(3, 4, 5)) {   # continuous: linear / log-linear / exponential
          eff_intercept <- all_rv$eff_endpoint_setting$eff_intercept[i]
          eff_slope <- all_rv$eff_endpoint_setting$eff_slope[i]
          Yhat <- switch(as.character(mdl),
                         "3" = eff_intercept + eff_slope * x,
                         "4" = eff_intercept + eff_slope * log(x),
                         "5" = exp(pmin(pmax(eff_intercept + eff_slope * x, -709), 709)))
          Yrng <- max(Yhat) - min(Yhat)
          y <- if(Yrng == 0) rep(0.5, length(Yhat)) else (Yhat - min(Yhat)) / Yrng
          tmp <- data.frame(x = x, y = y, endpoint = paste0("Efficacy_", i))
          plotdt <- rbind(plotdt, tmp)
        }
      }
    }
    
    if(!is.na(safe_num) & safe_num > 0) {
      for(i in 1:safe_num) {
        # per-endpoint model (B scheme): type picks the group's shared regression; fall back to scalar
        mdl <- if(!is.null(all_rv$overall_setting$safe_type_vec) &&
                  length(all_rv$overall_setting$safe_type_vec) >= i &&
                  !is.na(all_rv$overall_setting$safe_type_vec[i])) {
          if(all_rv$overall_setting$safe_type_vec[i] == "cont") all_rv$overall_setting$safe_cont_model
          else all_rv$overall_setting$safe_bin_model
        } else all_rv$overall_setting$safe_PK_model
        if(mdl == 1) {            # Logistic
          safe_slope <- all_rv$safe_endpoint_setting$safe_slope[i]
          safe_intercept <- all_rv$safe_endpoint_setting$safe_intercept[i]
          tmp <- data.frame(x = x, 
                            y = exp(safe_intercept + safe_slope * x)/(1 + exp(safe_intercept + safe_slope * x)),
                            endpoint = paste0("Safety_", i))
          plotdt <- rbind(plotdt, tmp)
        } else if(mdl == 2) {     # Emax 
          safe_baseline <- all_rv$safe_endpoint_setting$safe_baseline[i]
          safe_Emax <- all_rv$safe_endpoint_setting$safe_Emax[i]
          safe_EC50 <- all_rv$safe_endpoint_setting$safe_EC50[i]
          safe_hill <- all_rv$safe_endpoint_setting$safe_hill[i]
          E <- safe_baseline+safe_Emax*(x^safe_hill)/(safe_EC50^safe_hill+x^safe_hill)
          tmp <- data.frame(x = x, 
                            y = exp(E)/(1+exp(E)),
                            endpoint = paste0("Safety_", i))
          plotdt <- rbind(plotdt, tmp)
        } else if(mdl %in% c(3, 4, 5)) {   # continuous: linear / log-linear / exponential
          safe_intercept <- all_rv$safe_endpoint_setting$safe_intercept[i]
          safe_slope <- all_rv$safe_endpoint_setting$safe_slope[i]
          Yhat <- switch(as.character(mdl),
                         "3" = safe_intercept + safe_slope * x,
                         "4" = safe_intercept + safe_slope * log(x),
                         "5" = exp(pmin(pmax(safe_intercept + safe_slope * x, -709), 709)))
          Yrng <- max(Yhat) - min(Yhat)
          y <- if(Yrng == 0) rep(0.5, length(Yhat)) else (Yhat - min(Yhat)) / Yrng
          tmp <- data.frame(x = x, y = y, endpoint = paste0("Safety_", i))
          plotdt <- rbind(plotdt, tmp)
        }
      }
    }
    eff_colors_all <- c("#003f5c", "#006D2C", "#665191", "#41B6C4", "#9ECAE1",
                        "#1f77b4", "#17becf", "#2ca02c", "#1C9099", "#9467bd")
    
    safe_colors_all <- c("#d62728", "#D55E00", "#F0E442", "#A6761D", "#E7298A",
                         "#bc5090", "#B22222", "#F781BF", "#f95d6a", "#dd5182")
    
    eff_col  <- if (!is.na(eff_num) & eff_num  > 0) eff_colors_all[seq_len(eff_num)]  else character(0)
    safe_col <- if (!is.na(safe_num) & safe_num > 0) safe_colors_all[seq_len(safe_num)] else character(0)
    
    color_values <- c(
      if (!is.na(eff_num) & eff_num  > 0) setNames(eff_col,  paste0("Efficacy_", seq_len(eff_num))),
      if (!is.na(safe_num) & safe_num > 0) setNames(safe_col, paste0("Safety_",  seq_len(safe_num)))
    )
    
    ymin <- max(min(plotdt$y) - 0.05, 0)
    ymax <- min(max(plotdt$y) + 0.05, 1)
    if(coalesce(eff_num,0) + coalesce(safe_num, 0) > 0) {
      ggplot(plotdt, aes(x = x, y = y, color = endpoint)) + 
        geom_line(linewidth = 1) +
        ylim(c(ymin, ymax)) +
        scale_color_manual(name = "Endpoints", values = color_values) +
        labs(
          title = "PK - Endpoints Regression Curves",
          x = "PK",
          y = "Probability"
        ) +
        theme_minimal(base_size = 16) + 
        theme(
          strip.text = element_text(size = 18, face = "bold"),
          panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1)
        )
    }
  })
}