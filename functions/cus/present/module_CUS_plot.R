module_UI_CUS_plot <- function(id)
{
  ns <- NS(id)
  plotlyOutput(ns("CUS_plot"))
}

module_server_CUS_plot <- function(input, output, session, all_rv, rv)
{
  # CUS figure
  output$CUS_plot <- renderPlotly({
    eff_num <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    ER_dt <- all_rv$PK_data()$ER_dt
    Score_dt <- all_rv$PK_data()$Score_dt
    
    max_CUS  <- max(Score_dt$CUS, na.rm = TRUE)
    PK_at_max <- min(Score_dt$PK[Score_dt$CUS == max_CUS], na.rm = TRUE)
    
    eff_colors_all <- c("#003f5c", "#006D2C", "#665191", "#41B6C4", "#9ECAE1",
                        "#1f77b4", "#17becf", "#2ca02c", "#1C9099", "#9467bd")
    
    safe_colors_all <- c("#d62728", "#D55E00", "#F0E442", "#A6761D", "#E7298A",
                         "#bc5090", "#B22222", "#F781BF", "#f95d6a", "#dd5182")
    
    eff_col  <- if (eff_num  > 0) eff_colors_all[seq_len(eff_num)]  else character(0)
    safe_col <- if (safe_num > 0) safe_colors_all[seq_len(safe_num)] else character(0)
    
    color_values <- c(
      if (eff_num  > 0) setNames(eff_col,  paste0("Efficacy_V", seq_len(eff_num))),
      if (safe_num > 0) setNames(safe_col, paste0("Safety_V",  seq_len(safe_num)))
    )
    
    all_colors <- c(setNames("#CC00FF", "CUS"), color_values)
    
    fig <-  
      plot_ly(data = Score_dt, x = ~PK, y = ~CUS, type = 'scatter', mode = 'lines',
              line = list(color = '#CC00FF', width = 3),
              hovertemplate = paste("PK: %{x:.3f}", "<br>CUS: %{y:.3f}", 
                                    "<extra></extra>"),
              name = "CUS") %>% 
      add_lines(
        x = c(PK_at_max, PK_at_max),
        y = c(0, 1),
        line = list(color = "red", width = 3, dash = "dash"),
        showlegend = FALSE   
      )
    
    if(eff_num > 0) {
      for (i in 1:eff_num) {
        v <- paste0("Efficacy_V", i)
        fig <- fig %>% add_trace(data = ER_dt, x = ~PK, y = ER_dt[[v]], type='scatter', mode='lines',
                                 line=list(color=eff_col[i],width=1.5),
                                 hovertemplate=paste0("PK: %{x:.3f}<br>",v,": %{y:.3f}<extra></extra>"),
                                 name=v)
      }
    }
    
    if(safe_num > 0) {
      for (i in 1:safe_num) {
        v <- paste0("Safety_V", i)
        fig <- fig %>% add_trace(data = ER_dt, x = ~PK, y = ER_dt[[v]], type='scatter', mode='lines',
                                 line=list(color=safe_col[i],width=1.5),
                                 hovertemplate=paste0("PK: %{x:.3f}<br>",v,": %{y:.3f}<extra></extra>"),
                                 name=v)
      }
    }
    
    # bootstrap to generate CI 
    if (isTRUE(rv$need_CUS_CI) & eff_num + safe_num > 0) {
      boot <- bootstrap_CUS_data(all_rv)
      if (!isTRUE(boot$warning)) {
      bdt <- data.frame(PK=boot$PK_gen, LB=boot$boot_CUS_LB, UB=boot$boot_CUS_UB)
    
      # CI for CUS 
      fig <- fig %>% add_ribbons(data=bdt, x=~PK, ymin=~LB, ymax=~UB,
                                 fillcolor="rgba(208,140,255,0.3)", line=list(color="transparent"),
                                 name="95% CI", inherit = FALSE)
      
      fig <- fig %>% add_trace(data=bdt, x=~PK, y=~LB, type='scatter', mode='lines',
                               line=list(color="#D08CFF",width=1), 
                               hovertemplate=paste0("PK: %{x:.3f}", "<br>CUS_LB: %{y:.3f}<extra></extra>"),
                               name="CUS LB")
      
      fig <- fig %>% add_trace(data=bdt, x=~PK, y=~UB, type='scatter', mode='lines',
                               line=list(color="#D08CFF",width=1), 
                               hovertemplate=paste0("PK: %{x:.3f}", "<br>CUS_UB: %{y:.3f}<extra></extra>"),
                               name="CUS UB")
      }
    }
    
    if(isTRUE(rv$need_PK_CI) & eff_num + safe_num > 0) {
      # CI for PK 
      boot <- bootstrap_CUS_data(all_rv)
      if (!isTRUE(boot$warning)) {
      PK_LB <- boot$boot_PK_est_LB
      PK_UB <- boot$boot_PK_est_UB
      y_min <- 0
      y_max <- 1
      # fig <- fig %>% 
      #   add_polygons(
      #     x = c(PK_LB, PK_UB, PK_UB, PK_LB),
      #     y = c(y_min, y_min, y_max, y_max),
      #     fillcolor = "rgba(180,180,180,0.25)", line = list(color = "transparent"),
      #     hoverinfo = "none", showlegend = FALSE, inherit = FALSE
      #   ) %>% 
      fig <- fig %>% 
        add_segments(
          x = PK_LB, xend = PK_LB,
          y = y_min, yend = y_max,
          line = list(color = "rgba(100,100,100,0.8)", width = 2, dash = "dash"),
          showlegend = FALSE, hoverinfo = "none"
        ) %>% 
        add_segments(
          x = PK_UB, xend = PK_UB,
          y = y_min, yend = y_max,
          line = list(color = "rgba(100,100,100,0.8)", width = 2, dash = "dash"),
          showlegend = FALSE, hoverinfo = "none"
        ) %>% 
        layout(
          shapes = list(
            list(type = "rect", x0 = PK_LB, x1 = PK_UB, 
                 y0 = 0,    y1 = 1, fillcolor = "rgba(180,180,180,0.25)",
                 line = list(width = 0)
            )
          )
        ) 
      }
    }
    
    # layout 
    if(eff_num + safe_num > 0) {
      fig %>% 
        layout(
          title = list(
            text = "Clinical Utility Score",
            xanchor = "center",
            yanchor = "top",
            font = list(size = 16, family = "Arial", color = "black")
          ),
          xaxis = list(title = list(text = "PK", font = list(size = 14))),
          yaxis = list(title = list(text = "CUS", font = list(size = 14))),
          margin = list(t = 60, r = 20, l = 60, b = 50)
        ) %>% config(
          toImageButtonOptions = list(
            format = "png",
            filename = "CUS_plot",
            scale = 2
          )
        )
    }
  })
}