module_UI_inv_utility_stepwise_upload_eff <- function(id, index)
{
  ns <- NS(id)
  tagList(
    h6(paste0("For Efficacy Endpoint ", index, ":")),
    fluidRow(
      column(width = 6,
             DT::dataTableOutput(ns("inv_eff_stepwise_table"))
      ),
      column(width = 6, 
             plotlyOutput(ns("inv_stepwise_eff_figure"), width = "100%"))
    )
  )
}

module_server_inv_utility_stepwise_upload_eff <- function(input, output, session, index, all_rv)
{
  ns <- session$ns
  
  output$inv_eff_stepwise_table <- DT::renderDataTable({
    req(all_rv$individual_utility_stepwise_setting$eff_knot_num[index] > 0)
    eff_knot_num <- all_rv$individual_utility_stepwise_setting$eff_knot_num[index]
    eff_dt <- data.frame(measurement = all_rv$individual_utility_stepwise_setting$eff_measure[[index]][1:eff_knot_num],
                         score = all_rv$individual_utility_stepwise_setting$eff_score[[index]][1:eff_knot_num])
    DT::datatable(
      eff_dt,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     columnDefs = list(
                       list(className = "dt-center", targets = "_all"),  
                       list(width = '50%', targets = c(0, 1))         
                     )),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center;",
        paste0("Efficacy_V", index, " Stepwise Parameters")
      )
    )
  })
  
  # Stepwise Figure for Efficacy
  output$inv_stepwise_eff_figure <- renderPlotly({
    req(all_rv$individual_utility_stepwise_setting$eff_knot_num[index] > 0)
    eff_knot_num <- all_rv$individual_utility_stepwise_setting$eff_knot_num[index]
    df <- data.frame(measurement = all_rv$individual_utility_stepwise_setting$eff_measure[[index]][1:eff_knot_num], 
                     score = all_rv$individual_utility_stepwise_setting$eff_score[[index]][1:eff_knot_num]) %>% 
      na.omit()
    
    if(!0 %in% df$measurement) { df <- df %>% add_row(measurement = 0, score = min(df$score)) }
    if(!1 %in% df$measurement) { df <- df %>% add_row(measurement = 1, score = max(1, df$score)) }
    df <- df %>% arrange(measurement)
    
    p <- ggplot(df, aes(x = measurement, y = score)) +
      geom_step(direction = "vh", color = "steelblue", linewidth = 0.6) +
      geom_point(color = "darkorange", fill = "white", shape = 21, size = 2, stroke = 0.6) +
      xlab("Measurement") + ylab("Score") +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.6),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      )
    
    rt <- utility_response_ticks(all_rv, "eff", index)
    xaxis_cfg <- if (!is.null(rt)) {
      list(title = list(text = rt$axis_title, font = list(size = 14)), range = c(-0.05, 1.05),
           tickmode = "array", tickvals = rt$tickvals, ticktext = rt$ticktext)
    } else {
      list(title = list(text = "Measurement", font = list(size = 14)), range = c(-0.05, 1.05))
    }
    plotly::ggplotly(p, width = 400, height = 350) %>%
      layout(
        title = list(
          text = paste0("Utility Component for Efficacy_V", index),
          x = 0.5,
          y = 1.15,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        xaxis = xaxis_cfg,
        yaxis = list(title = list(text = "Score", font = list(size = 14)), range = c(-0.1, 1.1)),
        margin = list(t = 60, r = 20, l = 60, b = 50)
      )
    
  })
}