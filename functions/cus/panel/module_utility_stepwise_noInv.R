module_UI_utility_stepwise_noInv <- function(id)
{
  ns <- NS(id)
  tagList(
    # Upload or Enter parameters 
    radioButtons(inputId = ns("stepwise_upload_or_not"), width = "100%",
                 label = div(class = "custom-label", "How would you like to provide stepwise utility function parameters:"), 
                 choices = list("Upload parameter dataset" = 1, 
                                "Enter parameters manually" = 2), 
                 selected = 1),
    hr(),
    # choose to upload the dataset 
    conditionalPanel(
      condition = paste0("input['", ns("stepwise_upload_or_not"), "'] == 1"),
      module_UI_utility_stepwise_upload_noInv(ns("UI_utility_stepwise_upload_noInv"))
    ),
    
    # choose to enter the dataset 
    conditionalPanel(
      condition = paste0("input['", ns("stepwise_upload_or_not"), "'] == 2"),
      module_UI_utility_stepwise_enter_noInv(ns("UI_utility_stepwise_enter_noInv"))
    ),
    # visualize the stepwise function  
    fluidRow(
      column(width = 6,
             plotlyOutput(ns("stepwise_eff_figure"), width = "100%")),
      column(width = 6,
             plotlyOutput(ns("stepwise_safe_figure"), width = "100%"))
    )
  )
}

module_server_utility_stepwise_noInv <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  observeEvent(input$stepwise_upload_or_not, {
    all_rv$utility_stepwise_setting$upload_or_not <- input$stepwise_upload_or_not
  })
  
  # Stepwise Figure for Efficacy
  output$stepwise_eff_figure <- renderPlotly({
    eff_knot_num <- all_rv$utility_stepwise_setting$eff_knot_num
    req(!is.na(eff_knot_num) & eff_knot_num %in% 1:20)

    df <- data.frame(measurement = all_rv$utility_stepwise_setting$eff_measure[1:eff_knot_num],
                     score = all_rv$utility_stepwise_setting$eff_score[1:eff_knot_num]) %>%
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

    plotly::ggplotly(p, width = 400, height = 350) %>%
      layout(
        title = list(
          text = "Utility Component for Efficacy",
          x = 0.5,
          y = 1.15,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        xaxis = list(title = list(text = "Measurement", font = list(size = 14)), range = c(-0.05, 1.05)),
        yaxis = list(title = list(text = "Score", font = list(size = 14)), range = c(-0.1, 1.1)),
        margin = list(t = 60, r = 20, l = 60, b = 50)
      )

  })

  # Stepwise Figure for Safety
  output$stepwise_safe_figure <- renderPlotly({
    safe_knot_num <- all_rv$utility_stepwise_setting$safe_knot_num
    req(!is.na(safe_knot_num) & safe_knot_num %in% 1:20)
    df <- data.frame(measurement = all_rv$utility_stepwise_setting$safe_measure[1:safe_knot_num],
                     score = all_rv$utility_stepwise_setting$safe_score[1:safe_knot_num]) %>%
      na.omit()

    if(!0 %in% df$measurement) { df <- df %>% add_row(measurement = 0, score = max(df$score)) }
    if(!1 %in% df$measurement) { df <- df %>% add_row(measurement = 1, score = min(0, df$score)) }

    df <- df %>% arrange(measurement)

    p <- ggplot(df, aes(x = measurement, y = score)) +
      geom_step(direction = "vh", color = "firebrick", linewidth = 0.6) +
      geom_point(color = "darkorange", fill = "white", shape = 21, size = 2, stroke = 0.6) +
      xlab("Measurement") + ylab("Score") +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.6),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      )

    plotly::ggplotly(p, width = 400, height = 350) %>%
      layout(
        title = list(
          text = "Utility Component for Safety",
          x = 0.5,
          y = 1.15,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        xaxis = list(title = list(text = "Measurement", font = list(size = 14)), range = c(-0.05, 1.05)),
        yaxis = list(title = list(text = "Score", font = list(size = 14)), range = c(-0.1, 1.1)),
        margin = list(t = 60, r = 20, l = 60, b = 50)
      )
  })
  
  callModule(module = module_server_utility_stepwise_upload_noInv, 
             id = "UI_utility_stepwise_upload_noInv", all_rv)
  
  callModule(module = module_server_utility_stepwise_enter_noInv, 
             id = "UI_utility_stepwise_enter_noInv", all_rv) 
}