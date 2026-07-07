module_UI_inv_utility_stepwise_enter_eff <- function(id, index)
{
  ns <- NS(id)
  tagList(
    h6(paste0("For Efficacy Endpoint ", index, ":")),
    fluidRow(
      column(width = 5,
             div(
               style = "padding-left: 5px; padding-right: 10px;",
               numericInput(ns("eff_knot_num"),
                            label = paste0("Number of Knots for Efficacy_V", index, " Stepwise Utility Function:"),
                            value = 4, min = 1, max = 20),
               uiOutput(ns("eff_knot_num_warning")),
               fluidRow(
                 column(width = 6, tags$strong("Measurement")),
                 column(width = 6, tags$strong("Score")),
               ),
               # rows are rendered here by renderUI (driven by knot count only)
               uiOutput(ns("eff_rows"))
             )
      ),
      column(width = 6,
             plotlyOutput(ns("stepwise_eff_figure"), width = "100%"))
    ),
    hr()
  )
}

module_server_inv_utility_stepwise_enter_eff <- function(input, output, session, index, all_rv)
{
  ns <- session$ns

  # render the measurement/score rows from the current knot count.
  # depends ONLY on eff_knot_num (the row count); the current measure/score
  # values are read with isolate() so editing a cell does NOT rebuild the rows
  # (avoids the flicker that a value-dependent renderUI would cause).
  output$eff_rows <- renderUI({
    n <- all_rv$individual_utility_stepwise_setting$eff_knot_num[index]
    req(!is.null(n), !is.na(n), n %in% 1:20)
    isolate({
      meas <- all_rv$individual_utility_stepwise_setting$eff_measure[[index]]
      scor <- all_rv$individual_utility_stepwise_setting$eff_score[[index]]
      rows <- lapply(seq_len(n), function(i) {
        fluidRow(
          column(width = 6, numericInput(ns(paste0("eff_measure_", i)), label = NULL,
                                         value = meas[i], min = 0, max = 1, step = 0.1, width = "100%")),
          column(width = 6, numericInput(ns(paste0("eff_score_", i)), label = NULL,
                                         value = scor[i], min = 0, max = 1, step = 0.1, width = "100%"))
        )
      })
      do.call(tagList, rows)
    })
  })

  # write edited cell values back to the store. observers are registered once,
  # for the maximum possible number of knots; req() makes each one act only when
  # its input actually exists / is in range. keeps the data flow into all_rv
  # identical to before (per-cell writeback), without depending on insertUI timing.
  lapply(seq_len(20), function(i) {
    observeEvent(input[[paste0("eff_measure_", i)]], {
      req(i <= all_rv$individual_utility_stepwise_setting$eff_knot_num[index])
      tmp <- all_rv$individual_utility_stepwise_setting$eff_measure
      tmp[[index]][i] <- input[[paste0("eff_measure_", i)]]
      all_rv$individual_utility_stepwise_setting$eff_measure <- tmp
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    observeEvent(input[[paste0("eff_score_", i)]], {
      req(i <= all_rv$individual_utility_stepwise_setting$eff_knot_num[index])
      tmp <- all_rv$individual_utility_stepwise_setting$eff_score
      tmp[[index]][i] <- input[[paste0("eff_score_", i)]]
      all_rv$individual_utility_stepwise_setting$eff_score <- tmp
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  })

  # validate the knot count; write only valid values to the store
  observeEvent(debounce(reactive(input$eff_knot_num), 300)(), {
    new_num <- isolate(as.numeric(input$eff_knot_num))
    warning <- fun_numeric_check_warning(input_value = new_num, lower_bound = 1, upper_bound = 20, check_int = T)
    if (warning$show_warning == TRUE) {
      showFeedbackWarning(inputId = ns("eff_knot_num"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("eff_knot_num"))
      all_rv$individual_utility_stepwise_setting$eff_knot_num[index] <- new_num
    }
  })

  # Stepwise Figure for Efficacy
  output$stepwise_eff_figure <- renderPlotly({
    eff_knot_num <- all_rv$individual_utility_stepwise_setting$eff_knot_num[index]
    req(!is.na(eff_knot_num), eff_knot_num %in% 1:20)

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
          x = 0.5, y = 1.15, xanchor = "center", yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        xaxis = xaxis_cfg,
        yaxis = list(title = list(text = "Score", font = list(size = 14)), range = c(-0.1, 1.1)),
        margin = list(t = 60, r = 20, l = 60, b = 50)
      )
  })
}
