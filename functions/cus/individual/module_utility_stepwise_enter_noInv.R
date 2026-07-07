module_UI_utility_stepwise_enter_noInv <- function(id)
{
  ns <- NS(id)

  tagList(
    # provide number of knots
    fluidRow(
      column(
        width = 6,
        numericInput(ns("eff_knot_num"), div(class = "custom-label", "Number of Knots for Efficacy Stepwise Utility Function:"),
                     value = 4, min = 1, max = 20),
        uiOutput(ns("eff_knot_num_warning"))
      ),
      column(
        width = 6,
        numericInput(ns("safe_knot_num"), div(class = "custom-label", "Number of Knots for Safety Stepwise Utility Function:"),
                     value = 4, min = 1, max = 20),
        uiOutput(ns("safe_knot_num_warning"))
      )
    ),
    # enter the knots
    hr(),
    fluidRow(
      # Efficacy
      column(width = 5,
             h6("Enter Efficacy Stepwise Utility Function Parameters:"),
             fluidRow(
               column(width = 6, tags$strong("Measurement")),
               column(width = 6, tags$strong("Score")),
             ),
             # rows are rendered here by renderUI (driven by knot count only)
             uiOutput(ns("eff_rows"))),
      column(width = 2),
      # Safety
      column(width = 5,
             h6("Enter Safety Stepwise Utility Function Parameters:"),
             fluidRow(
               column(width = 6, tags$strong("Measurement")),
               column(width = 6, tags$strong("Score")),
             ),
             uiOutput(ns("safe_rows")))
    ),
    hr()
  )
}

module_server_utility_stepwise_enter_noInv <- function(input, output, session, all_rv)
{
  ns <- session$ns

  # render efficacy rows from the current knot count. depends ONLY on
  # eff_knot_num; current values read with isolate() so editing a cell does not
  # rebuild the rows.
  output$eff_rows <- renderUI({
    n <- all_rv$utility_stepwise_setting$eff_knot_num
    req(!is.null(n), !is.na(n), n %in% 1:20)
    isolate({
      meas <- all_rv$utility_stepwise_setting$eff_measure
      scor <- all_rv$utility_stepwise_setting$eff_score
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

  output$safe_rows <- renderUI({
    m <- all_rv$utility_stepwise_setting$safe_knot_num
    req(!is.null(m), !is.na(m), m %in% 1:20)
    isolate({
      meas <- all_rv$utility_stepwise_setting$safe_measure
      scor <- all_rv$utility_stepwise_setting$safe_score
      rows <- lapply(seq_len(m), function(i) {
        fluidRow(
          column(width = 6, numericInput(ns(paste0("safe_measure_", i)), label = NULL,
                                         value = meas[i], min = 0, max = 1, step = 0.1, width = "100%")),
          column(width = 6, numericInput(ns(paste0("safe_score_", i)), label = NULL,
                                         value = scor[i], min = 0, max = 1, step = 0.1, width = "100%"))
        )
      })
      do.call(tagList, rows)
    })
  })

  # write edited cell values back to the store. observers registered once for the
  # maximum number of knots; req() makes each act only when its input is in range.
  lapply(seq_len(20), function(i) {
    observeEvent(input[[paste0("eff_measure_", i)]], {
      req(i <= all_rv$utility_stepwise_setting$eff_knot_num)
      all_rv$utility_stepwise_setting$eff_measure[i] <- input[[paste0("eff_measure_", i)]]
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    observeEvent(input[[paste0("eff_score_", i)]], {
      req(i <= all_rv$utility_stepwise_setting$eff_knot_num)
      all_rv$utility_stepwise_setting$eff_score[i] <- input[[paste0("eff_score_", i)]]
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    observeEvent(input[[paste0("safe_measure_", i)]], {
      req(i <= all_rv$utility_stepwise_setting$safe_knot_num)
      all_rv$utility_stepwise_setting$safe_measure[i] <- input[[paste0("safe_measure_", i)]]
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    observeEvent(input[[paste0("safe_score_", i)]], {
      req(i <= all_rv$utility_stepwise_setting$safe_knot_num)
      all_rv$utility_stepwise_setting$safe_score[i] <- input[[paste0("safe_score_", i)]]
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  })

  # writeback knot count to inputs only in upload mode, so it never fights typing
  observeEvent(all_rv$utility_stepwise_setting$eff_knot_num, {
    req(all_rv$utility_stepwise_setting$upload_or_not == 1)
    updateNumericInput(session, "eff_knot_num", value = all_rv$utility_stepwise_setting$eff_knot_num)
  }, ignoreInit = TRUE)
  observeEvent(all_rv$utility_stepwise_setting$safe_knot_num, {
    req(all_rv$utility_stepwise_setting$upload_or_not == 1)
    updateNumericInput(session, "safe_knot_num", value = all_rv$utility_stepwise_setting$safe_knot_num)
  }, ignoreInit = TRUE)

  # --- update number of efficacy knots; write only valid values to the store ---
  observeEvent(debounce(reactive(input$eff_knot_num), 300)(), {
    new_num <- isolate(as.numeric(input$eff_knot_num))
    warning <- fun_numeric_check_warning(input_value = new_num, lower_bound = 1, upper_bound = 20, check_int = T)
    if (!is.na(warning$show_warning) & warning$show_warning == TRUE) {
      showFeedbackWarning(inputId = ns("eff_knot_num"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("eff_knot_num"))
      all_rv$utility_stepwise_setting$eff_knot_num <- new_num
    }
  })

  # --- update number of safety knots ---
  observeEvent(debounce(reactive(input$safe_knot_num), 300)(), {
    new_num <- isolate(as.numeric(input$safe_knot_num))
    warning <- fun_numeric_check_warning(input_value = new_num, lower_bound = 1, upper_bound = 20, check_int = T)
    if (!is.na(warning$show_warning) & warning$show_warning == TRUE) {
      showFeedbackWarning(inputId = ns("safe_knot_num"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("safe_knot_num"))
      all_rv$utility_stepwise_setting$safe_knot_num <- new_num
    }
  })
}
