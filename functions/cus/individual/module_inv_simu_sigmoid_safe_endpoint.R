module_UI_inv_simu_sigmoid_safe_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial values come from the store, so a re-inserted block shows the current value
  v_w <- if (is.null(init)) 1 else init$weight
  v_s <- if (is.null(init)) 0 else init$slope
  v_i <- if (is.null(init)) 0 else init$intercept
  tagList(
    h6(paste0("Settings for Safety Endpoint ", index, ":")),
    fluidRow(
      column(
        width = 5,
        sliderInput(
          ns("safe_weight"), label = 'Weight',
          value = v_w, min = 0, max = 10, step = 0.5, width = "100%"
        )
      ),
      column(
        width = 7,
        fluidRow(
          column(
            width = 6,
            numericInput(ns("safe_slope"), label = "Slope", value = v_s),
            uiOutput(ns("safe_slope_warning"))
          ),
          column(
            width = 6,
            numericInput(ns("safe_intercept"), label = "Intercept", value = v_i),
            uiOutput(ns("safe_intercept_warning"))
          )
        )
      )
    ),
    hr()
  )
}


module_server_inv_simu_sigmoid_safe_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # --- Weight ---
  observeEvent(debounce(reactive(input$safe_weight), 300)(), {
    all_rv$safe_endpoint_setting$safe_weight[index] <- isolate(input$safe_weight)
  })

  # --- Slope ---
  observeEvent(debounce(reactive(input$safe_slope), 300)(), {
    new_num <- isolate(as.numeric(input$safe_slope))
    warning <- fun_numeric_check_warning(input_value = new_num)
    if (warning$show_warning) {
      showFeedbackWarning(inputId = ns("safe_slope"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("safe_slope"))
      all_rv$safe_endpoint_setting$safe_slope[index] <- new_num
    }
  })

  # --- Intercept ---
  observeEvent(debounce(reactive(input$safe_intercept), 300)(), {
    new_num <- isolate(as.numeric(input$safe_intercept))
    warning <- fun_numeric_check_warning(input_value = new_num)
    if (warning$show_warning) {
      showFeedbackWarning(inputId = ns("safe_intercept"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("safe_intercept"))
      all_rv$safe_endpoint_setting$safe_intercept[index] <- new_num
    }
  })

  # writeback only on external data update, never on user typing
  observeEvent(all_rv$triggers$update_ER_dataset, {
    updateNumericInput(session, "safe_slope", value = all_rv$safe_endpoint_setting$safe_slope[index])
    updateNumericInput(session, "safe_intercept", value = all_rv$safe_endpoint_setting$safe_intercept[index])
  }, ignoreInit = TRUE)
}
