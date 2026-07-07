module_UI_sidebar_endpoint <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Endpoint Setting"),
    fluidRow(
      column(
        width = 6,
        numericInput(ns("eff_num"), "Efficacy Endpoints:", 1, min = 0, max = 10, width = "100%"),
        uiOutput(ns("eff_num_warning"))
      ),
      column(
        width = 6,
        numericInput(ns("safe_num"), "Safety Endpoints:", 2, min = 0, max = 10, width = "100%"),
        uiOutput(ns("safe_num_warning"))
      )
    ),
    hr(style = "margin-top:8px; margin-bottom:8px;")
  )
}
module_server_sidebar_endpoint <- function(input, output, session, endpoint_num_setting) {
  ns <- session$ns

  observeEvent(TRUE, {
    updateNumericInput(session, "eff_num", value = endpoint_num_setting$eff_num)
    updateNumericInput(session, "safe_num", value = endpoint_num_setting$safe_num)
  }, once = TRUE)

  # --- Update efficacy endpoints ---
  observeEvent(debounce(reactive(input$eff_num), 300)(), {
    new_num <- isolate(as.numeric(input$eff_num))
    warning <- fun_numeric_check_warning(
      input_value = new_num, lower_bound = 0, upper_bound = 10, check_int = TRUE
    )
    if (warning$show_warning) {
      showFeedbackWarning(inputId = ns("eff_num"), text = warning$warning_message)
      # keep last valid count: do not write NA
    } else {
      hideFeedback(ns("eff_num"))
      endpoint_num_setting$eff_num <- new_num
    }
  })

  # --- Update safety endpoints ---
  observeEvent(debounce(reactive(input$safe_num), 300)(), {
    new_num <- isolate(as.numeric(input$safe_num))
    warning <- fun_numeric_check_warning(
      input_value = new_num, lower_bound = 0, upper_bound = 10, check_int = TRUE
    )
    if (warning$show_warning) {
      showFeedbackWarning(inputId = ns("safe_num"), text = warning$warning_message)
      # keep last valid count: do not write NA
    } else {
      hideFeedback(ns("safe_num"))
      endpoint_num_setting$safe_num <- new_num
    }
  })

  # the typing-triggered writebacks were removed; they fought the keyboard and caused jumps
}
