module_UI_inv_simu_sigmoid_safe_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial values come from the store, so a re-inserted block shows the current value
  v_w <- if (is.null(init)) 1 else init$weight
  v_s <- if (is.null(init)) 0 else init$slope
  v_i <- if (is.null(init)) 0 else init$intercept
  # simulate-mode continuous response bounds (truncated support). NA -> empty input.
  v_lb <- if (is.null(init) || is.null(init$resp_lb)) NA_real_ else init$resp_lb
  v_ub <- if (is.null(init) || is.null(init$resp_ub)) NA_real_ else init$resp_ub
  # initial visibility: show only for continuous endpoints, so a freshly (re-)inserted
  # block is correct immediately without waiting for the toggle observer to re-fire.
  show_bounds <- !is.null(init) && isTRUE(init$is_cont)
  bounds_style <- if (show_bounds) "" else "display:none;"
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
    # continuous-only response bounds; truncated support applied to predicted Y before
    # the min-max map. initial visibility from init$is_cont; live switches handled by toggle.
    # aligned under Slope/Intercept: empty width-5 spacer + width-7 split into two halves.
    tags$div(id = ns("safe_bounds_row"), style = bounds_style,
      fluidRow(
        column(width = 5),
        column(
          width = 7,
          fluidRow(
            column(width = 6,
                   numericInput(ns("safe_resp_lb"), label = "Response lower bound (optional)",
                                value = v_lb, min = NA, max = NA, width = "100%")),
            column(width = 6,
                   numericInput(ns("safe_resp_ub"), label = "Response upper bound (optional)",
                                value = v_ub, min = NA, max = NA, width = "100%"))
          )
        )
      )
    ),
    hr()
  )
}


module_server_inv_simu_sigmoid_safe_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # is this endpoint continuous under the current type + regression selection?
  safe_is_cont <- function() {
    tv <- all_rv$overall_setting$safe_type_vec
    !is.null(tv) && length(tv) >= index && !is.na(tv[index]) && tv[index] == "cont"
  }

  # show the bounds row only for continuous endpoints (the "S" block is shared by
  # sigmoid + continuous in simulate mode, so we toggle instead of rebuilding)
  observe({
    all_rv$overall_setting$safe_type_vec
    shinyjs::toggle(id = "safe_bounds_row", condition = isTRUE(safe_is_cont()))
  })

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

  # --- Response lower bound (continuous only; truncated support) ---
  observeEvent(debounce(reactive(input$safe_resp_lb), 300)(), {
    lb  <- suppressWarnings(as.numeric(isolate(input$safe_resp_lb)))
    ub  <- all_rv$safe_endpoint_setting$safe_resp_ub[index]
    raw <- isolate(input$safe_resp_lb)
    if (!is.null(raw) && !is.na(raw) && is.na(lb)) {
      showNotification(paste0("Safety endpoint ", index,
                              ": response lower bound is not numeric; ignored."), type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_lb[index] <- NA_real_
    } else if (!is.na(lb) && !is.na(ub) && lb > ub) {
      showNotification(paste0("Safety endpoint ", index,
                              ": lower bound > upper bound; lower ignored."), type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_lb[index] <- NA_real_
    } else {
      all_rv$safe_endpoint_setting$safe_resp_lb[index] <- lb
    }
    all_rv$triggers$update_ER_dataset <- Sys.time()
  }, ignoreInit = TRUE)

  # --- Response upper bound (continuous only; truncated support) ---
  observeEvent(debounce(reactive(input$safe_resp_ub), 300)(), {
    ub  <- suppressWarnings(as.numeric(isolate(input$safe_resp_ub)))
    lb  <- all_rv$safe_endpoint_setting$safe_resp_lb[index]
    raw <- isolate(input$safe_resp_ub)
    if (!is.null(raw) && !is.na(raw) && is.na(ub)) {
      showNotification(paste0("Safety endpoint ", index,
                              ": response upper bound is not numeric; ignored."), type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_ub[index] <- NA_real_
    } else if (!is.na(ub) && !is.na(lb) && ub < lb) {
      showNotification(paste0("Safety endpoint ", index,
                              ": upper bound < lower bound; upper ignored."), type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_ub[index] <- NA_real_
    } else {
      all_rv$safe_endpoint_setting$safe_resp_ub[index] <- ub
    }
    all_rv$triggers$update_ER_dataset <- Sys.time()
  }, ignoreInit = TRUE)

  # writeback only on external data update, never on user typing
  observeEvent(all_rv$triggers$update_ER_dataset, {
    updateNumericInput(session, "safe_slope", value = all_rv$safe_endpoint_setting$safe_slope[index])
    updateNumericInput(session, "safe_intercept", value = all_rv$safe_endpoint_setting$safe_intercept[index])
    updateNumericInput(session, "safe_resp_lb", value = all_rv$safe_endpoint_setting$safe_resp_lb[index])
    updateNumericInput(session, "safe_resp_ub", value = all_rv$safe_endpoint_setting$safe_resp_ub[index])
  }, ignoreInit = TRUE)
}
