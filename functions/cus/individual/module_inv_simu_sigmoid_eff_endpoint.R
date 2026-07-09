module_UI_inv_simu_sigmoid_eff_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial values come from the store, so a re-inserted block shows the current
  # value instead of resetting to 0
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
    h6(paste0("Settings for Efficacy Endpoint ", index, ":")),
    fluidRow(
      column(
        width = 5,
        sliderInput(ns("eff_weight"), label = 'Weight',
                    value = v_w, min = 0, max = 10, step = 0.5, width = "100%")
      ),
      column(
        width = 7,
        fluidRow(
          column(
            width = 6,
            numericInput(ns("eff_slope"), label = "Slope", value = v_s),
            uiOutput(ns("eff_slope_warning"))
          ),
          column(
            width = 6,
            numericInput(ns("eff_intercept"), label = "Intercept", value = v_i),
            uiOutput(ns("eff_intercept_warning"))
          )
        )
      )
    ),
    # continuous-only response bounds; truncated support applied to predicted Y before
    # the min-max map. initial visibility from init$is_cont; live switches handled by toggle.
    # aligned under Slope/Intercept: empty width-5 spacer + width-7 split into two halves.
    tags$div(id = ns("eff_bounds_row"), style = bounds_style,
      fluidRow(
        column(width = 5),
        column(
          width = 7,
          fluidRow(
            column(width = 6,
                   numericInput(ns("eff_resp_lb"), label = "Response lower bound (optional)",
                                value = v_lb, min = NA, max = NA, width = "100%")),
            column(width = 6,
                   numericInput(ns("eff_resp_ub"), label = "Response upper bound (optional)",
                                value = v_ub, min = NA, max = NA, width = "100%"))
          )
        )
      )
    ),
    hr()
  )
}

module_server_inv_simu_sigmoid_eff_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # is this endpoint continuous under the current type + regression selection?
  eff_is_cont <- function() {
    tv <- all_rv$overall_setting$eff_type_vec
    !is.null(tv) && length(tv) >= index && !is.na(tv[index]) && tv[index] == "cont"
  }

  # show the bounds row only for continuous endpoints (the "S" block is shared by
  # sigmoid + continuous in simulate mode, so we toggle instead of rebuilding)
  observe({
    all_rv$overall_setting$eff_type_vec
    shinyjs::toggle(id = "eff_bounds_row", condition = isTRUE(eff_is_cont()))
  })

  # --- Weight ---
  observeEvent(debounce(reactive(input$eff_weight), 300)(), {
    all_rv$eff_endpoint_setting$eff_weight[index] <- isolate(input$eff_weight)
  })

  # --- Slope ---
  observeEvent(debounce(reactive(input$eff_slope), 300)(), {
    new_num <- isolate(as.numeric(input$eff_slope))
    warning <- fun_numeric_check_warning(input_value = new_num)
    if (warning$show_warning) {
      showFeedbackWarning(inputId = ns("eff_slope"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("eff_slope"))
      all_rv$eff_endpoint_setting$eff_slope[index] <- new_num
    }
  })

  # --- Intercept ---
  observeEvent(debounce(reactive(input$eff_intercept), 300)(), {
    new_num <- isolate(as.numeric(input$eff_intercept))
    warning <- fun_numeric_check_warning(input_value = new_num)
    if (warning$show_warning) {
      showFeedbackWarning(inputId = ns("eff_intercept"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("eff_intercept"))
      all_rv$eff_endpoint_setting$eff_intercept[index] <- new_num
    }
  })

  # --- Response lower bound (continuous only; truncated support) ---
  # empty -> NA (no truncation on that side); non-numeric or lower > upper -> notify + ignore.
  # only truncation is affected; the CUS core is untouched.
  observeEvent(debounce(reactive(input$eff_resp_lb), 300)(), {
    lb  <- suppressWarnings(as.numeric(isolate(input$eff_resp_lb)))
    ub  <- all_rv$eff_endpoint_setting$eff_resp_ub[index]
    raw <- isolate(input$eff_resp_lb)
    if (!is.null(raw) && !is.na(raw) && is.na(lb)) {
      showNotification(paste0("Efficacy endpoint ", index,
                              ": response lower bound is not numeric; ignored."), type = "warning")
      all_rv$eff_endpoint_setting$eff_resp_lb[index] <- NA_real_
    } else if (!is.na(lb) && !is.na(ub) && lb > ub) {
      showNotification(paste0("Efficacy endpoint ", index,
                              ": lower bound > upper bound; lower ignored."), type = "warning")
      all_rv$eff_endpoint_setting$eff_resp_lb[index] <- NA_real_
    } else {
      all_rv$eff_endpoint_setting$eff_resp_lb[index] <- lb
    }
    all_rv$triggers$update_ER_dataset <- Sys.time()
  }, ignoreInit = TRUE)

  # --- Response upper bound (continuous only; truncated support) ---
  observeEvent(debounce(reactive(input$eff_resp_ub), 300)(), {
    ub  <- suppressWarnings(as.numeric(isolate(input$eff_resp_ub)))
    lb  <- all_rv$eff_endpoint_setting$eff_resp_lb[index]
    raw <- isolate(input$eff_resp_ub)
    if (!is.null(raw) && !is.na(raw) && is.na(ub)) {
      showNotification(paste0("Efficacy endpoint ", index,
                              ": response upper bound is not numeric; ignored."), type = "warning")
      all_rv$eff_endpoint_setting$eff_resp_ub[index] <- NA_real_
    } else if (!is.na(ub) && !is.na(lb) && ub < lb) {
      showNotification(paste0("Efficacy endpoint ", index,
                              ": upper bound < lower bound; upper ignored."), type = "warning")
      all_rv$eff_endpoint_setting$eff_resp_ub[index] <- NA_real_
    } else {
      all_rv$eff_endpoint_setting$eff_resp_ub[index] <- ub
    }
    all_rv$triggers$update_ER_dataset <- Sys.time()
  }, ignoreInit = TRUE)

  # writeback only on external data update, never on user typing
  observeEvent(all_rv$triggers$update_ER_dataset, {
    updateNumericInput(session, "eff_slope", value = all_rv$eff_endpoint_setting$eff_slope[index])
    updateNumericInput(session, "eff_intercept", value = all_rv$eff_endpoint_setting$eff_intercept[index])
    updateNumericInput(session, "eff_resp_lb", value = all_rv$eff_endpoint_setting$eff_resp_lb[index])
    updateNumericInput(session, "eff_resp_ub", value = all_rv$eff_endpoint_setting$eff_resp_ub[index])
  }, ignoreInit = TRUE)
}
