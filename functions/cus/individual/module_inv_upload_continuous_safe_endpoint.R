# =========================================================
#   Module: Upload Mode - Safety Endpoint Setting (Continuous)
#   linear / log-linear / exponential; shows fitted intercept/slope
# =========================================================

module_UI_inv_upload_continuous_safe_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  v_w <- if (is.null(init)) 1 else init$weight
  v_lb <- if (is.null(init) || is.null(init$resp_lb)) NA_real_ else init$resp_lb
  v_ub <- if (is.null(init) || is.null(init$resp_ub)) NA_real_ else init$resp_ub
  tagList(
    h6(paste0("Settings for Safety Endpoint ", index, ":")),
    fluidRow(
      column(
        width = 5,
        sliderInput(ns("safe_weight"), label = 'Weight',
                    value = v_w, min = 0, max = 10, step = 0.5, width = "100%")
      ),
      column(width = 7,
             fluidRow(
               column(width = 6, uiOutput(ns("safe_slope_est_ui"))),
               column(width = 6, uiOutput(ns("safe_intercept_est_ui")))
             )
      )
    ),
    fluidRow(
      column(width = 6,
             numericInput(ns("safe_resp_lb"), label = "Response lower bound (optional)",
                          value = v_lb, min = NA, max = NA, width = "100%")),
      column(width = 6,
             numericInput(ns("safe_resp_ub"), label = "Response upper bound (optional)",
                          value = v_ub, min = NA, max = NA, width = "100%"))
    ),
    hr()
  )
}

module_server_inv_upload_continuous_safe_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  cont_ab <- function() {
    s <- all_rv$safe_endpoint_setting
    switch(as.character(all_rv$overall_setting$safe_cont_model),
           "3" = c(s$safe_lin_a[index], s$safe_lin_b[index]),
           "4" = c(s$safe_log_a[index], s$safe_log_b[index]),
           "5" = c(s$safe_exp_a[index], s$safe_exp_b[index]),
           c(NA, NA))
  }

  output$safe_slope_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Slope = <br>", round(cont_ab()[2], 2))))
  })
  output$safe_intercept_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Intercept = <br>", round(cont_ab()[1], 2))))
  })

  observeEvent(debounce(reactive(input$safe_weight), 300)(), {
    all_rv$safe_endpoint_setting$safe_weight[index] <- isolate(input$safe_weight)
  })

  # validate + store response bounds (same rule as efficacy)
  observeEvent(debounce(reactive(input$safe_resp_lb), 300)(), {
    lb <- suppressWarnings(as.numeric(isolate(input$safe_resp_lb)))
    ub <- all_rv$safe_endpoint_setting$safe_resp_ub[index]
    raw <- isolate(input$safe_resp_lb)
    if (!is.null(raw) && !is.na(raw) && is.na(lb)) {
      showNotification(paste0("Safety endpoint ", index,
                              ": response lower bound is not numeric; ignored."),
                       type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_lb[index] <- NA_real_
    } else if (!is.na(lb) && !is.na(ub) && lb > ub) {
      showNotification(paste0("Safety endpoint ", index,
                              ": lower bound > upper bound; lower ignored."),
                       type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_lb[index] <- NA_real_
    } else {
      all_rv$safe_endpoint_setting$safe_resp_lb[index] <- lb
    }
    all_rv$triggers$update_ER_dataset <- Sys.time()
  }, ignoreInit = TRUE)

  observeEvent(debounce(reactive(input$safe_resp_ub), 300)(), {
    ub <- suppressWarnings(as.numeric(isolate(input$safe_resp_ub)))
    lb <- all_rv$safe_endpoint_setting$safe_resp_lb[index]
    raw <- isolate(input$safe_resp_ub)
    if (!is.null(raw) && !is.na(raw) && is.na(ub)) {
      showNotification(paste0("Safety endpoint ", index,
                              ": response upper bound is not numeric; ignored."),
                       type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_ub[index] <- NA_real_
    } else if (!is.na(ub) && !is.na(lb) && ub < lb) {
      showNotification(paste0("Safety endpoint ", index,
                              ": upper bound < lower bound; upper ignored."),
                       type = "warning")
      all_rv$safe_endpoint_setting$safe_resp_ub[index] <- NA_real_
    } else {
      all_rv$safe_endpoint_setting$safe_resp_ub[index] <- ub
    }
    all_rv$triggers$update_ER_dataset <- Sys.time()
  }, ignoreInit = TRUE)
}
