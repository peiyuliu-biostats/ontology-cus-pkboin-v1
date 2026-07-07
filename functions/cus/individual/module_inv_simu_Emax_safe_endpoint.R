# =========================================================
#   Module: Simulation Mode - Safety Endpoint Setting (Emax)
# =========================================================

module_UI_inv_simu_Emax_safe_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial values come from the store, so a re-inserted block keeps its value
  v_w <- if (is.null(init)) 1 else init$weight
  v_b <- if (is.null(init)) 0 else init$baseline
  v_e <- if (is.null(init)) 1 else init$Emax
  v_c <- if (is.null(init)) 1 else init$EC50
  v_h <- if (is.null(init)) 1 else init$hill

  tagList(
    h6(paste0("Settings for Safety Endpoint ", index, ":")),

    fluidRow(
      column(
        width = 5,
        sliderInput(ns("safe_weight"), label = "Weight",
                    value = v_w, min = 0, max = 10, step = 0.5, width = "100%")
      ),
      column(
        width = 7,
        fluidRow(
          column(width = 6, numericInput(ns("safe_baseline"), "Baseline", value = v_b, step = 0.1)),
          column(width = 6, numericInput(ns("safe_Emax"), "Emax", value = v_e, step = 0.1))
        ),
        fluidRow(
          column(width = 6, numericInput(ns("safe_EC50"), "EC50", value = v_c, step = 0.1)),
          column(width = 6, numericInput(ns("safe_hill"), "Hill", value = v_h, step = 0.1))
        )
      )
    ),
    hr()
  )
}


module_server_inv_simu_Emax_safe_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # initialize once, without overwriting values the user already typed
  observeEvent(TRUE, {
    if (is.null(input$safe_weight))
      updateSliderInput(session, "safe_weight", value = all_rv$safe_endpoint_setting$safe_weight[index])
    for (p in c("safe_baseline", "safe_Emax", "safe_EC50", "safe_hill")) {
      if (is.null(input[[p]]) || is.na(input[[p]]))
        updateNumericInput(session, p, value = all_rv$safe_endpoint_setting[[p]][index])
    }
  }, once = TRUE)

  # weight (slider)
  observeEvent(debounce(reactive(input$safe_weight), DEBOUNCE_SLIDER)(), {
    all_rv$safe_endpoint_setting$safe_weight[index] <- isolate(input$safe_weight)
  })

  # 4 Emax numeric params (no warnings): unified debounce
  for (param in c("safe_baseline", "safe_Emax", "safe_EC50", "safe_hill")) {
    local({
      p <- param  # capture the current value of param
      observeEvent(debounce(reactive(input[[p]]), DEBOUNCE_NUM)(), {
        all_rv$safe_endpoint_setting[[p]][index] <- isolate(as.numeric(input[[p]]))
      })
    })
  }

  # good writeback: external event only
  observeEvent(all_rv$triggers$update_endpoint_Emax_trigger, {
    updateSliderInput(session, "safe_weight", value = all_rv$safe_endpoint_setting$safe_weight[index])
    updateNumericInput(session, "safe_baseline", value = all_rv$safe_endpoint_setting$safe_baseline[index])
    updateNumericInput(session, "safe_Emax",     value = all_rv$safe_endpoint_setting$safe_Emax[index])
    updateNumericInput(session, "safe_EC50",     value = all_rv$safe_endpoint_setting$safe_EC50[index])
    updateNumericInput(session, "safe_hill",     value = all_rv$safe_endpoint_setting$safe_hill[index])
  }, ignoreInit = TRUE)
}
