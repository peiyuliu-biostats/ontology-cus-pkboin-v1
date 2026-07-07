# =========================================================
#   Module: Simulation Mode - Efficacy Endpoint Setting (Emax)
# =========================================================

module_UI_inv_simu_Emax_eff_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial values come from the store, so a re-inserted block keeps its value
  v_w <- if (is.null(init)) 1 else init$weight
  v_b <- if (is.null(init)) 0 else init$baseline
  v_e <- if (is.null(init)) 1 else init$Emax
  v_c <- if (is.null(init)) 1 else init$EC50
  v_h <- if (is.null(init)) 1 else init$hill

  tagList(
    h6(paste0("Settings for Efficacy Endpoint ", index, ":")),

    fluidRow(
      column(
        width = 5,
        sliderInput(ns("eff_weight"), label = "Weight",
                    value = v_w, min = 0, max = 10, step = 0.5, width = "100%")
      ),
      column(
        width = 7,
        fluidRow(
          column(width = 6, numericInput(ns("eff_baseline"), "Baseline", value = v_b, step = 0.1)),
          column(width = 6, numericInput(ns("eff_Emax"), "Emax", value = v_e, step = 0.1))
        ),
        fluidRow(
          column(width = 6, numericInput(ns("eff_EC50"), "EC50", value = v_c, step = 0.1)),
          column(width = 6, numericInput(ns("eff_hill"), "Hill", value = v_h, step = 0.1))
        )
      )
    ),
    hr()
  )
}


module_server_inv_simu_Emax_eff_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # --- Weight ---
  observeEvent(debounce(reactive(input$eff_weight), 300)(), {
    all_rv$eff_endpoint_setting$eff_weight[index] <- isolate(input$eff_weight)
  })

  # --- 4 Emax inputs (no warnings) ---
  for (param in c("eff_baseline", "eff_Emax", "eff_EC50", "eff_hill")) {
    local({
      p <- param  # capture the current param value
      observeEvent(debounce(reactive(input[[p]]), 300)(), {
        all_rv$eff_endpoint_setting[[p]][index] <- isolate(as.numeric(input[[p]]))
      })
    })
  }

  # writeback only on external trigger, never on user typing
  observeEvent(all_rv$triggers$update_endpoint_Emax_trigger, {
    updateSliderInput(session, "eff_weight",  value = all_rv$eff_endpoint_setting$eff_weight[index])
    updateNumericInput(session, "eff_baseline", value = all_rv$eff_endpoint_setting$eff_baseline[index])
    updateNumericInput(session, "eff_Emax",     value = all_rv$eff_endpoint_setting$eff_Emax[index])
    updateNumericInput(session, "eff_EC50",     value = all_rv$eff_endpoint_setting$eff_EC50[index])
    updateNumericInput(session, "eff_hill",     value = all_rv$eff_endpoint_setting$eff_hill[index])
  }, ignoreInit = TRUE)
}
