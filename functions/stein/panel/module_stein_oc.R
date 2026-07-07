# =====================================================================
# STEIN Operating Characteristics tab module (simulate mode)
# ---------------------------------------------------------------------
# Click "Run simulation" once to start. After that, results auto-update
# (debounced ~700ms) whenever design, trial structure, or scenario
# truths change -- no need to click Run again. A visible status line
# always shows whether results are live/auto-updating or awaiting the
# first run. A scenario-summary echo at the top shows which truth/
# dose/design combination the results below belong to. The same
# start-gate (stein_rv$triggers$sim_active) is shared with the Data
# tab, which auto-updates alongside this one without its own button.
# =====================================================================

module_UI_stein_oc <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("scenario_echo")),
    actionButton(ns("run"), "Run simulation"),
    uiOutput(ns("status_note")),
    tags$hr(),
    tags$h4("Selection probability & allocation"),
    DT::DTOutput(ns("oc_table")),
    tags$h4("OBD selection probability"),
    plotOutput(ns("sel_plot"), height = "300px"),
    verbatimTextOutput(ns("early_stop"))
  )
}

module_server_stein_oc <- function(input, output, session, stein_rv) {

  # ---- scenario summary echo (top of tab) ----
  output$scenario_echo <- renderUI({
    tr <- stein_rv$trial_setting
    sc <- stein_rv$scenario_setting
    d  <- stein_rv$design_setting
    req(tr$n_dose, sc$shape, length(sc$p_true) == tr$n_dose, length(sc$q_true) == tr$n_dose,
        d$phi0, d$psi1, d$psi2, d$phi1, d$phi2)
    true_obd <- tryCatch({
      b <- stein_boundaries(d$phi0, d$psi1, d$psi2, phi1 = d$phi1, phi2 = d$phi2)
      stein_true_obd(sc$p_true, sc$q_true,
                     list(phi0 = d$phi0, psi1 = d$psi1, w1 = d$w1, w2 = d$w2), b)
    }, error = function(e) NA_integer_)
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; margin-bottom:10px; font-size:13px; color:#333;",
      sprintf("Current scenario: %s efficacy shape, %d doses, start dose %d, cohort size %d, N_max %d, replications %d%s",
              sc$shape, tr$n_dose, tr$start_dose, tr$cohort_size, tr$n_max, sc$n_rep,
              if (!is.na(true_obd)) sprintf(", true OBD = dose %d", true_obd) else "")
    )
  })

  # ---- start/auto-update gate: shared across OC and Data tabs ----
  observeEvent(input$run, {
    stein_rv$triggers$sim_active <- TRUE
  })

  output$status_note <- renderUI({
    if (isTRUE(stein_rv$triggers$sim_active)) {
      tags$p(style = "color:#1D9E75; font-size:12.5px; margin-top:6px; font-weight:600;",
             "\u2713 Live: results auto-update as you change design, trial structure, or scenario values.")
    } else {
      tags$p(style = "color:#a05a00; font-size:12.5px; margin-top:6px; font-weight:600;",
             "\u25b6 Click \"Run simulation\" to see results for the current settings.")
    }
  })

  # ---- inputs feeding the simulation, debounced so dragging/typing
  #      doesn't trigger a re-run on every keystroke ----
  oc_inputs <- reactive({
    d <- stein_rv$design_setting
    tr <- stein_rv$trial_setting
    sc <- stein_rv$scenario_setting
    req(length(sc$p_true) == tr$n_dose, length(sc$q_true) == tr$n_dose)
    list(
      design = list(phi0 = d$phi0, psi1 = d$psi1, psi2 = d$psi2,
                    phi1 = d$phi1, phi2 = d$phi2,
                    w1 = d$w1, w2 = d$w2, CT = d$CT, CE = d$CE),
      trial  = list(n_dose = tr$n_dose, start_dose = tr$start_dose,
                    cohort_size = tr$cohort_size, n_max = tr$n_max),
      p_true = sc$p_true, q_true = sc$q_true, n_rep = sc$n_rep
    )
  })
  oc_inputs_d <- debounce(oc_inputs, 700)

  oc <- reactive({
    req(isTRUE(stein_rv$triggers$sim_active))
    ins <- oc_inputs_d()
    stein_operating_char(ins$p_true, ins$q_true, ins$design, ins$trial, n_rep = ins$n_rep)
  })

  output$oc_table <- DT::renderDT({
    o <- oc()
    D <- stein_rv$trial_setting$n_dose
    df <- data.frame(
      dose = 1:D,
      selection_pct = round(o$selection_pct[paste0("dose", 1:D)], 1),
      mean_alloc    = round(o$mean_alloc, 2)
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  output$sel_plot <- renderPlot({
    o <- oc()
    D <- stein_rv$trial_setting$n_dose
    df <- data.frame(dose = factor(1:D),
                     pct = o$selection_pct[paste0("dose", 1:D)])
    ggplot2::ggplot(df, ggplot2::aes(dose, pct)) +
      ggplot2::geom_col(fill = "#7F77DD") +
      ggplot2::labs(x = "Dose level", y = "Selection %",
                    title = "OBD selection probability") +
      ggplot2::theme_minimal()
  })

  output$early_stop <- renderPrint({
    cat(sprintf("Early stop (no dose selected): %.1f%%\n", oc()$early_stop_pct))
  })
}
