# =====================================================================
# STEIN Data tab module
# ---------------------------------------------------------------------
# Simulation-mode archive. No Run button of its own -- shares the same
# start gate as Operating Characteristics (click "Run simulation"
# there once); after that, this tab auto-updates alongside it
# (debounced ~700ms) as design, trial structure, or scenario values
# change.
#   1. Design & scenario snapshot table
#   2. Truth vs. simulated-result comparison table (core)
#   3. Representative trial trajectory: step-through replay (slider +
#      Prev/Next) with a small highlighted dose-plane plot, plus the
#      full static table beneath for the complete record/download.
#      Step-through only (no animation/auto-play), per design decision.
#   4. Combined CSV download of all of the above
# =====================================================================

module_UI_stein_data <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status_note")),
    tags$hr(),

    tags$h4("Design & scenario snapshot"),
    DT::DTOutput(ns("snapshot_table")),

    tags$h4("Truth vs. simulated result"),
    tags$p(style = "color:#777; font-size:12px;",
           "Compares the assumed true dose-response curves against what the simulation actually selected as OBD, over the configured replications."),
    DT::DTOutput(ns("comparison_table")),

    tags$h4("Representative trial trajectory"),
    tags$p(style = "color:#777; font-size:12px;",
           "One simulated trial. Step through it cohort by cohort below, or view the full record in the table."),
    fluidRow(
      column(2, actionButton(ns("prev_step"), "\u25c0 Prev", width = "100%")),
      column(8, sliderInput(ns("step"), NULL, min = 1, max = 1, value = 1, step = 1, width = "100%")),
      column(2, actionButton(ns("next_step"), "Next \u25b6", width = "100%"))
    ),
    plotOutput(ns("step_plot"), height = "170px"),
    uiOutput(ns("step_detail")),
    tags$br(),
    DT::DTOutput(ns("trajectory_table")),

    tags$hr(),
    downloadButton(ns("dl"), "Download archive (.csv)")
  )
}

module_server_stein_data <- function(input, output, session, stein_rv) {

  output$status_note <- renderUI({
    if (isTRUE(stein_rv$triggers$sim_active)) {
      tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
             "\u2713 Live: archive auto-updates alongside Operating Characteristics as settings change.")
    } else {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u25b6 Click \"Run simulation\" on the Operating Characteristics tab to populate this archive.")
    }
  })

  archive_inputs <- reactive({
    d  <- stein_rv$design_setting
    tr <- stein_rv$trial_setting
    sc <- stein_rv$scenario_setting
    req(length(sc$p_true) == tr$n_dose, length(sc$q_true) == tr$n_dose,
        d$phi0, d$psi1, d$psi2, d$phi1, d$phi2)
    list(
      design = list(phi0 = d$phi0, psi1 = d$psi1, psi2 = d$psi2,
                    phi1 = d$phi1, phi2 = d$phi2,
                    w1 = d$w1, w2 = d$w2, CT = d$CT, CE = d$CE),
      trial  = list(n_dose = tr$n_dose, start_dose = tr$start_dose,
                    cohort_size = tr$cohort_size, n_max = tr$n_max),
      scenario = list(shape = sc$shape, p_true = sc$p_true, q_true = sc$q_true, n_rep = sc$n_rep)
    )
  })
  archive_inputs_d <- debounce(archive_inputs, 700)

  archive <- reactive({
    req(isTRUE(stein_rv$triggers$sim_active))
    ins <- archive_inputs_d()
    b <- stein_boundaries(ins$design$phi0, ins$design$psi1, ins$design$psi2,
                          phi1 = ins$design$phi1, phi2 = ins$design$phi2)
    oc <- stein_operating_char(ins$scenario$p_true, ins$scenario$q_true,
                               ins$design, ins$trial, n_rep = ins$scenario$n_rep)
    true_obd <- stein_true_obd(ins$scenario$p_true, ins$scenario$q_true,
                               list(phi0 = ins$design$phi0, psi1 = ins$design$psi1,
                                    w1 = ins$design$w1, w2 = ins$design$w2), b)
    traj <- stein_one_trial_traj(ins$scenario$p_true, ins$scenario$q_true,
                                 ins$design, ins$trial, b)
    list(design = ins$design, trial = ins$trial, scenario = ins$scenario, bounds = b,
         oc = oc, true_obd = true_obd, traj = traj$trajectory, traj_obd = traj$obd)
  })

  # ---- 1. snapshot table ----
  snapshot_df <- reactive({
    a <- archive()
    data.frame(
      Parameter = c("phi0 (target toxicity)", "phi1 (lower anchor)", "phi2 (upper anchor)",
                    "psi1 (uninteresting efficacy)", "psi2 (desirable efficacy)",
                    "w1 (toxicity utility weight)", "w2 (extra penalty weight)",
                    "CT (toxicity elim. cutoff)", "CE (efficacy elim. cutoff)",
                    "phiL (escalate boundary)", "phiU (de-escalate boundary)",
                    "psi (efficacy cutoff)",
                    "n_dose", "start_dose", "cohort_size", "n_max",
                    "scenario shape", "n_rep",
                    "this trial's simulated OBD"),
      Value = c(
        sprintf("%.4f", a$design$phi0), sprintf("%.4f", a$design$phi1), sprintf("%.4f", a$design$phi2),
        sprintf("%.4f", a$design$psi1), sprintf("%.4f", a$design$psi2),
        sprintf("%.4f", a$design$w1), sprintf("%.4f", a$design$w2),
        sprintf("%.4f", a$design$CT), sprintf("%.4f", a$design$CE),
        sprintf("%.4f", a$bounds$phiL), sprintf("%.4f", a$bounds$phiU), sprintf("%.4f", a$bounds$psi),
        as.character(a$trial$n_dose), as.character(a$trial$start_dose),
        as.character(a$trial$cohort_size), as.character(a$trial$n_max),
        a$scenario$shape, as.character(a$scenario$n_rep),
        ifelse(is.na(a$traj_obd), "none (stopped early)", as.character(a$traj_obd))
      )
    )
  })

  output$snapshot_table <- DT::renderDT({
    DT::datatable(snapshot_df(), rownames = FALSE, options = list(dom = "t", pageLength = 20, ordering = FALSE))
  })

  # ---- 2. truth vs simulated-result comparison (core) ----
  comparison_df <- reactive({
    a <- archive()
    D <- a$trial$n_dose
    data.frame(
      dose = 1:D,
      p_true = a$scenario$p_true,
      q_true = a$scenario$q_true,
      true_OBD = ifelse(!is.na(a$true_obd) & (1:D) == a$true_obd, "\u2605", ""),
      selection_pct = round(a$oc$selection_pct[paste0("dose", 1:D)], 1),
      mean_alloc = round(a$oc$mean_alloc, 2)
    )
  })

  output$comparison_table <- DT::renderDT({
    DT::datatable(comparison_df(), rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  # ---- 3a. step-through replay (slider + Prev/Next; static, no auto-play) ----
  observeEvent(archive(), {
    n <- nrow(archive()$traj)
    updateSliderInput(session, "step", max = max(n, 1), value = 1)
  })
  observeEvent(input$prev_step, {
    updateSliderInput(session, "step", value = max(1, input$step - 1))
  })
  observeEvent(input$next_step, {
    n <- nrow(archive()$traj)
    updateSliderInput(session, "step", value = min(n, input$step + 1))
  })

  output$step_plot <- renderPlot({
    a <- archive()
    D <- a$trial$n_dose
    idx <- min(max(input$step, 1), nrow(a$traj))
    row <- a$traj[idx, , drop = FALSE]
    df <- data.frame(dose = 1:D, y = 0)
    df$current <- df$dose == row$dose
    ggplot2::ggplot(df, ggplot2::aes(dose, y)) +
      ggplot2::geom_point(ggplot2::aes(size = current, colour = current)) +
      ggplot2::scale_size_manual(values = c(`FALSE` = 4, `TRUE` = 9), guide = "none") +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#7F77DD"), guide = "none") +
      ggplot2::scale_x_continuous(breaks = 1:D) +
      ggplot2::labs(x = "Dose level", y = NULL,
                    title = sprintf("Cohort %d of %d \u2014 currently at dose %d",
                                    row$cohort, nrow(a$traj), row$dose)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
  })

  output$step_detail <- renderUI({
    a <- archive()
    idx <- min(max(input$step, 1), nrow(a$traj))
    row <- a$traj[idx, , drop = FALSE]
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; font-size:13px; color:#333;",
      sprintf("Cohort %d: dose %d \u2192 %d patients (%d DLT, %d responses this cohort; cumulative %d/%d DLT, %d/%d responses at this dose). Decision: %s%s.",
              row$cohort, row$dose, row$cohort_n, row$cohort_dlt, row$cohort_eff,
              row$cum_dlt, row$cum_n, row$cum_eff, row$cum_n, row$decision,
              if (!is.na(row$next_dose)) sprintf(" \u2192 next dose %d", row$next_dose) else "")
    )
  })

  # ---- 3b. full trajectory table (complete record) ----
  output$trajectory_table <- DT::renderDT({
    a <- archive()
    DT::datatable(a$traj, rownames = FALSE, options = list(dom = "t", pageLength = 20, ordering = FALSE))
  })

  # ---- 4. combined download ----
  output$dl <- downloadHandler(
    filename = function() "stein_data_archive.csv",
    content = function(file) {
      a <- archive()
      con <- file(file, open = "w")
      on.exit(close(con))
      writeLines("# Design & scenario snapshot", con)
      utils::write.csv(snapshot_df(), con, row.names = FALSE)
      writeLines("", con)
      writeLines("# Truth vs. simulated result", con)
      utils::write.csv(comparison_df(), con, row.names = FALSE)
      writeLines("", con)
      writeLines("# Representative trial trajectory", con)
      utils::write.csv(a$traj, con, row.names = FALSE)
    }
  )
}
