# =====================================================================
# STEIN Trial Conduct tab module (upload mode)
# ---------------------------------------------------------------------
# No upload control here -- upload happens once in the sidebar
# (Settings panel), and this tab (plus OBD Selection) reads the same
# validated stein_rv$trial_data$cohort_data. Shows, from the actual
# uploaded data (no simulation):
#   - cumulative per-dose summary
#   - the next-dose recommendation implied by the STEIN rule after the
#     latest cohort
#   - a cohort-by-cohort step-through replay (slider + Prev/Next,
#     static -- no auto-play), mirroring the simulate-mode Data tab's
#     replay widget for a consistent interaction pattern
#   - the dose allocation trajectory actually observed
# =====================================================================

module_UI_stein_conduct <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status_note")),
    tags$hr(),

    tags$h4("Cumulative per-dose summary"),
    DT::DTOutput(ns("summary_table")),
    tags$p(style = "color:#777; font-size:12px; margin-top:2px;",
           "status: \"ok\" = tried, still admissible; \"eliminated\" = ruled out so far by the toxicity or futility elimination rule (Design/Flowchart tabs); \"not tried\" = no cohort enrolled at that dose yet."),

    tags$h4("Next-dose recommendation (after latest cohort)"),
    uiOutput(ns("next_dose_box")),

    tags$h4("Cohort-by-cohort replay"),
    fluidRow(
      column(2, actionButton(ns("prev_step"), "\u25c0 Prev", width = "100%")),
      column(8, sliderInput(ns("step"), NULL, min = 1, max = 1, value = 1, step = 1, width = "100%")),
      column(2, actionButton(ns("next_step"), "Next \u25b6", width = "100%"))
    ),
    plotOutput(ns("step_plot"), height = "170px"),
    uiOutput(ns("step_detail")),

    tags$h4("Dose allocation trajectory"),
    plotOutput(ns("traj_plot"), height = "260px"),

    tags$br(),
    tags$h5("Full replay log"),
    DT::DTOutput(ns("log_table"))
  )
}

module_server_stein_conduct <- function(input, output, session, stein_rv) {

  output$status_note <- renderUI({
    cd <- stein_rv$trial_data$cohort_data
    if (is.null(cd) || nrow(cd) == 0) {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u25b6 Upload trial data in the sidebar (Settings panel) to see results here.")
    } else {
      tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
             sprintf("\u2713 Loaded %d cohort(s) from uploaded data.", nrow(cd)))
    }
  })

  replay <- reactive({
    cd <- stein_rv$trial_data$cohort_data
    req(!is.null(cd), nrow(cd) > 0)
    D <- stein_rv$trial_setting$n_dose
    d <- stein_rv$design_setting
    req(d$phi0, d$psi1, d$psi2, d$phi1, d$phi2)
    req(all(cd$dose >= 1 & cd$dose <= D))
    design <- list(phi0 = d$phi0, psi1 = d$psi1, psi2 = d$psi2,
                   phi1 = d$phi1, phi2 = d$phi2,
                   w1 = d$w1, w2 = d$w2, CT = d$CT, CE = d$CE)
    stein_replay_uploaded(cd, design, D)
  })

  output$summary_table <- DT::renderDT({
    r <- replay()
    df <- data.frame(
      dose  = r$obs$dose,
      n     = r$obs$n,
      n_dlt = r$obs$n_dlt,
      n_eff = r$obs$n_eff,
      p_hat = ifelse(r$obs$n > 0, round(r$obs$n_dlt / r$obs$n, 3), NA),
      q_hat = ifelse(r$obs$n > 0, round(r$obs$n_eff / r$obs$n, 3), NA),
      status = ifelse(r$eliminated, "eliminated", ifelse(r$obs$n > 0, "ok", "not tried"))
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  output$next_dose_box <- renderUI({
    r <- replay()
    last <- r$log[nrow(r$log), ]
    nd_text <- if (is.na(last$next_dose)) {
      "no admissible dose remains \u2014 recommend stopping"
    } else {
      sprintf("next dose: %d", last$next_dose)
    }
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; font-size:13px; color:#333;",
      sprintf("Admissible set A_j = {%s}. Eliminated so far: {%s}. %s.",
              last$admissible_set,
              ifelse(last$eliminated_now == "", "none", last$eliminated_now),
              nd_text)
    )
  })

  observeEvent(replay(), {
    n <- nrow(replay()$log)
    updateSliderInput(session, "step", max = max(n, 1), value = n)
  })
  observeEvent(input$prev_step, {
    updateSliderInput(session, "step", value = max(1, input$step - 1))
  })
  observeEvent(input$next_step, {
    n <- nrow(replay()$log)
    updateSliderInput(session, "step", value = min(n, input$step + 1))
  })

  output$step_plot <- renderPlot({
    r <- replay()
    D <- stein_rv$trial_setting$n_dose
    idx <- min(max(input$step, 1), nrow(r$log))
    row <- r$log[idx, , drop = FALSE]
    df <- data.frame(dose = 1:D, y = 0)
    df$current <- df$dose == row$dose
    ggplot2::ggplot(df, ggplot2::aes(dose, y)) +
      ggplot2::geom_point(ggplot2::aes(size = current, colour = current)) +
      ggplot2::scale_size_manual(values = c(`FALSE` = 4, `TRUE` = 9), guide = "none") +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#7F77DD"), guide = "none") +
      ggplot2::scale_x_continuous(breaks = 1:D) +
      ggplot2::labs(x = "Dose level", y = NULL,
                    title = sprintf("Cohort %d of %d \u2014 currently at dose %d",
                                    row$cohort, nrow(r$log), row$dose)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
  })

  output$step_detail <- renderUI({
    r <- replay()
    idx <- min(max(input$step, 1), nrow(r$log))
    row <- r$log[idx, , drop = FALSE]
    nd_text <- if (is.na(row$next_dose)) "no admissible dose" else sprintf("next dose %d", row$next_dose)
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; font-size:13px; color:#333;",
      sprintf("Cohort %d: dose %d \u2192 %d patients (%d DLT, %d responses this cohort; cumulative %d/%d DLT, %d/%d responses at this dose). Admissible set {%s}, recommendation: %s.",
              row$cohort, row$dose, row$cohort_n, row$cohort_dlt, row$cohort_eff,
              row$cum_dlt, row$cum_n, row$cum_eff, row$cum_n,
              row$admissible_set, nd_text)
    )
  })

  output$traj_plot <- renderPlot({
    r <- replay()
    D <- stein_rv$trial_setting$n_dose
    ggplot2::ggplot(r$log, ggplot2::aes(cohort, dose)) +
      ggplot2::geom_step(colour = "#AFA9EC") +
      ggplot2::geom_point(size = 2, colour = "#7F77DD") +
      ggplot2::scale_y_continuous(breaks = 1:D, limits = c(1, D)) +
      ggplot2::labs(x = "Cohort", y = "Dose level", title = "Dose allocation over cohorts") +
      ggplot2::theme_minimal()
  })

  output$log_table <- DT::renderDT({
    DT::datatable(replay()$log, rownames = FALSE, options = list(dom = "t", pageLength = 20, ordering = FALSE))
  })
}
