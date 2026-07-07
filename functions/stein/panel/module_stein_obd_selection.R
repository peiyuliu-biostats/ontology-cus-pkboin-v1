# =====================================================================
# STEIN OBD Selection tab module (upload mode)
# ---------------------------------------------------------------------
# No input of its own -- reads the same validated stein_rv$trial_data
# $cohort_data as Trial Conduct (single shared upload source), aggregates
# it to per-dose totals, and runs the existing stein_select_obd() engine
# (unchanged -- the same function simulate-mode operating-characteristics
# uses internally for each replicate) to produce the final OBD.
# =====================================================================

module_UI_stein_obd_selection <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status_note")),
    tags$hr(),
    tags$h4("Toxicity: observed vs isotonic (PAVA)"),
    plotOutput(ns("tox_plot"), height = "280px"),
    tags$h4("Efficacy: observed vs unimodal + AIC model average"),
    plotOutput(ns("eff_plot"), height = "280px"),
    tags$h4("Utility and final OBD"),
    plotOutput(ns("utility_plot"), height = "280px"),
    DT::DTOutput(ns("summary_table"))
  )
}

module_server_stein_obd_selection <- function(input, output, session, stein_rv) {

  output$status_note <- renderUI({
    cd <- stein_rv$trial_data$cohort_data
    if (is.null(cd) || nrow(cd) == 0) {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u25b6 Upload trial data in the sidebar (Settings panel) to see the final OBD selection here.")
    } else {
      tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
             "\u2713 Computed from the uploaded trial data.")
    }
  })

  obd_result <- reactive({
    cd <- stein_rv$trial_data$cohort_data
    req(!is.null(cd), nrow(cd) > 0)
    D <- stein_rv$trial_setting$n_dose
    d <- stein_rv$design_setting
    req(d$phi0, d$psi1, d$psi2, d$phi1, d$phi2)
    req(all(cd$dose >= 1 & cd$dose <= D))

    obs <- data.frame(dose = 1:D, n = 0L, n_dlt = 0L, n_eff = 0L)
    for (i in seq_len(nrow(cd))) {
      j <- cd$dose[i]
      obs$n[j]     <- obs$n[j] + cd$n[i]
      obs$n_dlt[j] <- obs$n_dlt[j] + cd$n_dlt[i]
      obs$n_eff[j] <- obs$n_eff[j] + cd$n_eff[i]
    }
    design <- list(phi0 = d$phi0, psi1 = d$psi1, psi2 = d$psi2,
                   phi1 = d$phi1, phi2 = d$phi2,
                   w1 = d$w1, w2 = d$w2, CT = d$CT, CE = d$CE)
    stein_select_obd(obs, design)
  })

  output$tox_plot <- renderPlot({
    res <- obd_result()
    s <- res$summary
    req(nrow(s) > 0)
    df <- data.frame(
      dose = rep(s$dose, 2), value = c(s$p_hat, s$p_tilde),
      type = rep(c("observed p\u0302", "isotonic p\u0303"), each = nrow(s))
    )
    ggplot2::ggplot(df, ggplot2::aes(dose, value, colour = type)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::scale_colour_manual(values = c("observed p\u0302" = "#E24B4A", "isotonic p\u0303" = "#993C1D")) +
      ggplot2::labs(x = "Dose level", y = "Toxicity probability", colour = NULL) +
      ggplot2::ylim(0, 1) + ggplot2::theme_minimal()
  })

  output$eff_plot <- renderPlot({
    res <- obd_result()
    s <- res$summary
    req(nrow(s) > 0)
    df <- data.frame(
      dose = rep(s$dose, 2), value = c(s$q_hat, s$q_tilde),
      type = rep(c("observed q\u0302", "model-avg q\u0303"), each = nrow(s))
    )
    ggplot2::ggplot(df, ggplot2::aes(dose, value, colour = type)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::scale_colour_manual(values = c("observed q\u0302" = "#1D9E75", "model-avg q\u0303" = "#085041")) +
      ggplot2::labs(x = "Dose level", y = "Efficacy probability", colour = NULL) +
      ggplot2::ylim(0, 1) + ggplot2::theme_minimal()
  })

  output$utility_plot <- renderPlot({
    res <- obd_result()
    s <- res$summary
    req(nrow(s) > 0)
    s$is_obd <- !is.na(res$obd) & s$dose == res$obd
    ggplot2::ggplot(s, ggplot2::aes(factor(dose), utility, fill = is_obd)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c(`TRUE` = "#7F77DD", `FALSE` = "#CCCCCC"), guide = "none") +
      ggplot2::labs(x = "Dose level", y = "Utility U",
                    title = if (!is.na(res$obd)) sprintf("Final OBD: dose %d", res$obd) else "No OBD selected (all doses eliminated)") +
      ggplot2::theme_minimal()
  })

  output$summary_table <- DT::renderDT({
    res <- obd_result()
    s <- res$summary
    req(nrow(s) > 0)
    s$is_obd <- ifelse(!is.na(res$obd) & s$dose == res$obd, "\u2605", "")
    disp <- data.frame(
      dose       = s$dose,
      n          = s$n,
      p_hat      = stein_fmt_num(s$p_hat),
      p_tilde    = stein_fmt_num(s$p_tilde),
      q_hat      = stein_fmt_num(s$q_hat),
      q_tilde    = stein_fmt_num(s$q_tilde),
      utility    = stein_fmt_num(s$utility),
      eliminated = s$eliminated,
      is_obd     = s$is_obd
    )
    DT::datatable(disp, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
}
