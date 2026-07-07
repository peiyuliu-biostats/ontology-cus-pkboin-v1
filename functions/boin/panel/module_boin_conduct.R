# =====================================================================
# BOIN-12 Trial Conduct tab module (upload mode)
# ---------------------------------------------------------------------
# Mirrors functions/stein/panel/module_stein_conduct.R. No upload
# control here -- upload happens once in the sidebar; reads the same
# validated boin_rv$trial_data$cohort_data as OBD Determination.
# =====================================================================

module_UI_boin_conduct <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
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

module_server_boin_conduct <- function(input, output, session, boin_rv) {

  output$stage_notice <- renderUI(boin_stage_notice_ui(boin_rv$overall_setting$method))

  output$status_note <- renderUI({
    method <- boin_rv$overall_setting$method
    req(method %in% c("BOIN12", "PKBOIN-12", "TITE-PKBOIN-12"))
    dat <- if (method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
      boin_rv$trial_data$patient_data
    } else {
      boin_rv$trial_data$cohort_data
    }
    if (is.null(dat) || nrow(dat) == 0) {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u25b6 Upload trial data in the sidebar (Settings panel) to see results here.")
    } else {
      n_cohort <- length(unique(dat$cohort))
      tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
             sprintf("\u2713 Loaded %d cohort(s) from uploaded data.", n_cohort))
    }
  })

  replay <- reactive({
    method <- boin_rv$overall_setting$method
    req(method %in% c("BOIN12", "PKBOIN-12", "TITE-PKBOIN-12"))
    D <- boin_rv$trial_setting$n_dose
    d <- boin_rv$design_setting
    pk <- boin_rv$pk_setting
    tt <- boin_rv$tite_setting
    req(d$phi_T, d$phi_E, d$CT, d$CE)
    design <- list(phi_T = d$phi_T, phi_E = d$phi_E, CT = d$CT, CE = d$CE)
    b <- boin_boundaries(d$phi_T, phi1 = d$phi1, phi2 = d$phi2)
    u <- c(d$u1, d$u2, d$u3, d$u4)
    if (identical(method, "TITE-PKBOIN-12")) {
      pd <- boin_rv$trial_data$patient_data
      req(!is.null(pd), nrow(pd) > 0)
      req(all(pd$dose >= 1 & pd$dose <= D))
      pk_design <- list(r_P = pk$r_P, r_I_mult = pk$r_I_mult,
                        C_P = pk$C_P, CV = pk$CV, g_P = pk$g_P,
                        zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult))
      tite_design <- list(A_T = tt$A_T, A_E = tt$A_E,
                          accrual_rate = tt$accrual_rate,
                          suspend_threshold = tt$suspend_threshold,
                          use_susp = tt$use_susp,
                          accrual_random = tt$accrual_random)
      return(tite_pkboin_replay_uploaded(pd, design, b, pk_design,
                                         tite_design, u, D))
    }
    if (identical(method, "PKBOIN-12")) {
      pd <- boin_rv$trial_data$patient_data
      req(!is.null(pd), nrow(pd) > 0)
      req(all(pd$dose >= 1 & pd$dose <= D))
      pk_design <- list(r_P = pk$r_P, r_I_mult = pk$r_I_mult,
                        C_P = pk$C_P, CV = pk$CV, g_P = pk$g_P,
                        zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult))
      return(pkboin_replay_uploaded(pd, design, b, pk_design, u, D))
    }
    cd <- boin_rv$trial_data$cohort_data
    req(!is.null(cd), nrow(cd) > 0)
    req(all(cd$dose >= 1 & cd$dose <= D))
    boin_replay_uploaded(cd, design, b, u, D)
  })

  output$summary_table <- DT::renderDT({
    r <- replay()
    d <- boin_rv$design_setting
    u <- c(d$u1, d$u2, d$u3, d$u4)
    EU <- vapply(seq_len(nrow(r$obs)), function(i) {
      if (r$obs$n[i] > 0)
        boin_utility_posterior_mean(r$obs$n1[i], r$obs$n2[i], r$obs$n3[i], r$obs$n4[i], u)
      else NA_real_
    }, numeric(1))
    df <- data.frame(
      dose  = r$obs$dose,
      n     = r$obs$n,
      p_hat = ifelse(r$obs$n > 0, round((r$obs$n2 + r$obs$n4) / r$obs$n, 3), NA),
      q_hat = ifelse(r$obs$n > 0, round((r$obs$n1 + r$obs$n2) / r$obs$n, 3), NA),
      EU_d  = ifelse(r$obs$n > 0, round(EU, 2), NA),
      status = ifelse(r$eliminated, "tox/eff eliminated", ifelse(r$obs$n > 0, "ok", "not tried"))
    )
    if (!is.null(r$obs_pk)) {
      df$r_hat <- ifelse(is.na(r$obs_pk), NA_real_, round(r$obs_pk, 2))
      df$pk_n <- r$pk_n
      df$pk_eliminated <- r$pk_eliminated
      df$status <- ifelse(r$pk_eliminated, "PK eliminated", df$status)
    }
    if (identical(boin_rv$overall_setting$method, "TITE-PKBOIN-12")) {
      last <- r$log[nrow(r$log), ]
      df$pending_t_last <- NA_integer_
      df$pending_e_last <- NA_integer_
      df$ESS_t_last <- NA_real_
      df$ESS_e_last <- NA_real_
      cur <- if ("actual_dose" %in% names(last)) last$actual_dose else last$dose
      df$pending_t_last[df$dose == cur] <- last$pending_t_current
      df$pending_e_last[df$dose == cur] <- last$pending_e_current
      df$ESS_t_last[df$dose == cur] <- last$ESS_t_current
      df$ESS_e_last[df$dose == cur] <- last$ESS_e_current
    }
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  output$next_dose_box <- renderUI({
    r <- replay()
    last <- r$log[nrow(r$log), ]
    method <- boin_rv$overall_setting$method
    cur_dose <- if ("actual_dose" %in% names(last)) last$actual_dose else last$dose
    next_col <- if ("recommended_next_dose" %in% names(last)) "recommended_next_dose" else "next_dose"
    next_dose <- last[[next_col]]
    admissible <- if ("admissible_after_elim" %in% names(last)) last$admissible_after_elim else last$admissible_set
    eliminated <- if ("pk_eliminated" %in% names(last)) {
      paste(unique(c(strsplit(last$tox_eff_eliminated, ",")[[1]], strsplit(last$pk_eliminated, ",")[[1]])), collapse = ",")
    } else {
      last$eliminated_now
    }
    if (is.na(eliminated) || eliminated == "") eliminated <- "none"
    nd_text <- if (is.na(next_dose)) {
      "no admissible dose remains \u2014 recommend stopping"
    } else {
      sprintf("next dose: %d (%s, highest RDS = %s)", next_dose, last$decision,
              ifelse(is.na(last$rds_next), "\u2014", sprintf("%.3f", last$rds_next)))
    }
    pk_txt <- if (boin_is_pk_method(method) && "r_hat" %in% names(last)) {
      sprintf(" r_hat = %s, PK adequate = %s, d_star = %s, d_PK_min = %s.",
              ifelse(is.na(last$r_hat), "NA", sprintf("%.2f", last$r_hat)),
              as.character(last$pk_adequate),
              as.character(last$d_star), as.character(last$d_pk_min))
    } else ""
    tite_txt <- if (identical(method, "TITE-PKBOIN-12")) {
      sprintf(" decision time = %.2f, pending tox/eff = %s/%s, ESS tox/eff = %s/%s, p*/q* = %.3f/%.3f.",
              last$decision_time, as.character(last$pending_t_current),
              as.character(last$pending_e_current), as.character(last$ESS_t_current),
              as.character(last$ESS_e_current), last$p_star, last$q_star)
    } else ""
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; font-size:13px; color:#333;",
      sprintf("Current dose %d. Admissible set A_j = {%s}, RDS: %s. Eliminated so far: {%s}. %s.%s%s",
              cur_dose, admissible, last$rds_by_dose, eliminated, nd_text, pk_txt, tite_txt)
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
    D <- boin_rv$trial_setting$n_dose
    idx <- min(max(input$step, 1), nrow(r$log))
    row <- r$log[idx, , drop = FALSE]
    df <- data.frame(dose = 1:D, y = 0)
    cur_dose <- if ("actual_dose" %in% names(row)) row$actual_dose else row$dose
    df$current <- df$dose == cur_dose
    ggplot2::ggplot(df, ggplot2::aes(dose, y)) +
      ggplot2::geom_point(ggplot2::aes(size = current, colour = current)) +
      ggplot2::scale_size_manual(values = c(`FALSE` = 4, `TRUE` = 9), guide = "none") +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#7F77DD"), guide = "none") +
      ggplot2::scale_x_continuous(breaks = 1:D) +
      ggplot2::labs(x = "Dose level", y = NULL,
                    title = sprintf("Cohort %d of %d \u2014 currently at dose %d",
                                    row$cohort, nrow(r$log), cur_dose)) +
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
    cur_dose <- if ("actual_dose" %in% names(row)) row$actual_dose else row$dose
    next_col <- if ("recommended_next_dose" %in% names(row)) "recommended_next_dose" else "next_dose"
    next_dose <- row[[next_col]]
    admissible <- if ("admissible_after_elim" %in% names(row)) row$admissible_after_elim else row$admissible_set
    nd_text <- if (is.na(next_dose)) "no admissible dose (stop)" else
      sprintf("%s \u2192 next dose %d", row$decision, next_dose)
    pk_txt <- if ("r_hat" %in% names(row)) {
      sprintf(" r_hat = %s, PK adequate = %s, d_star = %s, d_PK_min = %s.",
              ifelse(is.na(row$r_hat), "NA", sprintf("%.2f", row$r_hat)),
              as.character(row$pk_adequate),
              as.character(row$d_star), as.character(row$d_pk_min))
    } else ""
    tite_txt <- if ("decision_time" %in% names(row)) {
      sprintf(" decision time = %.2f, pending tox/eff = %s/%s, ESS tox/eff = %s/%s, p*/q* = %.3f/%.3f.",
              row$decision_time, as.character(row$pending_t_current),
              as.character(row$pending_e_current), as.character(row$ESS_t_current),
              as.character(row$ESS_e_current), row$p_star, row$q_star)
    } else ""
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; font-size:13px; color:#333;",
      sprintf("Cohort %d: dose %d \u2192 %d patients (%d toxic, %d efficacious this cohort; cumulative %d/%d toxic, %d/%d efficacious at this dose). Admissible {%s} with RDS %s. Decision: %s.%s%s",
              row$cohort, cur_dose, row$cohort_n, row$cohort_tox, row$cohort_eff,
              row$cum_tox, row$cum_n, row$cum_eff, row$cum_n,
              admissible, row$rds_by_dose, nd_text, pk_txt, tite_txt)
    )
  })

  output$traj_plot <- renderPlot({
    r <- replay()
    D <- boin_rv$trial_setting$n_dose
    plot_df <- r$log
    plot_df$dose_plot <- if ("actual_dose" %in% names(plot_df)) plot_df$actual_dose else plot_df$dose
    ggplot2::ggplot(plot_df, ggplot2::aes(cohort, dose_plot)) +
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
