# =====================================================================
# BOIN-12 OBD Determination tab module (upload mode)
# ---------------------------------------------------------------------
# Mirrors functions/stein/panel/module_stein_obd_selection.R. Reads the
# same validated boin_rv$trial_data$cohort_data as Trial Conduct,
# aggregates to per-dose joint-count totals, and runs boin_select_obd()
# (fun_boin_obd.R, stage A5) -- the SAME final-selection function the
# simulate-mode operating characteristics use internally.
# =====================================================================

module_UI_boin_obd <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
    uiOutput(ns("status_note")),
    tags$hr(),
    tags$h4("Toxicity: observed vs isotonic (PAVA)"),
    plotOutput(ns("tox_plot"), height = "280px"),
    tags$h4("Expected utility EU_d per dose (same scale as interim RDS)"),
    plotOutput(ns("utility_plot"), height = "280px"),
    DT::DTOutput(ns("summary_table"))
  )
}

module_server_boin_obd <- function(input, output, session, boin_rv) {

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
             "\u25b6 Upload trial data in the sidebar (Settings panel) to see the final OBD determination here.")
    } else {
      tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
             "\u2713 Computed from the uploaded trial data.")
    }
  })

  obd_result <- reactive({
    method <- boin_rv$overall_setting$method
    req(method %in% c("BOIN12", "PKBOIN-12", "TITE-PKBOIN-12"))
    D <- boin_rv$trial_setting$n_dose
    d <- boin_rv$design_setting
    pk <- boin_rv$pk_setting
    tt <- boin_rv$tite_setting
    req(d$phi_T, d$phi_E, d$CT, d$CE)
    design <- list(phi_T = d$phi_T, phi_E = d$phi_E, CT = d$CT, CE = d$CE)
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
      b <- boin_boundaries(d$phi_T, phi1 = d$phi1, phi2 = d$phi2)
      rp <- tite_pkboin_replay_uploaded(pd, design, b, pk_design,
                                        tite_design, u, D)
      return(rp$final_obd)
    }

    if (identical(method, "PKBOIN-12")) {
      pd <- boin_rv$trial_data$patient_data
      req(!is.null(pd), nrow(pd) > 0)
      req(all(pd$dose >= 1 & pd$dose <= D))
      pk_design <- list(r_P = pk$r_P, r_I_mult = pk$r_I_mult,
                        C_P = pk$C_P, CV = pk$CV, g_P = pk$g_P,
                        zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult))
      b <- boin_boundaries(d$phi_T, phi1 = d$phi1, phi2 = d$phi2)
      rp <- pkboin_replay_uploaded(pd, design, b, pk_design, u, D)
      return(rp$final_obd)
    }

    cd <- boin_rv$trial_data$cohort_data
    req(!is.null(cd), nrow(cd) > 0)
    req(all(cd$dose >= 1 & cd$dose <= D))
    obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
    for (i in seq_len(nrow(cd))) {
      j <- cd$dose[i]
      obs$n[j]  <- obs$n[j]  + cd$n[i]
      obs$n1[j] <- obs$n1[j] + cd$n1[i]
      obs$n2[j] <- obs$n2[j] + cd$n2[i]
      obs$n3[j] <- obs$n3[j] + cd$n3[i]
      obs$n4[j] <- obs$n4[j] + cd$n4[i]
    }
    boin_select_obd(obs, design, u)
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

  output$utility_plot <- renderPlot({
    res <- obd_result()
    s <- res$summary
    req(nrow(s) > 0)
    s$is_obd <- !is.na(res$obd) & s$dose == res$obd
    ggplot2::ggplot(s, ggplot2::aes(factor(dose), utility, fill = is_obd)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c(`TRUE` = "#7F77DD", `FALSE` = "#CCCCCC"), guide = "none") +
      ggplot2::labs(x = "Dose level", y = "Expected utility EU_d",
                    title = if (!is.na(res$obd)) {
                      if (!is.null(res$d_pk_min) && !is.na(res$d_pk_min)) {
                        sprintf("Final OBD: dose %d  (d_MTD = %d, d_PK_min = %d)", res$obd, res$d_mtd, res$d_pk_min)
                      } else {
                        sprintf("Final OBD: dose %d  (MTD anchor d_MTD = %d)", res$obd, res$d_mtd)
                      }
                    } else "No OBD selected (all doses eliminated)") +
      ggplot2::theme_minimal()
  })

  output$summary_table <- DT::renderDT({
    res <- obd_result()
    s <- res$summary
    req(nrow(s) > 0)
    d <- boin_rv$design_setting
    u <- c(d$u1, d$u2, d$u3, d$u4)
    # cumulative RDS per tried dose on the final data (same quasi-beta-
    # binomial desirability used at interim), for interim/final parity.
    D  <- boin_rv$trial_setting$n_dose
    if (boin_rv$overall_setting$method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
      pd <- boin_rv$trial_data$patient_data
      obs <- pkboin_cum_obs_from_patients(pd, D)$obs
    } else {
      cd <- boin_rv$trial_data$cohort_data
      obs <- data.frame(dose = 1:D, n = 0L, n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
      for (i in seq_len(nrow(cd))) {
        j <- cd$dose[i]
        obs$n[j]  <- obs$n[j]  + cd$n[i];  obs$n1[j] <- obs$n1[j] + cd$n1[i]
        obs$n2[j] <- obs$n2[j] + cd$n2[i]; obs$n3[j] <- obs$n3[j] + cd$n3[i]
        obs$n4[j] <- obs$n4[j] + cd$n4[i]
      }
    }
    rds_of <- function(dd) {
      row <- obs[obs$dose == dd, ]
      if (row$n > 0) boin_rds(row$n1, row$n2, row$n3, row$n4, u, d$phi_T, d$phi_E) else NA_real_
    }
    s$is_obd <- ifelse(!is.na(res$obd) & s$dose == res$obd, "\u2605", "")
    disp <- data.frame(
      dose        = s$dose,
      n           = s$n,
      p_hat       = boin_fmt_num(s$p_hat),
      p_tilde     = boin_fmt_num(s$p_tilde),
      q_hat       = boin_fmt_num(s$q_hat),
      EU_d        = boin_fmt_num(s$utility),
      RDS         = boin_fmt_num(vapply(s$dose, rds_of, numeric(1))),
      eliminated  = s$eliminated,
      le_MTD      = s$admissible_final,
      is_obd      = s$is_obd
    )
    if ("r_hat" %in% names(s)) {
      disp$r_hat <- boin_fmt_num(s$r_hat)
      disp$r_tilde <- boin_fmt_num(s$r_tilde)
      disp$pk_eliminated <- s$pk_eliminated
      disp$d_PK_min <- s$d_pk_min
      disp$final_admissible <- s$admissible_final
    }
    DT::datatable(disp, rownames = FALSE, options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
  })
}
