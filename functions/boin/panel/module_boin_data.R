module_UI_boin_data <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
    uiOutput(ns("status_note")),
    tags$hr(),
    uiOutput(ns("mode_body"))
  )
}

module_server_boin_data <- function(input, output, session, boin_rv) {
  
  ns <- session$ns
  
  output$stage_notice <- renderUI(boin_stage_notice_ui(boin_rv$overall_setting$method))
  
  archive_inputs <- reactive({
    method <- boin_rv$overall_setting$method
    d <- boin_rv$design_setting
    tr <- boin_rv$trial_setting
    sc <- boin_rv$scenario_setting
    pk <- boin_rv$pk_setting
    tt <- boin_rv$tite_setting
    
    req(boin_method_implemented(method))
    req(length(sc$p_true) == tr$n_dose, length(sc$q_true) == tr$n_dose,
        d$phi_T, d$phi_E)
    if (boin_is_pk_method(method)) req(length(sc$r_true) == tr$n_dose)
    
    list(
      method = method,
      design = list(phi_T = d$phi_T, phi_E = d$phi_E, phi1 = d$phi1, phi2 = d$phi2,
                    CT = d$CT, CE = d$CE),
      pk_design = list(r_P = pk$r_P, r_I_mult = pk$r_I_mult, C_P = pk$C_P,
                       CV = pk$CV, g_P = pk$g_P,
                       zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult)),
      tite_design = list(A_T = tt$A_T, A_E = tt$A_E,
                         accrual_rate = tt$accrual_rate,
                         suspend_threshold = tt$suspend_threshold,
                         use_susp = tt$use_susp,
                         accrual_random = tt$accrual_random),
      u = c(d$u1, d$u2, d$u3, d$u4),
      trial = list(n_dose = tr$n_dose, start_dose = tr$start_dose,
                   cohort_size = tr$cohort_size, n_max = tr$n_max),
      scenario = list(shape = sc$shape, p_true = sc$p_true,
                      q_true = sc$q_true, r_true = sc$r_true, n_rep = sc$n_rep)
    )
  })
  
  archive_signature <- reactive({
    ins <- archive_inputs()
    boin_oc_signature(list(
      method = ins$method,
      design = ins$design,
      pk_design = ins$pk_design,
      tite_design = ins$tite_design,
      u = ins$u,
      trial = ins$trial,
      p_true = ins$scenario$p_true,
      q_true = ins$scenario$q_true,
      r_true = ins$scenario$r_true,
      n_rep = ins$scenario$n_rep
    ))
  })
  
  run_is_current <- reactive({
    z <- boin_rv$results$sim
    !is.null(z) && identical(z$signature, archive_signature())
  })
  
  output$status_note <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    mode <- boin_rv$overall_setting$simu_or_not
    method <- boin_rv$overall_setting$method
    
    if (identical(mode, 2L) || identical(mode, 2)) {
      dat <- if (method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
        boin_rv$trial_data$patient_data
      } else {
        boin_rv$trial_data$cohort_data
      }
      if (is.null(dat) || nrow(dat) == 0) {
        tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
               "\u25b6 Upload trial data in the sidebar to populate this archive.")
      } else {
        n_cohort <- length(unique(dat$cohort))
        tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
               sprintf("\u2713 Archive of uploaded trial (%d cohort(s)).", n_cohort))
      }
    } else if (isTRUE(boin_rv$triggers$sim_running)) {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u25b6 Simulation is running. Results will appear here when the OC run finishes.")
    } else if (is.null(boin_rv$results$sim)) {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u25b6 Click \"Run simulation\" on the Operating Characteristics tab to populate this archive.")
    } else if (!run_is_current()) {
      tags$p(style = "color:#a05a00; font-size:12.5px; font-weight:600;",
             "\u26a0 Settings changed after the last OC run. Click \"Run simulation\" again to update this archive.")
    } else {
      tags$p(style = "color:#1D9E75; font-size:12.5px; font-weight:600;",
             "\u2713 Archive is current for the last OC run.")
    }
  })
  
  output$mode_body <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    mode <- boin_rv$overall_setting$simu_or_not
    method <- boin_rv$overall_setting$method
    
    if (identical(mode, 2L) || identical(mode, 2)) {
      tagList(
        tags$h4("Design snapshot"),
        DT::DTOutput(ns("up_snapshot_table")),
        if (method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) tagList(
          tags$h4("Uploaded patient data"),
          DT::DTOutput(ns("up_patient_table"))
        ),
        tags$h4("Uploaded cohort data (as enrolled)"),
        DT::DTOutput(ns("up_cohort_table")),
        tags$h4(if (method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) paste(boin_method_label(method), "replay decision log") else "BOIN12 replay decision log"),
        DT::DTOutput(ns("up_log_table")),
        tags$h4("Final OBD (from uploaded data)"),
        DT::DTOutput(ns("up_obd_table")),
        tags$hr(),
        downloadButton(ns("dl_upload"), "Download archive (.csv)")
      )
    } else {
      tagList(
        tags$h4("Design & scenario snapshot"),
        DT::DTOutput(ns("snapshot_table")),
        tags$h4("Truth vs. simulated result"),
        DT::DTOutput(ns("comparison_table")),
        tags$h4("Representative trial trajectory"),
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
  })

  upload_archive <- reactive({
    mode <- boin_rv$overall_setting$simu_or_not
    req(identical(mode, 2L) || identical(mode, 2))
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
      pk_design <- list(r_P = pk$r_P, r_I_mult = pk$r_I_mult,
                        C_P = pk$C_P, CV = pk$CV, g_P = pk$g_P,
                        zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult))
      tite_design <- list(A_T = tt$A_T, A_E = tt$A_E,
                          accrual_rate = tt$accrual_rate,
                          suspend_threshold = tt$suspend_threshold,
                          use_susp = tt$use_susp,
                          accrual_random = tt$accrual_random)
      rp <- tite_pkboin_replay_uploaded(pd, design, b, pk_design,
                                        tite_design, u, D)
      return(list(method = method, design = design, bounds = b,
                  pk_design = pk_design, tite_design = tite_design,
                  patient_data = pd,
                  cohort_data = boin_rv$trial_data$cohort_data,
                  replay = rp))
    }

    if (identical(method, "PKBOIN-12")) {
      pd <- boin_rv$trial_data$patient_data
      req(!is.null(pd), nrow(pd) > 0)
      pk_design <- list(r_P = pk$r_P, r_I_mult = pk$r_I_mult,
                        C_P = pk$C_P, CV = pk$CV, g_P = pk$g_P,
                        zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult))
      rp <- pkboin_replay_uploaded(pd, design, b, pk_design, u, D)
      return(list(method = method, design = design, bounds = b,
                  pk_design = pk_design, patient_data = pd,
                  cohort_data = boin_rv$trial_data$cohort_data,
                  replay = rp))
    }

    cd <- boin_rv$trial_data$cohort_data
    req(!is.null(cd), nrow(cd) > 0)
    rp <- boin_replay_uploaded(cd, design, b, u, D)
    list(method = method, design = design, bounds = b, pk_design = NULL,
         patient_data = NULL, cohort_data = cd, replay = rp)
  })

  upload_snapshot_df <- reactive({
    a <- upload_archive()
    df <- data.frame(
      Parameter = c("method", "phi_T", "phi_E", "CT", "CE",
                    "lambda_e", "lambda_d", "n_dose"),
      Value = c(boin_method_label(a$method),
                sprintf("%.4f", a$design$phi_T),
                sprintf("%.4f", a$design$phi_E),
                sprintf("%.4f", a$design$CT),
                sprintf("%.4f", a$design$CE),
                sprintf("%.4f", a$bounds$lambda_e),
                sprintf("%.4f", a$bounds$lambda_d),
                as.character(boin_rv$trial_setting$n_dose))
    )
    if (a$method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
      df <- rbind(df, data.frame(
        Parameter = c("r_P", "r_I/r_P", "zeta1", "C_P", "CV", "g_P"),
        Value = c(sprintf("%.2f", a$pk_design$r_P),
                  sprintf("%.3f", a$pk_design$r_I_mult),
                  sprintf("%.2f", a$pk_design$zeta1),
                  sprintf("%.3f", a$pk_design$C_P),
                  sprintf("%.3f", a$pk_design$CV),
                  sprintf("%.3f", a$pk_design$g_P))
      ))
    }
    if (identical(a$method, "TITE-PKBOIN-12")) {
      df <- rbind(df, data.frame(
        Parameter = c("A_T", "A_E", "accrual interval", "suspension cutoff",
                      "use suspension", "random accrual"),
        Value = c(sprintf("%.1f", a$tite_design$A_T),
                  sprintf("%.1f", a$tite_design$A_E),
                  sprintf("%.2f", a$tite_design$accrual_rate),
                  sprintf("%.3f", a$tite_design$suspend_threshold),
                  as.character(a$tite_design$use_susp),
                  as.character(a$tite_design$accrual_random))
      ))
    }
    df
  })

  output$up_snapshot_table <- DT::renderDT({
    DT::datatable(upload_snapshot_df(), rownames = FALSE,
                  options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
  })

  output$up_patient_table <- DT::renderDT({
    a <- upload_archive()
    req(a$method %in% c("PKBOIN-12", "TITE-PKBOIN-12"), !is.null(a$patient_data))
    DT::datatable(a$patient_data, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, ordering = FALSE, scrollX = TRUE))
  })

  output$up_cohort_table <- DT::renderDT({
    a <- upload_archive()
    DT::datatable(a$cohort_data, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, ordering = FALSE, scrollX = TRUE))
  })

  output$up_log_table <- DT::renderDT({
    a <- upload_archive()
    DT::datatable(a$replay$log, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, ordering = FALSE, scrollX = TRUE))
  })

  output$up_obd_table <- DT::renderDT({
    a <- upload_archive()
    res <- if (a$method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
      a$replay$final_obd
    } else {
      boin_select_obd(a$replay$obs, a$design, c(boin_rv$design_setting$u1,
                                                boin_rv$design_setting$u2,
                                                boin_rv$design_setting$u3,
                                                boin_rv$design_setting$u4))
    }
    DT::datatable(res$summary, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, ordering = FALSE, scrollX = TRUE))
  })

  output$dl_upload <- downloadHandler(
    filename = function() {
      sprintf("%s_upload_archive.csv", gsub("[^A-Za-z0-9]+", "_", boin_rv$overall_setting$method))
    },
    content = function(file) {
      a <- upload_archive()
      con <- file(file, open = "wt")
      on.exit(close(con), add = TRUE)
      write_section <- function(title, df) {
        writeLines(paste0("# ", title), con)
        utils::write.table(df, con, sep = ",", row.names = FALSE, col.names = TRUE)
        writeLines("", con)
      }
      write_section("Design snapshot", upload_snapshot_df())
      if (a$method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
        write_section("Uploaded patient data", a$patient_data)
      }
      write_section("Uploaded cohort data", a$cohort_data)
      write_section("Replay log", a$replay$log)
      final_summary <- if (a$method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
        a$replay$final_obd$summary
      } else {
        boin_select_obd(a$replay$obs, a$design, c(boin_rv$design_setting$u1,
                                                  boin_rv$design_setting$u2,
                                                  boin_rv$design_setting$u3,
                                                  boin_rv$design_setting$u4))$summary
      }
      write_section("Final OBD summary", final_summary)
    }
  )

  archive <- reactive({
    z <- boin_rv$results$sim
    req(!is.null(z))
    z
  })
  
  snapshot_df <- reactive({
    a <- archive()
    df <- data.frame(
      Parameter = c("method", "phi_T", "phi_E", "CT", "CE", "lambda_e", "lambda_d",
                    "n_dose", "start_dose", "cohort_size", "n_max",
                    "scenario shape", "n_rep", "this trial's simulated OBD"),
      Value = c(
        boin_method_label(a$method),
        sprintf("%.4f", a$design$phi_T), sprintf("%.4f", a$design$phi_E),
        sprintf("%.4f", a$design$CT), sprintf("%.4f", a$design$CE),
        sprintf("%.4f", a$bounds$lambda_e), sprintf("%.4f", a$bounds$lambda_d),
        as.character(a$trial$n_dose), as.character(a$trial$start_dose),
        as.character(a$trial$cohort_size), as.character(a$trial$n_max),
        a$scenario$shape, as.character(a$scenario$n_rep),
        ifelse(is.na(a$traj_obd), "none (stopped early)", as.character(a$traj_obd))
      )
    )
    
    if (boin_is_pk_method(a$method)) {
      df <- rbind(df, data.frame(
        Parameter = c("r_P", "r_I/r_P", "zeta1", "C_P", "CV", "g_P"),
        Value = c(sprintf("%.2f", a$pk_design$r_P),
                  sprintf("%.3f", a$pk_design$r_I_mult),
                  sprintf("%.2f", a$pk_design$zeta1),
                  sprintf("%.3f", a$pk_design$C_P),
                  sprintf("%.3f", a$pk_design$CV),
                  sprintf("%.3f", a$pk_design$g_P))
      ))
    }
    if (identical(a$method, "TITE-PKBOIN-12")) {
      df <- rbind(df, data.frame(
        Parameter = c("A_T", "A_E", "accrual interval", "suspension cutoff",
                      "use suspension", "random accrual"),
        Value = c(sprintf("%.1f", a$tite_design$A_T),
                  sprintf("%.1f", a$tite_design$A_E),
                  sprintf("%.2f", a$tite_design$accrual_rate),
                  sprintf("%.3f", a$tite_design$suspend_threshold),
                  as.character(a$tite_design$use_susp),
                  as.character(a$tite_design$accrual_random))
      ))
      df <- rbind(df, data.frame(
        Parameter = c("this trial duration (days)", "this trial duration (months)"),
        Value = c(sprintf("%.2f", a$traj_duration),
                  sprintf("%.2f", a$traj_duration / 30))
      ))
    }
    df
  })
  
  output$snapshot_table <- DT::renderDT({
    DT::datatable(snapshot_df(), rownames = FALSE,
                  options = list(dom = "t", pageLength = 25, ordering = FALSE, scrollX = TRUE))
  })
  
  comparison_df <- reactive({
    a <- archive()
    D <- a$trial$n_dose
    df <- data.frame(
      dose = 1:D,
      p_true = a$scenario$p_true,
      q_true = a$scenario$q_true,
      true_OBD = ifelse(!is.na(a$true_obd) & (1:D) == a$true_obd, "\u2605", ""),
      selection_pct = round(a$oc$selection_pct[paste0("dose", 1:D)], 1),
      mean_alloc = round(a$oc$mean_alloc, 2)
    )
    if (boin_is_pk_method(a$method)) {
      df$r_true <- round(a$scenario$r_true, 1)
      df$pk_adequate <- ifelse(a$scenario$r_true > a$pk_design$zeta1, "yes", "no")
      df <- df[, c("dose", "p_true", "q_true", "r_true", "pk_adequate",
                   "true_OBD", "selection_pct", "mean_alloc")]
    }
    df
  })
  
  output$comparison_table <- DT::renderDT({
    DT::datatable(comparison_df(), rownames = FALSE,
                  options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
  })
  
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
                    title = sprintf("Cohort %d of %d - currently at dose %d",
                                    row$cohort, nrow(a$traj), row$dose)) +
      ggplot2::theme_minimal()
  })
  
  output$step_detail <- renderUI({
    a <- archive()
    idx <- min(max(input$step, 1), nrow(a$traj))
    row <- a$traj[idx, , drop = FALSE]
    pk_txt <- ""
    if (boin_is_pk_method(a$method) && "r_hat" %in% names(row)) {
      pk_txt <- sprintf(" r_hat = %.1f, PK adequate = %s, d_star = %s, d_PK_min = %s.",
                        row$r_hat, as.character(row$pk_adequate),
                        as.character(row$d_star), as.character(row$d_pk_min))
    }
    tite_txt <- ""
    if (identical(a$method, "TITE-PKBOIN-12")) {
      tite_txt <- sprintf(" decision time = %.1f days, pending tox/eff at current dose = %s/%s, ESS tox/eff = %s/%s, p*/q* = %.3f/%.3f.",
                          row$decision_time,
                          as.character(row$pending_t_current),
                          as.character(row$pending_e_current),
                          as.character(row$ESS_t_current),
                          as.character(row$ESS_e_current),
                          row$p_star, row$q_star)
    }
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; font-size:13px; color:#333;",
      sprintf("Cohort %d: dose %d -> %d patients (%d toxic, %d efficacious this cohort; cumulative %d/%d toxic, %d/%d efficacious at this dose). Decision: %s%s.%s",
              row$cohort, row$dose, row$cohort_n, row$cohort_tox, row$cohort_eff,
              row$cum_tox, row$cum_n, row$cum_eff, row$cum_n, row$decision,
              if (!is.na(row$next_dose)) sprintf(" -> next dose %d", row$next_dose) else "",
              paste0(pk_txt, tite_txt))
    )
  })
  
  output$trajectory_table <- DT::renderDT({
    DT::datatable(archive()$traj, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, ordering = FALSE, scrollX = TRUE))
  })
}
