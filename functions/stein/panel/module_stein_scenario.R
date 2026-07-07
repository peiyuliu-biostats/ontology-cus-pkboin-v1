# =====================================================================
# STEIN Scenario tab module (simulate mode)
# ---------------------------------------------------------------------
# Editable per-dose true toxicity / efficacy table + true-curve preview.
# Changing the "Efficacy shape" selector in the sidebar immediately
# reseeds p_true/q_true (no extra click needed). Manual cell edits
# persist until the shape is changed again or the dose count changes.
# Single source of truth: stein_rv$scenario_setting$p_true/q_true --
# both the table render and the edit handler read/write the same
# object, so edits can no longer be silently overwritten by a
# render tied to a different, independent reactive.
# =====================================================================

# seed example true rates for a given shape and dose count
stein_seed_truth <- function(shape, D) {
  tox <- seq(0.05, 0.55, length.out = D)
  eff <- switch(shape,
    increasing = seq(0.10, 0.60, length.out = D),
    plateau    = pmin(seq(0.10, 0.60, length.out = D), 0.45),
    unimodal   = {
      peak <- ceiling(D / 2)
      up <- seq(0.15, 0.55, length.out = peak)
      dn <- seq(0.55, 0.30, length.out = D - peak + 1)[-1]
      c(up, dn)
    },
    constant   = rep(0.40, D)
  )
  data.frame(dose = 1:D, p_true = round(tox, 3), q_true = round(eff[1:D], 3))
}

module_UI_stein_scenario <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Per-dose true rates"),
    helpText("Values are edited directly in the table below and persist. Changing \"Efficacy shape\" in the sidebar immediately reseeds all values from that shape's template (this overwrites any manual edits)."),
    DT::DTOutput(ns("truth_table")),
    tags$p(style = "color:#777; font-size:12px; margin-top:4px;",
           "\u2605 marks the true optimal biological dose (OBD) implied by these truths and the current design parameters."),
    tags$h4("True dose-response curves"),
    plotOutput(ns("truth_plot"), height = "320px")
  )
}

module_server_stein_scenario <- function(input, output, session, stein_rv) {

  # ---- single source of truth: stein_rv$scenario_setting$p_true/q_true ----
  # (a) dose count changed -> reseed (lengths must match D or the rest of
  #     the app breaks; this cannot be left to manual editing).
  observe({
    D <- stein_rv$trial_setting$n_dose
    req(D)
    if (length(stein_rv$scenario_setting$p_true) != D) {
      tb <- stein_seed_truth(stein_rv$scenario_setting$shape, D)
      stein_rv$scenario_setting$p_true <- tb$p_true
      stein_rv$scenario_setting$q_true <- tb$q_true
    }
  })

  # (b) shape changed -> reseed immediately, no extra click required.
  #     Fires only when scenario_setting$shape itself changes (fine-grained
  #     reactiveValues invalidation), so it does not re-fire on unrelated
  #     updates (e.g. manual cell edits to p_true/q_true).
  observeEvent(stein_rv$scenario_setting$shape, {
    D <- stein_rv$trial_setting$n_dose
    req(D)
    tb <- stein_seed_truth(stein_rv$scenario_setting$shape, D)
    stein_rv$scenario_setting$p_true <- tb$p_true
    stein_rv$scenario_setting$q_true <- tb$q_true
  }, ignoreInit = TRUE)

  # true OBD (oracle) under current truths + current design parameters
  true_obd <- reactive({
    D <- stein_rv$trial_setting$n_dose
    p <- stein_rv$scenario_setting$p_true
    q <- stein_rv$scenario_setting$q_true
    req(length(p) == D, length(q) == D)
    d <- stein_rv$design_setting
    req(d$phi0, d$psi1, d$psi2, d$phi1, d$phi2)
    design <- list(phi0 = d$phi0, psi1 = d$psi1, w1 = d$w1, w2 = d$w2)
    b <- stein_boundaries(d$phi0, d$psi1, d$psi2, phi1 = d$phi1, phi2 = d$phi2)
    stein_true_obd(p, q, design, b)
  })

  truth_df <- reactive({
    D <- stein_rv$trial_setting$n_dose
    p <- stein_rv$scenario_setting$p_true
    q <- stein_rv$scenario_setting$q_true
    req(length(p) == D, length(q) == D)
    df <- data.frame(dose = 1:D, p_true = p, q_true = q)
    obd <- tryCatch(true_obd(), error = function(e) NA_integer_)
    df$true_OBD <- ifelse(!is.na(obd) & df$dose == obd, "\u2605", "")
    df
  })

  # renders directly from the single source of truth (stein_rv$scenario_setting),
  # the same object the cell-edit handler below writes into.
  output$truth_table <- DT::renderDT({
    DT::datatable(
      truth_df(), rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 3))),
      options = list(dom = "t", ordering = FALSE)
    )
  }, server = TRUE)

  observeEvent(input$truth_table_cell_edit, {
    info <- input$truth_table_cell_edit
    D <- stein_rv$trial_setting$n_dose
    val <- suppressWarnings(as.numeric(info$value))
    req(!is.na(val), info$row >= 1, info$row <= D)
    # columns (0-indexed, rownames = FALSE): 0=dose, 1=p_true, 2=q_true, 3=true_OBD
    if (info$col == 1) stein_rv$scenario_setting$p_true[info$row] <- val
    if (info$col == 2) stein_rv$scenario_setting$q_true[info$row] <- val
  })

  output$truth_plot <- renderPlot({
    D <- stein_rv$trial_setting$n_dose
    p <- stein_rv$scenario_setting$p_true
    q <- stein_rv$scenario_setting$q_true
    req(length(p) == D, length(q) == D)
    df <- data.frame(
      dose = rep(1:D, 2),
      value = c(p, q),
      type = rep(c("toxicity", "efficacy"), each = D)
    )
    ggplot2::ggplot(df, ggplot2::aes(dose, value, colour = type)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::scale_colour_manual(values = c(toxicity = "#E24B4A", efficacy = "#1D9E75")) +
      ggplot2::labs(x = "Dose level", y = "True probability", colour = NULL,
                    title = "True dose-toxicity / dose-efficacy") +
      ggplot2::ylim(0, 1) + ggplot2::theme_minimal()
  })
}
