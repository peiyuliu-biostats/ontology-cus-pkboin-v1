# =====================================================================
# STEIN Design tab module
# ---------------------------------------------------------------------
# Shows auto-derived boundaries (phiL, phiU, psi) read-only, in three
# layers: (1) a compact number table, (2) a one-line plain-language
# decision rule, (3) a collapsible formula section (MathJax, hidden by
# default) -- plus the promising / exploratory / inadmissible decision
# region plot (unchanged from prior increment).
# =====================================================================

module_UI_stein_design <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Derived decision boundaries"),
    tags$div(
      style = "font-family:'Courier New', monospace; background:#f6f6f8; padding:10px 14px; border-radius:6px;",
      uiOutput(ns("boundaries_table"))
    ),
    uiOutput(ns("boundaries_rule")),
    tags$details(
      style = "margin-bottom:16px;",
      tags$summary(style = "cursor:pointer; color:#555;", "Show derivation formulas"),
      uiOutput(ns("boundaries_formula"))
    ),
    tags$h4("Decision regions"),
    plotOutput(ns("region_plot"), height = "360px")
  )
}

module_server_stein_design <- function(input, output, session, stein_rv) {

  bounds <- reactive({
    d <- stein_rv$design_setting
    req(d$phi0, d$psi1, d$psi2)
    stein_boundaries(d$phi0, d$psi1, d$psi2,
                     phi1 = d$phi1, phi2 = d$phi2)
  })

  # ---- layer 1: number table ----
  output$boundaries_table <- renderUI({
    b <- bounds()
    tags$table(
      style = "width:100%; border-collapse:collapse; font-size:13px;",
      tags$tr(
        tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Toxicity anchors"),
        tags$td(sprintf("\u03c61 = %.4f    \u03c62 = %.4f", b$phi1, b$phi2))
      ),
      tags$tr(
        tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Toxicity boundaries"),
        tags$td(sprintf("\u03c6L = %.4f    \u03c6U = %.4f", b$phiL, b$phiU))
      ),
      tags$tr(
        tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Efficacy cutoff"),
        tags$td(sprintf("\u03c8 = %.4f", b$psi))
      )
    )
  })

  # ---- layer 2: one-line plain-language rule ----
  output$boundaries_rule <- renderUI({
    b <- bounds()
    tags$p(
      style = "margin:6px 0 14px; color:#333; font-size:13.5px;",
      sprintf(
        "Observed toxicity rate \u2264 %.3f \u2192 escalate; between %.3f and %.3f \u2192 stay; \u2265 %.3f \u2192 de-escalate. Observed efficacy rate \u2265 %.3f is treated as promising.",
        b$phiL, b$phiL, b$phiU, b$phiU, b$psi
      )
    )
  })

  # ---- layer 3: collapsible formulas (MathJax), hidden by default ----
  output$boundaries_formula <- renderUI({
    bounds()  # establish reactive dependency so formulas re-typeset if design changes
    tagList(
      tags$p(style = "margin-top:10px;",
             "$$\\phi_L=\\dfrac{\\log\\dfrac{1-\\phi_1}{1-\\phi_0}}{\\log\\dfrac{\\phi_0(1-\\phi_1)}{\\phi_1(1-\\phi_0)}}$$"),
      tags$p("$$\\phi_U=\\dfrac{\\log\\dfrac{1-\\phi_0}{1-\\phi_2}}{\\log\\dfrac{\\phi_2(1-\\phi_0)}{\\phi_0(1-\\phi_2)}}$$"),
      tags$p("$$\\psi=\\dfrac{\\log\\dfrac{1-\\psi_1}{1-\\psi_2}}{\\log\\dfrac{\\psi_2(1-\\psi_1)}{\\psi_1(1-\\psi_2)}}$$"),
      tags$script(HTML("if (window.MathJax) { MathJax.Hub.Queue(['Typeset', MathJax.Hub]); }"))
    )
  })

  output$region_plot <- renderPlot({
    b <- bounds()
    d <- stein_rv$design_setting
    phiU <- b$phiU; psi <- b$psi
    # three regions on the (toxicity, efficacy) plane
    df <- expand.grid(tox = seq(0, 1, 0.01), eff = seq(0, 1, 0.01))
    df$region <- with(df, ifelse(tox >= phiU, "inadmissible",
                          ifelse(eff >= psi, "promising", "exploratory")))
    ggplot2::ggplot(df, ggplot2::aes(tox, eff, fill = region)) +
      ggplot2::geom_raster(alpha = 0.55) +
      ggplot2::geom_vline(xintercept = phiU, linetype = 2) +
      ggplot2::geom_hline(yintercept = psi, linetype = 2) +
      ggplot2::scale_fill_manual(values = c(
        promising = "#9FE1CB", exploratory = "#FAC775", inadmissible = "#F7C1C1")) +
      ggplot2::labs(x = "Toxicity probability", y = "Efficacy probability",
                    fill = NULL, title = "STEIN decision regions") +
      ggplot2::theme_minimal()
  })
}
