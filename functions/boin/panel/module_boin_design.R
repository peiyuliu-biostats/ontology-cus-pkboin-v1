# =====================================================================
# BOIN-12 / PKBOIN-12 Design tab module
# =====================================================================

module_UI_boin_design <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
    uiOutput(ns("pk_section")),
    tags$h4("Derived decision boundaries"),
    tags$div(
      style = "font-family:'Courier New', monospace; background:#f6f6f8; padding:10px 14px; border-radius:6px;",
      uiOutput(ns("boundaries_table"))
    ),
    uiOutput(ns("boundaries_note")),
    tags$details(
      style = "margin:6px 0 16px;",
      tags$summary(style = "cursor:pointer; color:#555;", "Show BOIN12 decision rule"),
      uiOutput(ns("boundaries_rule"))
    ),
    tags$details(
      style = "margin-bottom:16px;",
      tags$summary(style = "cursor:pointer; color:#555;", "Show derivation formulas"),
      uiOutput(ns("boundaries_formula"))
    ),
    tags$h4("Utility table"),
    DT::DTOutput(ns("utility_table")),
    tags$h4("Utility by joint outcome (dose-independent view)"),
    plotOutput(ns("utility_bar"), height = "300px"),
    tags$h4("Rank-based desirability score (RDS) look-up"),
    tags$details(
      style = "margin:4px 0 8px;",
      tags$summary(style = "cursor:pointer; color:#555;", "Show RDS note"),
      uiOutput(ns("rds_note"))
    ),
    DT::DTOutput(ns("rds_table"))
  )
}

module_server_boin_design <- function(input, output, session, boin_rv) {
  
  output$stage_notice <- renderUI(boin_stage_notice_ui(boin_rv$overall_setting$method))
  
  bounds <- reactive({
    d <- boin_rv$design_setting
    req(d$phi_T)
    boin_boundaries(d$phi_T, phi1 = d$phi1, phi2 = d$phi2)
  })
  
  output$pk_section <- renderUI({
    if (!boin_is_pk_method(boin_rv$overall_setting$method)) return(NULL)
    pk <- boin_rv$pk_setting
    req(pk$r_P, pk$r_I_mult, pk$C_P)
    z <- pkboin_zeta1(pk$r_P, pk$r_I_mult)
    rI <- pk$r_I_mult * pk$r_P
    
    tags$div(
      tags$h4(sprintf("PK admissibility (%s)", boin_rv$overall_setting$method)),
      tags$div(
        style = "font-family:'Courier New', monospace; background:#fff6e6; padding:10px 14px; border-radius:6px; font-size:13px;",
        tags$table(
          style = "width:100%; border-collapse:collapse;",
          tags$tr(
            tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Target PK value"),
            tags$td(sprintf("r_P = %.0f", pk$r_P))
          ),
          tags$tr(
            tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Inefficacious PK"),
            tags$td(sprintf("r_I = %.2f x r_P = %.0f", pk$r_I_mult, rI))
          ),
          tags$tr(
            tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "PK cutoff"),
            tags$td(sprintf("zeta1 = (r_P + r_I)/2 = %.0f", z))
          ),
          tags$tr(
            tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "PK elimination cutoff"),
            tags$td(sprintf("C_P = %.2f", pk$C_P))
          )
        )
      ),
      tags$details(
        style = "margin:6px 0 12px;",
        tags$summary(style = "cursor:pointer; color:#555;", "Show PKBOIN12 PK rule"),
        tags$p(
          style = "margin:8px 0 4px; color:#333; font-size:13px;",
          sprintf("Expanded admissible set: when the current dose's observed mean PK r_hat_d > zeta1 = %.0f, the admissible set's lower end drops from j-1 to d* = min(j-1, d_PK,min), where d_PK,min is the lowest dose with r_hat > zeta1. When r_hat_d <= zeta1, the set is exactly BOIN12's.", z)
        ),
        tags$p(
          style = "margin:2px 0 4px; color:#333; font-size:13px;",
          sprintf("PK elimination: a dose with n >= 6 and Pr(r_d < r_P | data) > C_P = %.2f is removed for inefficacious exposure. If the top dose is flagged, the trial terminates. Toxicity/efficacy elimination, lambda_e/lambda_d, utility, and RDS are unchanged from BOIN12.", pk$C_P)
        )
      ),
      tags$hr(style = "margin:10px 0;")
    )
  })
  
  output$boundaries_table <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    b <- bounds()
    tags$table(
      style = "width:100%; border-collapse:collapse; font-size:13px;",
      tags$tr(
        tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Toxicity anchors"),
        tags$td(sprintf("phi1 = %.4f    phi2 = %.4f", b$phi1, b$phi2))
      ),
      tags$tr(
        tags$td(style = "font-weight:bold; padding:2px 12px 2px 0; white-space:nowrap;", "Toxicity boundaries"),
        tags$td(sprintf("lambda_e = %.4f    lambda_d = %.4f", b$lambda_e, b$lambda_d))
      )
    )
  })
  
  output$boundaries_note <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    tags$p(
      style = "color:#777; font-size:11.5px; margin:2px 0 10px;",
      "phi1, phi2, lambda_e, and lambda_d depend only on phi_T. phi_E drives futility elimination, the RDS utility benchmark, and OBD efficacy admissibility."
    )
  })
  
  output$boundaries_rule <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    b <- bounds()
    tags$p(
      style = "margin:8px 0 14px; color:#333; font-size:13.5px;",
      sprintf(
        "Observed toxicity rate >= %.3f: de-escalate to j-1. Otherwise choose the dose with the largest RDS over the toxicity-admissible, non-eliminated set. If %.3f < p_hat_tox < %.3f, the BOIN12 set is {j-1,j} when n_j >= N* (=6), otherwise {j-1,j,j+1}. If p_hat_tox <= %.3f, the set is {j-1,j,j+1}, except the fast-escalation shortcut sends the trial straight to j+1 when n_j >= 9 and j+1 has never been tried. Safety/futility elimination is applied by posterior cutoffs.",
        b$lambda_d, b$lambda_e, b$lambda_d, b$lambda_e
      )
    )
  })
  
  output$boundaries_formula <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    bounds()
    tagList(
      tags$p(style = "margin-top:10px;",
             "$$\\lambda_e=\\dfrac{\\log\\dfrac{1-\\phi_1}{1-\\phi_T}}{\\log\\dfrac{\\phi_T(1-\\phi_1)}{\\phi_1(1-\\phi_T)}}$$"),
      tags$p("$$\\lambda_d=\\dfrac{\\log\\dfrac{1-\\phi_T}{1-\\phi_2}}{\\log\\dfrac{\\phi_2(1-\\phi_T)}{\\phi_T(1-\\phi_2)}}$$"),
      tags$p("$$U=100\\pi_1+60\\pi_2+40\\pi_3+0\\pi_4 \\quad \\text{under default utility values}$$"),
      tags$script(HTML("if (window.MathJax) { MathJax.Hub.Queue(['Typeset', MathJax.Hub]); }"))
    )
  })
  
  output$utility_table <- DT::renderDT({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    d <- boin_rv$design_setting
    df <- data.frame(
      Category = c("Efficacy & no-toxicity", "Efficacy & toxicity",
                   "No-efficacy & no-toxicity", "No-efficacy & toxicity"),
      Utility = c(d$u1, d$u2, d$u3, d$u4)
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
  
  output$utility_bar <- renderPlot({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    d <- boin_rv$design_setting
    df <- data.frame(
      Category = factor(c("eff & no-tox", "eff & tox", "no-eff & no-tox", "no-eff & tox"),
                        levels = c("eff & no-tox", "eff & tox", "no-eff & no-tox", "no-eff & tox")),
      Utility = c(d$u1, d$u2, d$u3, d$u4)
    )
    ggplot2::ggplot(df, ggplot2::aes(Category, Utility, fill = Category)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c("#9FE1CB", "#FAC775", "#AFC9E8", "#F7C1C1"), guide = "none") +
      ggplot2::labs(x = NULL, y = "Utility", title = "Utility table (current settings)") +
      ggplot2::theme_minimal(base_size = 15)
  })
  
  rds_tbl <- reactive({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    d <- boin_rv$design_setting
    tr <- boin_rv$trial_setting
    req(d$phi_T, d$phi_E, tr$cohort_size, tr$n_max)
    u <- c(d$u1, d$u2, d$u3, d$u4)
    cs <- max(1L, as.integer(tr$cohort_size))
    ns <- seq(cs, min(3L * cs, as.integer(tr$n_max)), by = cs)
    ns <- ns[ns >= 1]
    boin_rds_table(u, d$phi_T, d$phi_E, ns)
  })
  
  output$rds_note <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    tb <- rds_tbl()
    exact <- attr(tb, "exact")
    u_b <- attr(tb, "u_b")
    tags$p(
      style = "margin:8px 0; color:#333; font-size:13px;",
      sprintf("Utility benchmark u_b = %.2f. Higher RDS is more desirable; interim allocation picks the admissible dose with the largest RDS. ", u_b),
      if (exact) {
        "Because u2 + u3 = 100, RDS is an exact function of the marginals, matching the paper's Table 3 layout."
      } else {
        tags$span(style = "color:#a05a00;",
                  "Because u2 + u3 != 100, RDS depends on the full joint split n1..n4; the table below is indicative for canonical splits.")
      }
    )
  })
  
  output$rds_table <- DT::renderDT({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    DT::datatable(rds_tbl(), rownames = FALSE,
                  options = list(dom = "tp", pageLength = 12, ordering = TRUE))
  })
}
