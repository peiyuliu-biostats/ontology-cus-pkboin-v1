# R/ui_description.R

library(shiny)
library(shinydashboard)

uimod_content_doc <- function(which = "model_desc") {
  if (which == "model_desc") {
    tabItem(
      tabName = "model_desc",
      withMathJax(),
      box(
        width = 12, title = "Methods Guide", status = "info", solidHeader = TRUE,
        p("This guide summarizes the mathematical core, decision rules, and intended use of the app's three dose-optimization components. CUS is an exposure-response scoring framework for selecting dose from given data, while BOIN12, PKBOIN-12, TITE-PKBOIN-12, and STEIN are adaptive phase I/II trial-design workflows."),
        tabsetPanel(
          tabPanel(
            "CUS",
            h4("ER-Clinical Utility Score"),
            p("The Clinical Utility Score (CUS) framework aggregates exposure-response (ER) relationships into a single desirability score. It is a dose-evaluation tool for observed or simulated data, not an adaptive rule for assigning the next cohort."),
            h4("1. Exposure-response modeling"),
            p("For each binary efficacy or safety endpoint i, the default ER model is logistic:"),
            p(style = "text-align:center;",
              "$$ P_i(x)=\\frac{1}{1+\\exp[-(\\beta_{0i}+\\beta_{1i}x)]}. $$"),
            p("Here x denotes exposure such as AUC, Cmax, or Cavg. The fitted probability P_i(x) is then mapped to a unit-scale endpoint utility U_i(x)."),
            h4("2. Endpoint utility transformation"),
            p("For linear binary scoring, efficacy uses higher-is-better scoring and safety uses lower-is-better scoring:"),
            p(style = "text-align:center;",
              "$$ U_i(x)=P_i(x)\\ \\text{for efficacy}, \\qquad U_i(x)=1-P_i(x)\\ \\text{for safety}. $$"),
            p("Stepwise or nonlinear utility mappings can be used when clinically specified cut points are preferred. A small positive lower bound is applied before logarithmic aggregation."),
            h4("3. Multi-endpoint aggregation and selection"),
            p("With normalized clinical weights w_i, the multiplicative CUS is"),
            p(style = "text-align:center;",
              "$$ CUS(x)=\\prod_{i=1}^{m} U_i(x)^{w_i}=\\exp\\left\\{\\sum_{i=1}^{m} w_i\\log U_i(x)\\right\\}, \\qquad \\sum_{i=1}^{m}w_i=1. $$"),
            p("The recommended exposure or dose is"),
            p(style = "text-align:center;",
              "$$ x^*=\\arg\\max_x CUS(x). $$"),
            p("Bootstrap resampling can summarize uncertainty in the fitted ER curves, CUS curve, and selected dose.")
          ),
          tabPanel(
            "STEIN",
            h4("STEIN Dose Optimization"),
            p("STEIN is an adaptive phase I/II design for identifying an optimal biological dose using both toxicity and efficacy. It assigns cohorts sequentially while removing doses that are unsafe or insufficiently efficacious."),
            h4("1. Boundary construction"),
            p("Let p_d and q_d denote toxicity and efficacy probabilities at dose d. STEIN uses target toxicity phi0, toxicity anchors phi1 < phi0 < phi2, and efficacy anchors psi1 < psi2. The app derives the model-assisted boundaries"),
            p(style = "text-align:center;",
              "$$ \\phi_L=\\frac{\\log\\{(1-\\phi_1)/(1-\\phi_0)\\}}{\\log\\{\\phi_0(1-\\phi_1)/[\\phi_1(1-\\phi_0)]\\}}, \\qquad
                  \\phi_U=\\frac{\\log\\{(1-\\phi_0)/(1-\\phi_2)\\}}{\\log\\{\\phi_2(1-\\phi_0)/[\\phi_0(1-\\phi_2)]\\}}, $$"),
            p(style = "text-align:center;",
              "$$ \\psi=\\frac{\\log\\{(1-\\psi_1)/(1-\\psi_2)\\}}{\\log\\{\\psi_2(1-\\psi_1)/[\\psi_1(1-\\psi_2)]\\}}. $$"),
            h4("2. Posterior monitoring"),
            p("After observing n_d patients, x_d toxicities, and y_d efficacies at dose d, the beta-binomial posterior updates are"),
            p(style = "text-align:center;",
              "$$ p_d\\mid data\\sim Beta(1+x_d,1+n_d-x_d), \\qquad q_d\\mid data\\sim Beta(1+y_d,1+n_d-y_d). $$"),
            p("A dose is removed for excessive toxicity or futility when"),
            p(style = "text-align:center;",
              "$$ Pr(p_d>\\phi_0\\mid data)>C_T \\Rightarrow \\text{eliminate dose }d\\text{ and all higher doses}, $$"),
            p(style = "text-align:center;",
              "$$ Pr(q_d\\le \\psi_1\\mid data)>C_E \\Rightarrow \\text{eliminate dose }d. $$"),
            h4("3. Sequential dose assignment"),
            p("At current dose j with observed toxicity rate hat p_j, the local admissible set is"),
            p(style = "text-align:center;",
              "$$ A_j=\\begin{cases}
                  \\{j,j+1\\}, & \\widehat p_j\\le \\phi_L,\\\\
                  \\{j-1,j,j+1\\}, & \\phi_L<\\widehat p_j<\\phi_U,\\\\
                  \\{j-1,j\\}, & \\widehat p_j\\ge \\phi_U,
                \\end{cases} $$"),
            p("after truncating to valid dose levels and removing eliminated doses. The next dose maximizes posterior efficacy promise within A_j:"),
            p(style = "text-align:center;",
              "$$ d_{next}=\\arg\\max_{d\\in A_j} Pr(q_d>\\psi\\mid data). $$"),
            h4("4. Final OBD selection"),
            p("At the end of the trial, observed toxicity rates are monotonized by PAVA to obtain tilde p_d. Efficacy is estimated by unimodal isotonic regression with AIC model averaging to obtain tilde q_d. The utility used by the app is"),
            p(style = "text-align:center;",
              "$$ U_d=\\widetilde q_d-w_1\\widetilde p_d-w_2\\widetilde p_d I(\\widetilde p_d>\\phi_0). $$"),
            p("The final OBD is the non-eliminated tried dose with the largest U_d under the STEIN safety and futility constraints.")
          ),
          tabPanel(
            "PK-BOIN12 Suite",
            h4("BOIN12, PKBOIN-12, and TITE-PKBOIN-12"),
            p("BOIN12, PKBOIN-12, and TITE-PKBOIN-12 form a progressive adaptive phase I/II family. BOIN12 jointly uses toxicity and efficacy, PKBOIN-12 adds pharmacokinetic exposure, and TITE-PKBOIN-12 further handles delayed toxicity and efficacy outcomes during ongoing accrual."),
            h4("1. BOIN12 utility and RDS"),
            p("For dose d, BOIN12 records the four joint efficacy-toxicity outcomes: efficacy/no toxicity, efficacy/toxicity, no efficacy/no toxicity, and no efficacy/toxicity. With utilities u_k and counts n_dk, the fractional utility response is"),
            p(style = "text-align:center;",
              "$$ x_d=\\frac{1}{100}\\sum_{k=1}^{4}u_k n_{dk}, \\qquad U_d\\mid data\\sim Beta(1+x_d,1+n_d-x_d). $$"),
            p("The rank desirability score is"),
            p(style = "text-align:center;",
              "$$ RDS_d=Pr(U_d>u_b\\mid data). $$"),
            h4("2. BOIN12 interim decision"),
            p("The toxicity interval boundaries define whether the current dose should de-escalate, stay, or allow escalation. Candidate doses are restricted to admissible, non-eliminated doses, and the next dose is the candidate with the largest RDS. Toxicity elimination is cascading, while efficacy-futility elimination applies to the evaluated dose:"),
            p(style = "text-align:center;",
              "$$ Pr(p_d>\\phi_T\\mid data)>C_T \\Rightarrow \\text{eliminate }d,d+1,\\ldots, $$"),
            p(style = "text-align:center;",
              "$$ Pr(q_d<\\phi_E\\mid data)>C_E \\Rightarrow \\text{eliminate }d. $$"),
            h4("3. PKBOIN-12 extension"),
            p("PKBOIN-12 keeps the BOIN12 efficacy-toxicity decision structure and adds PK adequacy. Let r_d denote the probability of adequate exposure at dose d. The intermediate PK threshold is"),
            p(style = "text-align:center;",
              "$$ \\zeta_1=(r_P+r_I)/2. $$"),
            p("Doses with strong evidence of inadequate PK can be pruned:"),
            p(style = "text-align:center;",
              "$$ Pr(r_d<r_P\\mid \\hat r_d,n_d)>C_P \\Rightarrow \\text{exclude low-exposure dose }d. $$"),
            p("At final analysis, PKBOIN-12 identifies the MTD by isotonic toxicity, identifies the minimum PK-acceptable dose by isotonic PK, and selects the maximum-utility non-eliminated dose in the admissible interval"),
            p(style = "text-align:center;",
              "$$ d\\in\\{d^{*}_{PK,min},\\ldots,d_{MTD}\\}. $$"),
            h4("4. TITE-PKBOIN-12 extension"),
            p("TITE-PKBOIN-12 preserves the PKBOIN-12 structure but replaces pending toxicity and efficacy outcomes with approximated-likelihood quasi-observations at each decision time. For a patient followed for t_j within assessment window A_T, a pending toxicity contributes conditional expected toxicity"),
            p(style = "text-align:center;",
              "$$ E(Y_T=1\\mid X_T>t_j)=\\frac{\\widehat p^{\\,*}_d(1-t_j/A_T)}{1-\\widehat p^{\\,*}_d t_j/A_T}. $$"),
            p("The same construction is used for efficacy with A_E and the current interim efficacy estimate. These quasi-counts update the interim toxicity, efficacy, joint utilities, RDS, and elimination rules without changing the final complete-data OBD logic. Accrual can be suspended when the current dose has too many pending outcomes.")
          ),
          tabPanel(
            "References",
            h4("Primary References"),
            tags$ul(
              tags$li("Cheng Y, Chu S, Pu J, et al. Exposure-Response-Based Multiattribute Clinical Utility Score Framework to Facilitate Optimal Dose Selection for Oncology Drugs. Journal of Clinical Oncology. 2024."),
              tags$li("Lin R, Zhou Y, Yan F, Li D, Yuan Y. BOIN12: Bayesian optimal interval phase I/II trial design for utility-based dose finding in immunotherapy. JCO Precision Oncology. 2020."),
              tags$li("Sun H, Tu J. PKBOIN12: A Bayesian optimal interval phase I/II design incorporating pharmacokinetics outcomes to find the optimal biological dose. Pharmaceutical Statistics. 2025."),
              tags$li("Yuan Y, Lin R, Li D, et al. Time-to-event Bayesian optimal interval design to accelerate phase I trials. Clinical Cancer Research. 2018.")
            )
          )
        )
      )
    )
  } else if (which == "about") {
    tabItem(
      tabName = "about",
      box(
        width = 12, title = "Author", status = "info",
        h4("Author"),
        p("Peiyu Liu"),
        p("Department of Biostatistics, University of Florida"),
        br(),
        h4("Contact"),
        p(tags$a(href = "mailto:peiyu.liu.stats@gmail.com", "peiyu.liu.stats@gmail.com")),
        p("Welcome reach out.")
      )
    )
  }
}
