# R/ui_description.R
# ui-mod1: Description & Author

library(shiny)
library(shinydashboard)

uimod_content_doc <- function(which = "model_desc") {
  if (which == "model_desc") {
    tabItem(tabName = "model_desc",
            # Enable MathJax for formula rendering
            withMathJax(),
            
            box(width = 12, title = "Model Description: CUS Framework", status = "info", solidHeader = TRUE,
                
                h3("Overview"),
                p("The Clinical Utility Score (CUS) framework integrates efficacy and safety exposure-response (ER) relationships into a single quantitative metric. It aims to guide optimal dose selection in oncology by balancing therapeutic benefits against toxicity risks."),
                
                tags$hr(),
                
                h4("1. Exposure-Response (ER) Modeling"),
                p("For each clinical endpoint (efficacy or safety), the probability of an event occurring at a given exposure level (x) is modeled using Logistic Regression:"),
                p(style = "text-align: center;",
                  "$$ P(x) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 \\cdot x)}} $$"
                ),
                p("Where:", tags$br(),
                  "- \\( x \\): Drug Exposure (e.g., AUC, Cmax, Cavg)", tags$br(),
                  "- \\( \\beta_0 \\): Intercept parameter", tags$br(),
                  "- \\( \\beta_1 \\): Slope parameter"),
                
                h4("2. Utility Functions"),
                p("Probabilities are transformed into Utility Scores \\( U(x) \\) to represent desirability. This app currently implements the Linear Utility function:"),
                p(style = "text-align: center;",
                  "$$ U(x) = \\begin{cases} P(x) & \\text{for Efficacy (Higher is better)} \\\\ 1 - P(x) & \\text{for Safety (Lower is better)} \\end{cases} $$"
                ),
                p("Note: A minimum threshold (e.g., 1e-6) is applied to prevent logarithmic errors."),
                
                h4("3. Aggregation (CUS Calculation)"),
                p("Individual utility scores are aggregated into a single Clinical Utility Score using a weighted multiplicative model. This ensures that a poor score in any critical endpoint significantly impacts the overall score."),
                p(style = "text-align: center;",
                  "$$ CUS(x) = \\prod_{i=1}^{n} (U_i(x))^{w_i'} = \\exp \\left( \\sum_{i=1}^{n} w_i' \\cdot \\ln(U_i(x)) \\right) $$"
                ),
                p("Where:", tags$br(),
                  "- \\( n \\): Total number of endpoints", tags$br(),
                  "- \\( w_i' \\): Normalized weight for endpoint \\( i \\), such that \\( \\sum w_i' = 1 \\)"),
                
                tags$hr(),
                
                h4("References"),
                p("Cheng Y, Chu S, Pu J, et al. Exposure-Response–Based Multiattribute Clinical Utility Score Framework to Facilitate Optimal Dose Selection for Oncology Drugs. J Clin Oncol. 2024.")
            )
    )
  } else if (which == "about") {
    tabItem(tabName = "about",
            box(width = 12, title = "Author", status = "info",
                h4("Developer"),
                p("Developed based on the CUS framework literature."),
                p("This application demonstrates the implementation of complex decision-making frameworks using R Shiny, featuring modular architecture, reactive calculation, and interactive visualization."),
                br(),
                h4("Contact"),
                p("For questions or feedback regarding this application, please refer to the repository documentation.")
            )
    )
  }
}