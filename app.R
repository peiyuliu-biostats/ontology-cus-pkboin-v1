library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(tidyr)
library(purrr)
library(DT)
library(readxl)
library(plotly)
library(shinyFeedback)
library(shinyjs)
library(shinybusy)
library(stringr)
library(ggplot2)
library(mvtnorm)
library(parallel)
# locale setting
Sys.setlocale("LC_TIME", "English")

# source modular function library (step 0+1: functions/ holds extracted pure fns)
# sourced before R/ so definitions exist when R/server_inputs.R uses them
invisible(lapply(
  list.files("functions", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  source
))

# source retained legacy modules 
source("R/ui_description.R")   # Methods Guide tab  
source("R/data_cases.R")       # case library  

# ========== 
# 1. ui  
# =========== 
ui <- dashboardPage(
  dashboardHeader(title = "Early-Phase Adaptive Dose Optimization",
                  # theme
                  tags$li(class = "dropdown",
                          tags$style(HTML("

      /* 1. topbar -> pastel purple */
      .skin-blue .main-header .navbar { background-color: #C3B1E1 !important; }
      .skin-blue .main-header .logo {
        background-color: #C3B1E1 !important;
        color: #fff !important;
        border-bottom: 1px solid #a892d0 !important;
        white-space: normal !important;
        line-height: 20px !important;
        font-size: 16px !important;
        padding-top: 5px !important;
      }
      .skin-blue .main-header .logo:hover { background-color: #a892d0 !important; }
      .skin-blue .main-header .navbar .sidebar-toggle { color: #fff !important; }
      .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #a892d0 !important; }
      .skin-blue .main-header { border-bottom: 1px solid #a892d0 !important; }

      /* 2. sidebar white, dark text, pastel-purple active/hover */
      .skin-blue .main-sidebar { background-color: #ffffff !important; }
      .skin-blue .sidebar-menu > li > a { color: #333333 !important; }
      .skin-blue .sidebar-menu > li:hover > a {
        background-color: #E6E6FA !important;
        color: #000000 !important;
        border-left-color: #C3B1E1 !important;
      }
      .skin-blue .sidebar-menu > li.active > a {
        border-left-color: #C3B1E1 !important;
        background-color: #E6E6FA !important;
        color: #000000 !important;
      }

      /* 3. all-white content canvas */
      .content-wrapper, .right-side { background-color: #ffffff !important; }

      /* 4. flatten boxes: header has NO background, matches box body (gray/white) */
      .box {
        border: none !important;
        border-top: none !important;
        box-shadow: none !important;
        background: transparent !important;
      }
      .box > .box-header {
        background: transparent !important;
        background-color: transparent !important;
        color: #333 !important;
        padding: 6px 10px 4px !important;
        border-bottom: 1px solid #eee !important;
      }
      .box > .box-header .box-title {
        font-weight: 700 !important;
        font-size: 15px !important;
        color: #333 !important;
      }

      /* 5. left Settings box -> soft gray rounded panel (header transparent) */
      .box.box-solid.box-primary {
        background: #f6f6f8 !important;
        border-radius: 10px !important;
        padding-bottom: 6px !important;
      }
      .box.box-solid.box-primary > .box-header { border-bottom: 1px solid #e6e6ea !important; }
      .box.box-solid.box-primary > .box-body { background: #f6f6f8 !important; }

      /* 6. right Analysis box -> white (header transparent) */
      .box.box-solid.box-warning { background: #ffffff !important; }
      .box.box-solid.box-warning > .box-body { background: #ffffff !important; }

      /* methods guide / author static pages */
      .box.box-solid.box-info,
      .box.box-solid.box-info > .box-body,
      .tab-content,
      .tab-pane,
      .MathJax_Display,
      mjx-container {
        background: #ffffff !important;
      }
      .box.box-solid.box-info > .box-body,
      .tab-content,
      .tab-pane {
        overflow: visible !important;
      }
      .MathJax_Display,
      mjx-container {
        position: relative !important;
        z-index: 1 !important;
      }

      /* 7. narrow the left/right gutter */
      .box { margin-bottom: 10px !important; }
      .col-sm-4 { padding-right: 8px !important; }
      .col-sm-8 { padding-left: 8px !important; }

      /* 8. buttons neutral */
      .btn-default, .btn.action-button {
        background-color: #f4f4f4 !important;
        border-color: #d5d5d5 !important;
        color: #333 !important;
      }
      .btn-default:hover, .btn.action-button:hover {
        background-color: #e9e9e9 !important;
        border-color: #c5c5c5 !important;
        color: #000 !important;
      }

      /* 9. results tabs: idle text pastel purple, active text black */
      .nav-tabs > li > a { color: #C3B1E1 !important; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus { color: #000 !important; }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #C3B1E1 !important; }
                          ")))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PK-BOIN12 Suite", tabName = "pkboin", icon = icon("vial")),
      menuItem("STEIN Dashboard", tabName = "stein", icon = icon("chart-line")),
      menuItem("ER-Clinical Utility Score", tabName = "analysis", icon = icon("chart-line")),
      
      menuItem("Methods Guide", tabName = "model_desc", icon = icon("book")),
      menuItem("GitHub", icon = icon("github"), href = "https://github.com/peiyuliu-biostats/ontology-cus-pkboin-v1", newtab = TRUE),
      menuItem("Author", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    withMathJax(),
    # BMS CUS custom css + updateText handler (ported from BMS ui.R header, Port-6)
    tags$head(tags$style(HTML("
      .cus-help {display:inline-flex; align-items:center; justify-content:center;
        width:16px; height:16px; border-radius:50%; border:1px solid #999;
        color:#666; background:#fff; font-size:11px; font-weight:bold;
        cursor:help; position:relative; margin-left:5px; vertical-align:middle;}
      .cus-help:hover {background:#666; color:#fff;}
      .cus-help .cus-tip {visibility:hidden; opacity:0; transition:opacity .15s;
        position:absolute; left:50%; top:135%; transform:translateX(-50%);
        width:270px; background:#333; color:#fff; font-size:12px; font-weight:normal;
        line-height:1.5; text-align:left; padding:8px 10px; border-radius:6px; z-index:1000;}
      .cus-help:hover .cus-tip {visibility:visible; opacity:1;}
    "))),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateText', function(message) {
        const el = document.getElementById(message.id);
        if (el) { el.textContent = message.text; }
      });
    ")),
    tabItems(
      # main analysis tab (cus) — BMS CUS ported into shinydashboard box/tabBox (Port-4)
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Settings", width = 4, status = "primary",
                    solidHeader = TRUE, collapsible = FALSE,
                    module_UI_sidebar("sidebar_setting")
                ),
                box(title = "Analysis Panels", width = 8, status = "warning",
                    solidHeader = TRUE, collapsible = FALSE,
                    tabBox(width = 12,
                           tabPanel("Endpoint", module_UI_panel_endpoint("panel_endpoint")),
                           tabPanel("Utility",  module_UI_panel_utility("panel_utility")),
                           tabPanel("Clinical Utility Score", module_UI_panel_CUS("panel_CUS")),
                           tabPanel("Data", module_UI_panel_data("panel_data"))
                    )
                )
              )
      ),
      
      # BOIN dashboard tab (stage B: full tabBox; PKBOIN-12/TITE-PKBOIN-12
      # show a stage-not-implemented notice inside each tab -- see
      # fun_boin_stage_notice.R)
      tabItem(tabName = "pkboin",
              fluidRow(
                box(title = "Settings", width = 4, status = "primary",
                    solidHeader = TRUE, collapsible = FALSE,
                    module_UI_boin_sidebar("boin_sidebar")
                ),
                box(title = "Analysis Panels", width = 8, status = "warning",
                    solidHeader = TRUE, collapsible = FALSE,
                    tabBox(width = 12, id = "boin_tabs",
                           tabPanel("Design", module_UI_boin_design("boin_design")),
                           tabPanel("Flowchart", module_UI_boin_flowchart("boin_flowchart")),
                           tabPanel("Scenario", module_UI_boin_scenario("boin_scenario")),
                           tabPanel("Operating Characteristics", module_UI_boin_oc("boin_oc")),
                           tabPanel("Trial Conduct", module_UI_boin_conduct("boin_conduct")),
                           tabPanel("OBD Determination", module_UI_boin_obd("boin_obd")),
                           tabPanel("Data", module_UI_boin_data("boin_data"))
                    )
                )
              )
      ),
      
      # stein dashboard tab (simulate mode)
      tabItem(tabName = "stein",
              fluidRow(
                box(title = "Settings", width = 4, status = "primary",
                    solidHeader = TRUE, collapsible = FALSE,
                    module_UI_stein_sidebar("stein_sidebar")
                ),
                box(title = "Analysis Panels", width = 8, status = "warning",
                    solidHeader = TRUE, collapsible = FALSE,
                    tabBox(width = 12, id = "stein_tabs",
                           tabPanel("Design", module_UI_stein_design("stein_design")),
                           tabPanel("Flowchart", module_UI_stein_flowchart("stein_flowchart")),
                           tabPanel("Scenario", module_UI_stein_scenario("stein_scenario")),
                           tabPanel("Operating Characteristics", module_UI_stein_oc("stein_oc")),
                           tabPanel("Trial Conduct", module_UI_stein_conduct("stein_conduct")),
                           tabPanel("OBD Selection", module_UI_stein_obd_selection("stein_obd_selection")),
                           tabPanel("Data", module_UI_stein_data("stein_data"))
                    )
                )
              )
      ),
      
      # static content tabs
      uimod_content_doc(which = "model_desc"),
      uimod_content_doc(which = "about")
    )
  )
)

# ========== 
# 2. server  
# =========== 
server <- function(input, output, session) {
  
  addResourcePath("www", ".")
  
  ### initialize all reactive values (BMS)
  all_rv <- initialize_all_reactive_values()
  
  # UI: Sidebar
  callModule(module = module_server_sidebar, id = "sidebar_setting", all_rv)
  
  # UI: Panel - Endpoint
  callModule(module = module_server_panel_endpoint, id = "panel_endpoint", all_rv)
  
  # UI: Panel - Utility
  callModule(module = module_server_panel_utility, id = "panel_utility", all_rv)
  
  # UI: Panel - CUS
  callModule(module = module_server_panel_CUS, id = "panel_CUS", all_rv)
  
  # UI: Panel - Data
  callModule(module = module_server_panel_data, id = "panel_data", all_rv)
  
  ### STEIN dashboard (isolated reactiveValues; stein_ prefixed ids) ####
  stein_rv <- initial_stein_rv()
  callModule(module = module_server_stein_sidebar,  id = "stein_sidebar",  stein_rv)
  callModule(module = module_server_stein_design,   id = "stein_design",   stein_rv)
  callModule(module = module_server_stein_flowchart, id = "stein_flowchart", stein_rv)
  callModule(module = module_server_stein_scenario, id = "stein_scenario", stein_rv)
  callModule(module = module_server_stein_oc,       id = "stein_oc",       stein_rv)
  callModule(module = module_server_stein_conduct,       id = "stein_conduct",       stein_rv)
  callModule(module = module_server_stein_obd_selection, id = "stein_obd_selection", stein_rv)
  callModule(module = module_server_stein_data,     id = "stein_data",     stein_rv)

  # mode-dependent tab visibility: Scenario/Operating Characteristics/Data are
  # simulate-only (Data tab's simulate-specific content is being reworked for
  # upload mode in a later increment, so it stays hidden there for now);
  # Trial Conduct/OBD Selection are upload-only. Design/Flowchart always shown.
  observe({
    mode <- stein_rv$overall_setting$simu_or_not
    if (identical(mode, 1L) || identical(mode, 1)) {
      showTab(inputId = "stein_tabs", target = "Scenario")
      showTab(inputId = "stein_tabs", target = "Operating Characteristics")
      showTab(inputId = "stein_tabs", target = "Data")
      hideTab(inputId = "stein_tabs", target = "Trial Conduct")
      hideTab(inputId = "stein_tabs", target = "OBD Selection")
    } else {
      hideTab(inputId = "stein_tabs", target = "Scenario")
      hideTab(inputId = "stein_tabs", target = "Operating Characteristics")
      hideTab(inputId = "stein_tabs", target = "Data")
      showTab(inputId = "stein_tabs", target = "Trial Conduct")
      showTab(inputId = "stein_tabs", target = "OBD Selection")
    }
  })
  
  ### BOIN dashboard (isolated reactiveValues; boin_ prefixed ids) ####
  ### stage B: full module set (BOIN-12 computation only; PKBOIN-12/
  ### TITE-PKBOIN-12 show a stage-not-implemented notice per tab) ####
  boin_rv <- initial_boin_rv()
  callModule(module = module_server_boin_sidebar,   id = "boin_sidebar",   boin_rv)
  callModule(module = module_server_boin_design,    id = "boin_design",   boin_rv)
  callModule(module = module_server_boin_flowchart, id = "boin_flowchart", boin_rv)
  callModule(module = module_server_boin_scenario,  id = "boin_scenario", boin_rv)
  callModule(module = module_server_boin_oc,        id = "boin_oc",       boin_rv)
  callModule(module = module_server_boin_conduct,   id = "boin_conduct",  boin_rv)
  callModule(module = module_server_boin_obd,       id = "boin_obd",      boin_rv)
  callModule(module = module_server_boin_data,      id = "boin_data",     boin_rv)

  # mode-dependent tab visibility: same convention as the STEIN block
  # below (Scenario/Operating Characteristics/Data are simulate-only;
  # Trial Conduct/OBD Determination are upload-only; Design/Flowchart
  # always shown), applied to the isolated "boin_tabs" tabBox id.
  observe({
    mode <- boin_rv$overall_setting$simu_or_not
    if (identical(mode, 1L) || identical(mode, 1)) {
      showTab(inputId = "boin_tabs", target = "Scenario")
      showTab(inputId = "boin_tabs", target = "Operating Characteristics")
      showTab(inputId = "boin_tabs", target = "Data")
      hideTab(inputId = "boin_tabs", target = "Trial Conduct")
      hideTab(inputId = "boin_tabs", target = "OBD Determination")
    } else {
      hideTab(inputId = "boin_tabs", target = "Scenario")
      hideTab(inputId = "boin_tabs", target = "Operating Characteristics")
      showTab(inputId = "boin_tabs", target = "Data")
      showTab(inputId = "boin_tabs", target = "Trial Conduct")
      showTab(inputId = "boin_tabs", target = "OBD Determination")
    }
  })

  # surface duplicate-measurement warnings from the CUS computation to the user
  observe({
    dups <- all_rv$PK_data()$dup_warnings
    if (!is.null(dups) && length(dups) > 0) {
      showNotification(
        paste0("Duplicate measurement value(s) detected for: ",
               paste(dups, collapse = ", "),
               ". Kept the first score for each duplicate; please check your stepwise inputs."),
        type = "warning", duration = 8
      )
    }
  })
  
}

shinyApp(ui, server)
