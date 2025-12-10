library(shiny)
library(shinydashboard)
library(shinyBS)
library(promises)
library(future)
plan(multisession)
# locale setting
Sys.setlocale("LC_TIME", "English")

# source modules
source("R/ui_inputs.R")
source("R/ui_outputs.R")
source("R/ui_description.R")
source("R/server_inputs.R")
source("R/server_outputs.R")

# ========== 
# 1. ui  
# =========== 
ui <- dashboardPage(
  dashboardHeader(title = "Oncology Dose CUS",
                  # theme
                  tags$li(class = "dropdown",
                          tags$style(HTML("
      /* 1. change top bar background to purple */
      .skin-blue .main-header .navbar {
        background-color: #6f42c1 !important;
      }
      .skin-blue .main-header .logo {
        background-color: #6f42c1 !important;
      }
      /* logo hover state */
      .skin-blue .main-header .logo:hover {
        background-color: #5a32a3 !important;
      }
      
      /* 2. change sidebar background to white and text to dark */
      .skin-blue .main-sidebar {
        background-color: #ffffff !important;
      }
      /* sidebar text color */
      .skin-blue .sidebar-menu > li > a {
        color: #333333 !important;
      }
      /* sidebar hover state */
      .skin-blue .sidebar-menu > li:hover > a {
        background-color: #f4f4f4 !important;
        color: #000000 !important;
        border-left-color: #6f42c1 !important;
      }
      /* sidebar active state */
      .skin-blue .sidebar-menu > li.active > a {
        border-left-color: #6f42c1 !important;
        background-color: #f0ebf9 !important;
        color: #000000 !important;
      }
      
      /* 3 & 4. custom box colors overriding default primary/warning */
      
      /* left parameter box (primary) -> light purple (#9575cd) */
      .box.box-solid.box-primary {
        border-color: #9575cd;
      }
      .box.box-solid.box-primary > .box-header {
        background: #9575cd;
        background-color: #9575cd;
        color: #fff;
      }
      
      /* right parameter box (warning) -> light orange (#ffb74d) */
      .box.box-solid.box-warning {
        border-color: #ffb74d;
      }
      .box.box-solid.box-warning > .box-header {
        background: #ffb74d;
        background-color: #ffb74d;
        color: #fff;
      }
                          ")))
  ),
  dashboardSidebar(
    sidebarMenu(
      # rename to cus dashboard
      menuItem("CUS Dashboard", tabName = "analysis", icon = icon("chart-line")),
      # add pkboin-12 dashboard
      menuItem("PKBOIN-12 Dashboard", tabName = "pkboin", icon = icon("vial")),
      
      menuItem("Model Description", tabName = "model_desc", icon = icon("book")),
      menuItem("GitHub", icon = icon("github"), href = "#", newtab = TRUE),
      menuItem("Author", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # main analysis tab (cus)
      tabItem(tabName = "analysis",
              uimod_content_inputs(),
              uimod_content_outputs()
      ),
      
      # placeholder for pkboin-12 tab
      tabItem(tabName = "pkboin",
              h2("PKBOIN-12 Dashboard"),
              p("Content for PKBOIN-12 design will go here.")
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
  
  # 1. inputs & computing module
  # returns list of reactives: data, inputs, pk_ref
  reactives_list <- servermod_inputs_computing(input, output, session)
  
  # 2. outputs & rendering module
  # consumes reactives in step 1
  servermod_output_render(input, output, reactives_list)
  
}

shinyApp(ui, server)
