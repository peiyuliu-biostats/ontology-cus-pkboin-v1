module_UI_panel_utility <- function(id)
{
  ns <- NS(id)
  tagList(
    # select the type of the utility function
    radioButtons(width = 1000, inputId = ns("utility_type"), 
                 label = div(class = "custom-label", "Select the type of the utility functions:"), 
                 choices = list("S-Shpaed utility function" = 1, "Stepwise utility function" = 2), 
                 selected = 1),
    # ---- S-Shape Utility -----
    conditionalPanel(
      condition = paste0("input['", ns("utility_type"), "'] == 1"),
      radioButtons(width = 1000, inputId = ns("individual_Sshape_utility"), 
                   label = div(class = "custom-label", "Provide separate utility per endpoint?"), 
                   choices = list("Yes" = 1, "No" = 2), 
                   selected = 2),
      # Not provide separate utility per endpoint
      conditionalPanel(
        condition = paste0("input['", ns("individual_Sshape_utility"), "'] == 2"),   
        module_UI_utility_Sshape(ns("utility_Sshape_setting"))
      )
    ),
    # Provide separate utility per endpoint
    uiOutput(ns("UI_Sshape_eff_for_inv")),
    uiOutput(ns("UI_Sshape_safe_for_inv")),
    
    # ---- Stepwise Utility -----
    conditionalPanel(
      condition = paste0("input['", ns("utility_type"), "'] == 2"),
      radioButtons(width = 1000, inputId = ns("individual_stepwise_utility"), 
                   label = div(class = "custom-label", "Provide separate utility per endpoint?"), 
                   choices = list("Yes" = 1, "No" = 2), 
                   selected = 2),
      # Not provide separate utility per endpoint
      conditionalPanel(
        condition = paste0("input['", ns("individual_stepwise_utility"), "'] == 2"),   
        module_UI_utility_stepwise_noInv(ns("utility_stepwise_setting_noInv"))
      ),
      # Provide separate utility per endpoint
      conditionalPanel(
        condition = paste0("input['", ns("individual_stepwise_utility"), "'] == 1"),   
        module_UI_utility_stepwise_forInv(ns("utility_stepwise_setting_forInv"))
      )
    )
  )
}

module_server_panel_utility <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  observeEvent(input$utility_type, {
    all_rv$overall_setting$utility_type = input$utility_type
  })
  
  observeEvent(input$individual_Sshape_utility, {
    all_rv$overall_setting$individual_Sshape_utility <- input$individual_Sshape_utility
    all_rv$triggers$update_utility_Sshape_indiv_trigger <- Sys.time()
  })
  
  observeEvent(input$individual_stepwise_utility, {
    all_rv$overall_setting$individual_stepwise_utility <- input$individual_stepwise_utility
    all_rv$triggers$update_utility_stepwise_indiv_trigger <- Sys.time()
  })
  
  # ------ indiviaul Sshape utility ----------
  
  # individual Sshape utility function for each efficacy endpoint 
  output$UI_Sshape_eff_for_inv <- renderUI({
    eff_num <- all_rv$endpoint_num_setting$eff_num
    req(!is.null(eff_num), !is.na(eff_num), eff_num > 0)
    if (all_rv$overall_setting$utility_type == 1 & all_rv$overall_setting$individual_Sshape_utility == 1) {
      lapply(1:eff_num, function(i) {
        module_UI_inv_utility_Sshape_eff(ns(paste0("eff_utility_Sshape_", i)), i)
      })
    }
  })
  
  # individual Sshape utility function for each safety endpoint 
  output$UI_Sshape_safe_for_inv <- renderUI({
    safe_num <- all_rv$endpoint_num_setting$safe_num
    req(!is.null(safe_num), !is.na(safe_num), safe_num > 0)
    if (all_rv$overall_setting$utility_type == 1 & all_rv$overall_setting$individual_Sshape_utility == 1) {
      lapply(1:safe_num, function(i) {
        module_UI_inv_utility_Sshape_safe(ns(paste0("safe_utility_Sshape_", i)), i)
      })
    }
  })
  
  observe({
    eff_num <- all_rv$endpoint_num_setting$eff_num
    req(!is.null(eff_num), !is.na(eff_num), eff_num > 0)
    lapply(1:eff_num, function(i) {
      if (all_rv$overall_setting$utility_type == 1 & all_rv$overall_setting$individual_Sshape_utility == 1) {
        callModule(module_server_inv_utility_Sshape_eff, paste0("eff_utility_Sshape_", i), i, all_rv)
      } 
    })
  })
  
  observe({
    safe_num <- all_rv$endpoint_num_setting$safe_num
    req(!is.null(safe_num), !is.na(safe_num), safe_num > 0)
    lapply(1:safe_num, function(i) {
      if (all_rv$overall_setting$utility_type == 1 & all_rv$overall_setting$individual_Sshape_utility == 1) {
        callModule(module_server_inv_utility_Sshape_safe, paste0("safe_utility_Sshape_", i), i, all_rv)
      } 
    })
  })
  
  
  callModule(module = module_server_utility_Sshape, id = "utility_Sshape_setting", 
             all_rv)
  
  callModule(module = module_server_utility_stepwise_noInv, 
             id = "utility_stepwise_setting_noInv", all_rv)
  
  callModule(module = module_server_utility_stepwise_forInv, 
             id = "utility_stepwise_setting_forInv", all_rv)

}