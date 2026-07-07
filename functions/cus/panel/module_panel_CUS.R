module_UI_panel_CUS <- function(id) {
  ns <- NS(id)

  tagList(
      fluidRow(
          column(width = 12, 
                 # h5("Clinical Utility Score", align = "center"),
                 # plotlyOutput(ns("CUS_plot")),
                 uiOutput(ns("UI_agg_block")),
                 uiOutput(ns("UI_CI_block")),
                 module_UI_CUS_plot(ns("CUS_plot")),
                 uiOutput(ns("CUS_footnote")), 
                 hr(), 
                 module_UI_CUS_dose_plot(ns("CUS_dose_plot"))
          )
      )
  )
}

module_server_panel_CUS <- function(input, output, session, all_rv) {
  ns <- session$ns

  # module for CUS plot 
  callModule(module = module_server_CUS_plot, "CUS_plot", all_rv, rv)
  
  # module for CUS dose boxplot 
  callModule(module = module_server_CUS_dose_plot, "CUS_dose_plot", all_rv)
  
  # CUS aggregation formula selector (1 = multiplicative, 2 = linear) 
  output$UI_agg_block <- renderUI({
    tagList(
      tags$div(
        style = "margin-bottom:10px;",
        tags$span("CUS aggregation formula", class = "custom-label"),
        tags$span(
          class = "cus-help", HTML("&#63;"),
          tags$span(
            class = "cus-tip",
            HTML("How per-endpoint scores are combined into one CUS. Both use the same weights and the same optimal-dose rule (max CUS over PK).<br><br><b>Multiplicative</b> &Pi; s<sub>i</sub><sup>w<sub>i</sub></sup>: conservative, one weak endpoint pulls the whole score down.<br><br><b>Linear</b> &Sigma; w<sub>i</sub> s<sub>i</sub>: compensatory, a strong endpoint can offset a weak one.")
          )
        ),
        radioButtons(
          ns("cus_agg_type"), label = NULL,
          choices = c("Multiplicative  (\u220F s\u1d62^w\u1d62)" = 1,
                      "Linear  (\u03A3 w\u1d62 s\u1d62)" = 2),
          selected = all_rv$overall_setting$cus_agg_type,
          inline = TRUE
        )
      )
    )
  })
  
  observeEvent(input$cus_agg_type, {
    new_type <- as.numeric(input$cus_agg_type)
    if(!is.na(new_type)) all_rv$overall_setting$cus_agg_type <- new_type
  })
  
  output$UI_CI_block <- renderUI({
    req(all_rv$overall_setting$simu_or_not == 2)
    tagList(
      h6("Add 95% Confidence Interval (CI) for:"),
      fluidRow(
        style = "width:100%;", 
        column(
          width = 4,
          actionButton(ns("add_CUS_CI"), "Clinical Utility Score"),
        ),
        column(
          width = 5,
          actionButton(ns("add_PK_CI"), "PK that maximizes CUS")
        ),
        column(
          width = 3, 
          actionButton(ns("remove_CI"), "Remove CI")
        )
        
      )
    )
  })
  
  rv <- reactiveValues(need_CUS_CI = FALSE, need_PK_CI = FALSE)

  observeEvent(input$add_CUS_CI, {
    if(all_rv$overall_setting$simu_or_not == 2 & nrow(all_rv$ER_data_list$ER_rawdt) > 0) {
      rv$need_CUS_CI <- TRUE
      rv$need_PK_CI <- FALSE
    } else {
      rv$need_CUS_CI <- FALSE
    }
  })
  
  observeEvent(input$add_PK_CI, {
    if(all_rv$overall_setting$simu_or_not == 2 & nrow(all_rv$ER_data_list$ER_rawdt) > 0) {
      rv$need_PK_CI <- TRUE
      rv$need_CUS_CI <- FALSE
    } else {
      rv$need_PK_CI <- FALSE
    }
  })
  
  observeEvent(input$remove_CI, {
    rv$need_CUS_CI <- FALSE
    rv$need_PK_CI <- FALSE
  })
  
  observeEvent({
    list(
      all_rv$overall_setting,
      all_rv$ER_data_list$ER_rawdt,
      all_rv$PK_data(),
      all_rv$utility_Sshape_setting, 
      all_rv$inidividual_utility_Sshape_setting, 
      all_rv$utility_stepwise_setting, 
      all_rv$individual_utility_stepwise_setting
    )
  }, {
    rv$need_CUS_CI <- FALSE
    rv$need_PK_CI <- FALSE
  })
  
  
  # CUS weight footnote 
  output$CUS_footnote <- renderUI({
    eff_num  <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    Score_dt <- all_rv$PK_data()$Score_dt
    
    if ((eff_num + safe_num) == 0) return(NULL)
    
    eff_part <- NULL
    eff_weight <- NULL
    safe_part <- NULL
    safe_weight <- NULL
    if(eff_num > 0) {
      eff_part <- paste(paste0("EFF_V", 1:eff_num), collapse = ": ")
      eff_weight <- paste(all_rv$eff_endpoint_setting$eff_weight[1:eff_num], collapse = ": ")
    }
    if(safe_num > 0) {
      safe_part <- paste(paste0("SAFE_V", 1:safe_num), collapse = ": ")
      safe_weight <- paste(all_rv$safe_endpoint_setting$safe_weight[1:safe_num], collapse = ": ")
    }

    # weight footnote: 
    comb_part <- NULL 
    if(eff_num > 0 & safe_num > 0) comb_part <- ": "
    weight_txt <- paste0("CUS Weights by Endpoint -- ", eff_part, comb_part, 
                       safe_part, " = ", eff_weight, comb_part, safe_weight)
    
    # CUS footnote: 
    max_CUS <- max(Score_dt$CUS, na.rm = TRUE)
    PK_at_max <- min(Score_dt$PK[Score_dt$CUS == max_CUS], na.rm = TRUE)
    max_txt <- paste0("Max CUS = ", round(max_CUS, 3),
                      " at PK = ", round(PK_at_max, 3))
    
    tagList(
      tags$p(
        weight_txt, 
        style = "font-size: 13px; font-style: italic; color: #666; margin-top: 5px;"
      ),
      tags$p(
        max_txt,
        style = "font-size: 13px; font-style: italic; color: #B22222; margin-top: -5px;"
      )
    )
  })
}
