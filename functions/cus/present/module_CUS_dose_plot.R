module_UI_CUS_dose_plot <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dose_plot_ui"))
  )
}

module_server_CUS_dose_plot <- function(input, output, session, all_rv) {
  ns <- session$ns
  
  output$dose_plot_ui <- renderUI({
    ER <- all_rv$ER_data_list$ER_rawdt
    req(!is.null(ER) & all_rv$overall_setting$simu_or_not == 2)
    
    if (!("Dose" %in% names(ER))) {
      return(NULL)
    }
    
    plotOutput(ns("plot_pk_dose"))
  })
  
  output$plot_pk_dose <- renderPlot({
    ER <- all_rv$ER_data_list$ER_rawdt
    req(!is.null(ER))
    req("Dose" %in% names(ER))
    req("PK" %in% names(ER))
    
    ER <- ER[!is.na(ER$Dose) & !is.na(ER$PK), ]
    req(nrow(ER) > 0)
    
    ggplot(ER, aes(y = factor(Dose), x = PK, col = factor(Dose))) +
      geom_boxplot() +
      labs(
        x = "PK",
        y = "Dose",
        title = "PK by Dose"
      ) +
      scale_color_discrete(name = "Dose") + 
      theme_bw(base_size = 16) +   
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
      )
  })

}