module_UI_utility_Sshape <- function(id) {
  ns <- NS(id)
  tagList(
    h6("S-shaped Utility Function Settings:"),
    fluidRow(
      column(width = 5,
             div(
               style = "display:flex; align-items:center; height:100%; justify-content:center;",
               class = "custom-label", "For efficacy:"
             )),
      column(width = 2),  
      column(width = 5,
             div(
               style = "display:flex; align-items:center; height:100%; justify-content:center;",
               class = "custom-label", "For safety:"
             ))
    ),
    
    fluidRow(
      column(width = 5, sliderInput(ns("eff_beta"), label = 'Beta parameter',
                                    min = 0, max = 10, step = 0.5, value = 1)),
      column(width = 2),
      column(width = 5, sliderInput(ns("safe_beta"), label = 'Beta parameter',
                                    min = 0, max = 10, step = 0.5, value = 1))
    ),
    
    fluidRow(
      column(width = 5, sliderInput(ns("eff_shape"), label = 'Location parameter',
                                    min = 0, max = 1, step = 0.05, value = 0.5)),
      column(width = 2),
      column(width = 5, sliderInput(ns("safe_shape"), label = 'Location parameter',
                                    min = 0, max = 1, step = 0.05, value = 0.5))
    ),
    hr(),
    # Utility function figure
    fluidRow(
      column(width = 6, 
             plotlyOutput(ns("utility_sigmoid_eff"), width = "100%")),
      column(width = 6, 
             plotlyOutput(ns("utility_sigmoid_safe"), width = "100%"))
    )
  )
}

module_server_utility_Sshape <- function(input, output, session, all_rv) {
  ns <- session$ns
  
  observeEvent(input$eff_beta, { all_rv$utility_Sshape_setting$eff_beta <- input$eff_beta })
  observeEvent(input$eff_shape, { all_rv$utility_Sshape_setting$eff_shape <- input$eff_shape })
  observeEvent(input$safe_beta, { all_rv$utility_Sshape_setting$safe_beta <- input$safe_beta })
  observeEvent(input$safe_shape, { all_rv$utility_Sshape_setting$safe_shape <- input$safe_shape })
  
  # S-shape Figure for Efficacy
  output$utility_sigmoid_eff<-renderPlotly({
    beta<-all_rv$utility_Sshape_setting$eff_beta
    u <- all_rv$utility_Sshape_setting$eff_shape
    x <- seq(0, 1, by = 0.002)
    y = round(1/(1+((x*(1-u))/(u*(1-x)))^(-beta)), 3)
    data<-as.data.frame(cbind(x,y))
    
    p <- ggplot(data, aes(x, y)) +
      geom_line(color = "steelblue", linewidth = 1) +     
      xlab("Rate") + ylab("Score") + 
      theme_bw(base_size = 12) +                         
      theme(
        plot.title = element_text(size = 13, face = "plain", hjust = 0.5),  
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
        axis.title = element_text(size = 11),
        axis.text  = element_text(size = 10)
      )
    
    plotly::ggplotly(p, width = 400, height = 350) %>% 
      layout(
        title = list(
          text = "Utility Component for Efficacy",  
          x = 0.5,                                 
          y = 1.15,                                
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        margin = list(t = 60)                    
      )
  })
  # S-shape Figure for Safety 
  output$utility_sigmoid_safe<-renderPlotly({
    beta<-all_rv$utility_Sshape_setting$safe_beta
    u <- all_rv$utility_Sshape_setting$safe_shape
    x <- seq(0, 1, by = 0.002)
    y = round(1 - 1/(1+((x*(1-u))/(u*(1-x)))^(-beta)), 3)
    data<-as.data.frame(cbind(x,y))
    
    p <- ggplot(data, aes(x, y)) +
      geom_line(color = "steelblue", linewidth = 1) +     
      xlab("Rate") + ylab("Score") + 
      theme_bw(base_size = 12) +                          
      theme(
        plot.title = element_text(size = 13, face = "plain", hjust = 0.5),  
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
        axis.title = element_text(size = 11),
        axis.text  = element_text(size = 10)
      )
    
    plotly::ggplotly(p, width = 400, height = 350) %>% 
      layout(
        title = list(
          text = "Utility Component for Safety",  
          x = 0.5,                                 
          y = 1.15,                                
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        margin = list(t = 60)                 
      )
  })
}