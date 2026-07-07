module_UI_inv_utility_Sshape_eff <- function(id, index)
{
  ns <- NS(id)
  tagList(
    if(index == 1) {
      h6("Efficacy:")
    },
    fluidRow(
      column(width = 5, 
             div(
               style = "padding-left: 5px; padding-right: 10px;",
               div(
                 style = "display:flex; align-items:center; height:100%; justify-content:center;",
                 class = "custom-label", paste0("For efficacy endpoint ", index, ":")
               ),
               sliderInput(ns("eff_beta"), label = 'Beta parameter',
                           min = 0, max = 10, step = 0.5, value = 1),
               sliderInput(ns("eff_shape"), label = 'Location parameter',
                           min = 0, max = 1, step = 0.05, value = 0.5)
             )
      ),
      column(width = 6,
             plotlyOutput(ns("utility_sigmoid_eff"), width = "100%")
      )
    ),
  )
}


module_server_inv_utility_Sshape_eff <- function(input, output, session, index, all_rv)
{
  ns <- session$ns
  
  observeEvent(TRUE, {
    updateSliderInput(session, "eff_beta", value = all_rv$inidividual_utility_Sshape_setting$eff_beta[index])
    updateSliderInput(session, "eff_shape", value = all_rv$inidividual_utility_Sshape_setting$eff_shape[index])
  }, once = TRUE)
  
  observeEvent(all_rv$triggers$update_utility_Sshape_indiv_trigger, {
    updateNumericInput(session, "eff_beta", value = all_rv$inidividual_utility_Sshape_setting$eff_beta[index])
    updateNumericInput(session, "eff_shape", value = all_rv$inidividual_utility_Sshape_setting$eff_shape[index])
  }, ignoreInit = TRUE)
  
  observeEvent(debounce(reactive(input$eff_beta), 50)(), {
    new_num <- isolate(as.numeric(input$eff_beta))
    all_rv$inidividual_utility_Sshape_setting$eff_beta[index] <- input$eff_beta
  })
  
  observeEvent(debounce(reactive(input$eff_shape), 50)(), {
    new_num <- isolate(as.numeric(input$eff_shape))
    all_rv$inidividual_utility_Sshape_setting$eff_shape[index] <- input$eff_shape
  })
  
  # S-shape Figure for Efficacy
  output$utility_sigmoid_eff<-renderPlotly({
    beta <- all_rv$inidividual_utility_Sshape_setting$eff_beta[index]
    u <- all_rv$inidividual_utility_Sshape_setting$eff_shape[index]
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
    
    rt <- utility_response_ticks(all_rv, "eff", index)
    gp <- plotly::ggplotly(p, width = 400, height = 350) %>% 
      layout(
        title = list(
          text = paste0("Utility Component for Efficacy Endpoint ", index),  
          x = 0.5,                                 
          y = 1.15,                                
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16, family = "Arial", color = "black")
        ),
        margin = list(t = 60)                    
      )
    if (!is.null(rt)) {
      gp <- gp %>% layout(xaxis = list(title = list(text = rt$axis_title),
                                       tickmode = "array",
                                       tickvals = rt$tickvals, ticktext = rt$ticktext))
    }
    gp
  })
}