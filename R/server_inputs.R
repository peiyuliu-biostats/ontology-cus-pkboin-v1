# R/server_inputs.R
# server module: inputs & computation

library(shiny)
library(dplyr)
library(purrr)

# ==============================================================================
# helper functions: core calculations (pure functions)
# ==============================================================================

#' logistic function
#' @param x exposure value
#' @param b0 intercept
#' @param b1 slope
calc_prob_logistic <- function(x, b0, b1) {
  # standard logistic regression formula
  1 / (1 + exp(-(b0 + b1 * x)))
}

#' linear utility function
#' @param prob probability
#' @param type "efficacy" or "safety"
calc_utility_linear <- function(prob, type) {
  if (type == "Efficacy") {
    return(prob)
  } else {
    # ensure it doesn't hit 0 to avoid log errors
    return(pmax(1 - prob, 1e-6))
  }
}

#' calculate cus for a single exposure point
#' @param x exposure value
#' @param endpoints list of endpoint configurations
calculate_cus_point <- function(x, endpoints) {
  
  # 1. calculate individual scores (si)
  scores <- map_dbl(endpoints, function(ep) {
    prob <- calc_prob_logistic(x, ep$intercept, ep$slope)
    util <- calc_utility_linear(prob, ep$type) 
    return(util)
  })
  
  # 2. get weights (wi)
  weights <- map_dbl(endpoints, ~ .x$weight)
  
  # 3. normalize weights
  # the formula is product(si ^ wi_normalized) where sum(wi_norm) = 1
  # this is equivalent to the weighted geometric mean
  if (sum(weights) == 0) weights <- rep(1, length(weights)) # avoid div by zero
  norm_weights <- weights / sum(weights)
  
  # 4. aggregate
  # cus = product( si ^ wi_norm )
  # using exp(sum(log)) for numerical stability
  cus_val <- exp(sum(norm_weights * log(scores)))
  
  return(cus_val)
}

#' generate full dataset for plotting
#' @param pk_min min exposure
#' @param pk_max max exposure
#' @param endpoints list of endpoint definitions
generate_cus_data <- function(pk_min, pk_max, endpoints) {
  # generate x sequence
  x_seq <- seq(pk_min, pk_max, length.out = 200)
  
  # initialize data frame
  df <- data.frame(Exposure = x_seq)
  
  # 1. calculate cus column
  df$CUS <- map_dbl(x_seq, ~ calculate_cus_point(.x, endpoints))
  
  # 2. calculate individual probability columns (for component plot)
  for (i in seq_along(endpoints)) {
    ep <- endpoints[[i]]
    # raw probability (from logistic)
    prob_raw <- calc_prob_logistic(x_seq, ep$intercept, ep$slope)
    
    # store raw probabilities for visualization
    # note: for safety, we usually plot the probability of event (rising curve),
    # even though the utility contributes as (1-p)
    df[[paste0("Prob_", ep$name)]] <- prob_raw
    
    # optional: store utility score for debugging
    # df[[paste0("Util_", ep$name)]] <- calc_utility_linear(prob_raw, ep$type)
  }
  
  return(df)
}

# ============================= 
# server module: inputs logic
# ============================= 

servermod_inputs_computing <- function(input, output, session) {
  
  # 1. state to hold current endpoint data
  current_eps <- reactiveValues(
    data = list(
      list(name = "ORR", type = "Efficacy", intercept = -0.746, slope = 1.81, weight = 1),
      list(name = "Safety_Event", type = "Safety", intercept = -4.3, slope = 1.32, weight = 1)
    )
  )
  
  # 2. case info modal
  observeEvent(input$case_info_btn, {
    req(input$demo_case != "None")
    case <- case_library[[input$demo_case]]
    showModal(modalDialog(
      title = case$label,
      h4("Background"), p(case$desc),
      h4("Endpoints Included"),
      tags$ul(lapply(case$endpoints, function(x) tags$li(paste0(x$name, " (", x$type, ")")))),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  # 3. load demo logic
  observeEvent(input$load_demo, {
    req(input$demo_case != "None")
    case <- case_library[[input$demo_case]]
    
    # update ui inputs
    updateNumericInput(session, "pk_min", value = case$pk_min)
    updateNumericInput(session, "pk_max", value = case$pk_max)
    updateNumericInput(session, "pk_ref", value = case$pk_ref)
    updateSliderInput(session, "n_endpoints", value = length(case$endpoints))
    
    # update internal state
    current_eps$data <- case$endpoints
    
    showNotification(paste("Loaded:", case$label), type = "message", icon = icon("check"))
  })
  
  # 4. render dynamic ui based on current_eps$data
  output$endpoint_ui_container <- renderUI({
    n <- input$n_endpoints
    preset <- current_eps$data
    
    lapply(1:n, function(i) {
      # use preset if available, else default
      vals <- if (i <= length(preset)) preset[[i]] else list(name=paste0("New_EP_", i), type="Safety", intercept=0, slope=1, weight=1)
      
      wellPanel(
        style = "background: #f9f9f9; padding: 10px; margin-bottom: 10px; border-left: 3px solid #3c8dbc;",
        h5(strong(paste("Endpoint", i)), class = "text-primary", style = "margin-top:0;"),
        fluidRow(
          column(4, textInput(paste0("ep_name_", i), "Name", value = vals$name)),
          column(4, selectInput(paste0("ep_type_", i), "Type", choices = c("Efficacy", "Safety"), selected = vals$type)),
          column(4, numericInput(paste0("ep_weight_", i), "Weight", value = vals$weight, min = 0, step = 0.5))
        ),
        fluidRow(
          column(6, numericInput(paste0("ep_int_", i), "Intercept (b0)", value = vals$intercept, step = 0.1)),
          column(6, numericInput(paste0("ep_slope_", i), "Slope (b1)", value = vals$slope, step = 0.001)) # smaller step for small slopes
        )
      )
    })
  })
  
  # 5. core computation trigger
  
  # collect inputs (reactive)
  endpoint_params <- reactive({
    req(input$n_endpoints)
    # ensure ui is ready before collecting inputs
    req(input$ep_name_1)
    
    lapply(1:input$n_endpoints, function(i) {
      list(
        name = input[[paste0("ep_name_", i)]],
        type = input[[paste0("ep_type_", i)]],
        intercept = input[[paste0("ep_int_", i)]],
        slope = input[[paste0("ep_slope_", i)]],
        weight = input[[paste0("ep_weight_", i)]]
      )
    })
  })
  
  # main calculation (event reactive)
  cus_data <- eventReactive(input$calc_btn, {
    req(input$pk_min, input$pk_max, endpoint_params())
    
    # validation: check for null or na
    valid_eps <- endpoint_params()
    if (any(sapply(valid_eps, function(x) is.null(x$intercept) || is.na(x$intercept)))) return(NULL)
    
    generate_cus_data(input$pk_min, input$pk_max, valid_eps)
  })
  
  return(list(
    data = cus_data,
    inputs = endpoint_params,
    pk_ref = reactive(input$pk_ref)
  ))
}
