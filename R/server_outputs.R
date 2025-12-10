# r/server_outputs.R
# server module: output rendering

library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)

# ==============================================================================
# 1. helper functions: visualization & recalculation (pure functions)
# ==============================================================================

#' plot cus curve with component overlay
plot_cus_curve <- function(df, pk_ref) {
  if (is.null(df)) return(NULL)
  
  # find max cus
  max_row <- df[which.max(df$CUS), ]
  
  # reshape for individual components (dashed lines)
  df_long <- df %>%
    select(Exposure, starts_with("Prob_")) %>%
    pivot_longer(cols = starts_with("Prob_"), 
                 names_to = "Endpoint", 
                 values_to = "Probability") %>%
    mutate(Endpoint = sub("Prob_", "", Endpoint))
  
  p <- ggplot() +
    # layer 1: components (dashed)
    geom_line(data = df_long, aes(x = Exposure, y = Probability, color = Endpoint), 
              linetype = "dashed", size = 0.8, alpha = 0.7) +
    # layer 2: cus (solid)
    geom_line(data = df, aes(x = Exposure, y = CUS, color = "Total CUS"), 
              size = 1.5) +
    # layer 3: vertical lines
    geom_vline(xintercept = max_row$Exposure, linetype = "dotted", color = "#28a745", size = 0.8) +
    geom_vline(xintercept = pk_ref, linetype = "dotted", color = "#dc3545", size = 0.8) +
    # annotations
    annotate("text", x = max_row$Exposure, y = 1.02, 
             label = paste0("Optimal\n", round(max_row$Exposure, 2)), 
             color = "#28a745", size = 3.5, vjust = 0) +
    annotate("text", x = pk_ref, y = -0.02, 
             label = paste0("Ref\n", pk_ref), 
             color = "#dc3545", size = 3.5, vjust = 1) +
    # style
    scale_color_manual(values = c("Total CUS" = "black")) +
    labs(title = "Clinical Utility Score & Component Probabilities",
         subtitle = "Solid: CUS (Dynamic Weight) | Dashed: Component Probabilities",
         x = "Exposure (PK)", y = "Probability / Score", color = "Legend") +
    theme_minimal() +
    theme(legend.position = "right") +
    ylim(-0.05, 1.1)
  
  ggplotly(p)
}

plot_components_only <- function(df) {
  if (is.null(df)) return(NULL)
  
  df_long <- df %>%
    select(Exposure, starts_with("Prob_")) %>%
    pivot_longer(cols = starts_with("Prob_"), names_to = "Endpoint", values_to = "Probability") %>%
    mutate(Endpoint = sub("Prob_", "", Endpoint))
  
  p <- ggplot(df_long, aes(x = Exposure, y = Probability, color = Endpoint)) +
    geom_line(size = 1) +
    labs(title = "Individual Endpoint Probabilities",
         x = "Exposure (PK)", y = "Probability") +
    theme_minimal()
  
  ggplotly(p)
}

#' recalculate cus based on new weights (vectorized)
#' @param base_df data frame containing exposure and prob_* columns
#' @param inputs list of endpoint definitions (contains name and type)
#' @param new_weights numeric vector of new weights
recalculate_cus_data <- function(base_df, inputs, new_weights) {
  df <- base_df
  
  # 1. normalize weights
  if (sum(new_weights) == 0) new_weights <- rep(1, length(new_weights))
  norm_weights <- new_weights / sum(new_weights)
  
  # 2. vectorized cus calculation
  # we iterate through each exposure row
  
  # pre-calculate utility scores for all rows
  scores_list <- lapply(seq_along(inputs), function(i) {
    ep <- inputs[[i]]
    col_name <- paste0("Prob_", ep$name)
    prob_vec <- df[[col_name]]
    
    # apply linear utility logic: safety = 1 - p, efficacy = p
    if (ep$type == "Safety") {
      util <- pmax(1 - prob_vec, 1e-6)
    } else {
      util <- pmax(prob_vec, 1e-6)
    }
    return(util)
  })
  
  # 3. aggregate: exp(sum(weight_i * log(score_i)))
  log_scores_matrix <- do.call(cbind, lapply(scores_list, log))
  
  weighted_log_sum <- log_scores_matrix %*% norm_weights
  
  df$CUS <- as.numeric(exp(weighted_log_sum))
  
  return(df)
}

# ==============================================================================
# 2. server module: render logic
# ==============================================================================

servermod_output_render <- function(input, output, reactives) {
  
  # --- 1. main cus plot (dashboard tab) ---
  output$cus_plot <- renderPlotly({
    req(reactives$data())
    plot_cus_curve(reactives$data(), reactives$pk_ref())
  }) %>% bindCache(reactives$data(), reactives$pk_ref())
  
  # --- 2. component plot ---
  output$component_plot <- renderPlotly({
    req(reactives$data())
    plot_components_only(reactives$data())
  }) %>% bindCache(reactives$data())
  
  # --- 3. summary table ---
  output$optimal_summary <- renderTable({
    req(reactives$data())
    df <- reactives$data()
    max_idx <- which.max(df$CUS)
    
    data.frame(
      Metric = c("Optimal Exposure", "Max CUS Score", "Reference Exposure"),
      Value = c(
        round(df$Exposure[max_idx], 3),
        round(df$CUS[max_idx], 3),
        reactives$pk_ref()
      )
    )
  })
  
  # --- 4. weight sensitivity ui (dynamic) ---
  output$weight_sliders_ui <- renderUI({
    req(reactives$inputs())
    eps <- reactives$inputs()
    
    lapply(seq_along(eps), function(i) {
      sliderInput(paste0("sens_weight_", i), 
                  label = paste0(eps[[i]]$name, " (", eps[[i]]$type, ")"), 
                  min = 0, max = 5, 
                  value = eps[[i]]$weight,
                  step = 0.1)
    })
  })
  
  # --- 5. sensitivity logic: capture & debounce ---
  
  # a. capture slider values
  current_sensitivity_weights <- reactive({
    req(reactives$inputs())
    n <- length(reactives$inputs())
    
    weights <- numeric(n)
    for (i in 1:n) {
      val <- input[[paste0("sens_weight_", i)]]
      weights[i] <- if (is.null(val)) reactives$inputs()[[i]]$weight else val
    }
    weights
  })
  
  # b. debounce values
  debounced_weights <- current_sensitivity_weights %>% debounce(500)
  
  # --- 6. sensitivity plot (live update) ---
  output$sensitivity_plot <- renderPlotly({
    req(reactives$data(), reactives$inputs())
    
    base_df <- reactives$data()
    
    new_w <- debounced_weights()
    
    new_df <- recalculate_cus_data(base_df, reactives$inputs(), new_w)
    
    plot_cus_curve(new_df, reactives$pk_ref())
    
  })
  
  # --- 7. data table ---
  output$result_table <- renderDT({
    req(reactives$data())
    datatable(reactives$data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}
