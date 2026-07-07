module_UI_sidebar_upload_data <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("ER_upload_data"), "Upload ER dataset:",
              accept = c(".csv", ".xlsx", "text/csv", "text/comma-separated-values", "text/plain", ".txt", ".rds")),
    helpText(
      "Data Requirements:",
      tags$ol(
        tags$li("Acceptable formats: .csv, .xlsx, .txt, .rds;"),
        tags$li("Include one PK column named ", tags$code("PK"), ";"),
        tags$li("(Optional) Include one dose column named ", tags$code("Dose"), ";"),
        tags$li("Efficacy/safety endpoints named ",
                tags$code("EFF1"), ", ", tags$code("EFF2"), ", ..., ",
                tags$code("SAFE1"), ", ", tags$code("SAFE2"), "... ",
                "(binary 0/1 for Logistic/Emax, or continuous values for Linear/Log-linear/Exponential)")
      )
    ),
    hr(),
    uiOutput(ns("ER_summary_ui"))
  )
}

module_server_sidebar_upload_data <- function(input, output, session, all_rv) {
  ns <- session$ns
  
  observeEvent(input$ER_upload_data, {
    req(input$ER_upload_data)
    req(all_rv$overall_setting$simu_or_not == 2)
    
    file_path <- input$ER_upload_data$datapath
    file_ext  <- tools::file_ext(file_path)
    
    new_data <- switch(
      file_ext,
      "csv"  = read.csv(file_path, header = TRUE),
      "txt"  = read.csv(file_path, header = TRUE),
      "xlsx" = readxl::read_excel(file_path),
      "rds"  = readRDS(file_path),
      { showNotification("Unsupported file type.", type = "error"); return(NULL) }
    )

    # validate the uploaded file is an ER dataset before using it.
    # without this, uploading e.g. a utility-parameter table (no PK / EFF / SAFE
    # columns) crashes the app downstream (pivot_longer / min(PK)).
    if (is.null(new_data) || !is.data.frame(new_data) || nrow(new_data) == 0) {
      showNotification("Uploaded file is empty or could not be read.", type = "error")
      return(NULL)
    }
    cols <- colnames(new_data)
    has_PK <- "PK" %in% cols
    has_endpoint <- any(grepl("^EFF", cols)) || any(grepl("^SAFE", cols))
    if (!has_PK || !has_endpoint) {
      missing_msg <- paste0(
        "This does not look like an ER dataset. ",
        if (!has_PK) "Missing a 'PK' column. " else "",
        if (!has_endpoint) "Missing efficacy/safety endpoint columns named EFF1, SAFE1, ... " else "",
        "Please upload a file that meets the data requirements."
      )
      showNotification(missing_msg, type = "error", duration = 10)
      return(NULL)
    }

    update_ER_settings(new_data, all_rv)
    showNotification("ER dataset uploaded successfully!", type = "message")

    # informational checks for continuous regression applicability (non-blocking)
    cont_msgs <- character(0)
    # mixed-type within a group: some endpoints binary, some continuous -> treated as continuous
    eff_cols  <- grep("^EFF",  colnames(new_data), value = TRUE)
    safe_cols <- grep("^SAFE", colnames(new_data), value = TRUE)
    check_mixed <- function(cols, label) {
      if (length(cols) < 2) return(NULL)
      flags <- sapply(cols, function(cc) is_binary_endpoint(new_data[[cc]]))
      if (any(flags) && !all(flags)) {
        return(paste0(label, " endpoints mix binary and continuous columns; the group is treated as continuous \u2014 only Linear/Log-linear/Exponential are available."))
      }
      NULL
    }
    cont_msgs <- c(cont_msgs, check_mixed(eff_cols, "Efficacy"), check_mixed(safe_cols, "Safety"))
    ep_cols <- c(eff_cols, safe_cols)
    if (any(new_data$PK <= 0, na.rm = TRUE)) {
      cont_msgs <- c(cont_msgs, "PK contains non-positive values; Log-linear regression will drop those rows.")
    }
    for (cc in ep_cols) {
      vals <- new_data[[cc]]
      uniq <- unique(vals[is.finite(vals)])
      if (length(uniq) <= 2 && all(uniq %in% c(0, 1))) {
        cont_msgs <- c(cont_msgs, paste0(cc, " looks binary (0/1); continuous regressions may be unsuitable \u2014 consider Logistic/Emax."))
      } else if (any(vals <= 0, na.rm = TRUE)) {
        cont_msgs <- c(cont_msgs, paste0(cc, " has non-positive values; Exponential regression will drop those rows."))
      }
      if (sum(is.finite(vals)) < 5) {
        cont_msgs <- c(cont_msgs, paste0(cc, " has very few observations; empirical-CDF mapping may be unreliable."))
      }
    }
    if (length(cont_msgs) > 0) {
      showNotification(
        HTML(paste0("<b>Note on continuous endpoints:</b><br>", paste(cont_msgs, collapse = "<br>"))),
        type = "warning", duration = 12
      )
    }
    
    output$ER_summary_ui <- renderUI({
      tagList(
        h6("Uploaded Dataset Summary:"),
        tags$table(
          class = "table table-bordered table-sm",
          style = "width:100%; border-collapse:collapse; border:1px solid #ccc; font-size:14px;",
          tags$thead(tags$tr(tags$th("Metric"), tags$th("Value"))),
          tags$tbody(
            tags$tr(tags$td("Sample size"), tags$td(nrow(new_data))),
            tags$tr(tags$td("PK Minimum Value"), tags$td(round(min(new_data$PK, na.rm=TRUE), 1))),
            tags$tr(tags$td("PK Maximum Value"), tags$td(round(max(new_data$PK, na.rm=TRUE), 1))),
            tags$tr(tags$td("# Efficacy Endpoints"), tags$td(length(grep("^EFF", colnames(new_data))))),
            tags$tr(tags$td("# Safety Endpoints"), tags$td(length(grep("^SAFE", colnames(new_data)))))
          )
        )
      )
    })
  })
}