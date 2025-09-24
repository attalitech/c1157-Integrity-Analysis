function(input, output, session) {

  commentsLog <- reactiveVal(NULL)
  output$logContent <- renderUI({
    HTML(commentsLog())
  })
  observe({
    shinyjs::toggle("logContent", condition = !is.null(commentsLog()))
  })
  # Register the comments log with this user's session, to use outside the server
  session$userData$commentsLog <- commentsLog

  raw_data <- reactive({
    req(input$upload)

    commentsLog(NULL)

    result <- tryCatch({
      read_input_file(input$upload$datapath)
    }, error = function(err) {
      outputComments(err$message)
      NULL
    })

    result
  })

  data_validated <- reactive({
    validate_data(raw_data())
  })

  observeEvent(data_validated(), ignoreNULL = FALSE, {
    shinyjs::toggle("analyze", condition = !is.null(data_validated()))
    shinyjs::hide("download_results")
  })

  analysis_result <- eventReactive(input$analyze, {
    data <- data_validated()
    start_time <- Sys.time()
    trials <- unique(data$TRIAL)
    CategoryNames <- classify_cols(data)$cat

    progressr::withProgressShiny(
      message = "Processing",
      detail = "Setting up ...",
      value = 0, {
        p <- progressr::progressor(along = trials)

        process_function <- function(trial) {
          trial_data <- data[data$TRIAL == trial, ]
          result <- P_Calc(trial_data, CategoryNames)
          p(paste0(trial, " (P = ", result$PLE[nrow(result) - 1], ")"))
          result
        }

        func_name <- lapply
        func_args <- list(X = trials, FUN = process_function)

        if (RUN_PARALLEL) {
          func_name <- future.apply::future_lapply
          func_args$future.seed <- TRUE
        }

        results <- do.call(func_name, func_args)
      }
    )
    results <- do.call(rbind, results)

    outputComments(
      paste0(
        "Trial ",
        unique(na.omit(results$TRIAL)),
        ": p = ",
        results$PLE[results$ROW == "Summary" & !is.na(results$ROW)],
        collapse = "<br>"
      )
    )

    outputComments("Execution time", round(as.integer(Sys.time()) - as.integer(start_time), 2), "seconds")
    results
  })

  observeEvent(analysis_result(), {
    shinyjs::hide("analyze")
    shinyjs::show("download_results")
  })

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("Integrity Analysis.",format(Sys.time(), format = "%y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      x <- analysis_result()
      names(x) <- c("TRIAL", "ROW", "Fraction <=", "Fraction >=")
      openxlsx::write.xlsx(x, file)
    }
  )

  observeEvent(input$stop, {
    shinyjs::runjs("window.close()")
    session$close()
  })
}
