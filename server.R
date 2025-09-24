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
    progress <- shiny::Progress$new(session, style = "notification")
    progress$set(message = "Processing Trial:", value = 0)
    on.exit(progress$close())

    data <- data_validated()
    start_time <- Sys.time()
    trials <- unique(data$TRIAL)
    LengthTrials <- length(trials)
    CategoryNames <- classify_cols(data)$cat
    result_list <- lapply(seq(LengthTrials), function(idx) {
      trial <- trials[idx]
      trial_data <- data[data$TRIAL == trial, ]
      result <- P_Calc(trial_data, CategoryNames)
      progress$set(
        value = idx / LengthTrials,
        detail = paste0(" ", trial, "P = ", result$PLE[nrow(result)-1])
      )
      result
    })
    result <- do.call(rbind, result_list)

    outputComments("Execution time", round(Sys.time() - start_time, 2), "seconds")
    result
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
