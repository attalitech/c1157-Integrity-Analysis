function(input, output, session) {
  analysisDone <- reactiveVal(FALSE)
  analysisOutput <- reactiveVal()

  stopImplicitCluster()
  #  cores <- detectCores() - 1  # Use one less than available cores
  #  cluster <- makeCluster(cores)
  #  registerDoParallel(cluster)

  commentsLog <- reactiveVal(NULL)
  output$logContent <- renderUI({
    HTML(commentsLog())
  })
  observe({
    shinyjs::toggle("logContent", condition = !is.null(commentsLog()))
  })
  # Register the comments log with this user's session, to use outside the server
  session$userData$commentsLog <- commentsLog

  ###########################################################
  # Processing Loop                                         #
  ###########################################################

  observeEvent(
    {
      input$analyze
    },
    {
      progress <- shiny::Progress$new(session, style = "notification")
      on.exit(progress$close())
      data <- data_validated()
      start_time <- Sys.time()
      progress$set(message = "Processing Trial:", value = 0)
      cores <- detectCores() - 1
      registerDoParallel(cores)
      trials <- unique(data$TRIAL)
      LengthTrials <- length(trials)
      result <- NULL
      for (i in 1:LengthTrials)
      {
        TRIAL <- trials[i]
        result <- rbind(
          result,
          P_Calc(data, TRIAL)
        )
        progress$set(
          value = i / LengthTrials,
          detail = paste0(" ",TRIAL, "P = ",result$PLE[nrow(result)-1]))
      }
      analysisOutput(result)
      # Not sure which is correct
      with(registerDoFuture(), local = TRUE)

      outputComments("Execution time", round(Sys.time() - start_time, 2))
      analysisDone(TRUE)
    }
  )

  raw_data <- reactive({
    req(input$upload)

    analysisDone(FALSE)
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
  })

  observeEvent(analysisDone(), {
    if (analysisDone()) {
      shinyjs::hide("analyze")
    }
    shinyjs::toggle("download_results", condition = analysisDone())
  })

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("Integrity Analysis.",format(Sys.time(), format = "%y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      x <- analysisOutput()
      names(x) <- c("TRIAL", "ROW", "Fraction <=", "Fraction >=")
      openxlsx::write.xlsx(x, file)
    }
  )

  observeEvent(input$stop, {
    shinyjs::runjs("window.close()")
    session$close()
  })
}
