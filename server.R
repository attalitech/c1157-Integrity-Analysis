function(input, output, session) {
  reactiveData <- reactiveVal()
  reactiveDataValidated <- reactiveVal()
  reactiveResults <- reactiveVal()
  reactiveDone <- reactiveVal(FALSE)
  output$downloadButton <- NULL
  output$logContent <- NULL
  output$GoButton <- NULL
  OUTPUT <- NULL
  stopImplicitCluster()
#  cores <- detectCores() - 1  # Use one less than available cores
#  cluster <- makeCluster(cores)
#  registerDoParallel(cluster)

  output$stopButton <-
    renderUI({
      fluidRow(
        column(
          12,
          br(),
          actionBttn("stop", HTML("&nbsp &nbsp EXIT &nbsp &nbsp"), style = "gradient", size = "xs", color = "warning"),
          br()
        )
      )
    })


  # Write out logs to the log section
  initLogMsg <- "Comments Log"
  commentsLog <- reactiveVal(NULL)
  output$logContent <- renderUI({
    invalidateLater(1000)
    HTML(commentsLog())
  })
  # Register the comments log with this user's session, to use outside the server
  session$userData$commentsLog <- commentsLog

  is_category <- function(x) {
    # Remove NAs first for efficiency, then check if all values are integers

    # If there are no na values, then it can't be a category
    if (sum(is.na(x)) == 0)
      return(FALSE)

    # If the vector is empty after removing NAs then it is not a category
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0)
      return(FALSE)

    # Check if all values are equal to their integer representation
    all(x_clean == as.integer(x_clean))
  }

  ###########################################################
  # Primary Statistical Function for Monte Carlo Simulation #
  ###########################################################

  P_Calc <- function(TRIAL)
  {
    data <- DATA[DATA$TRIAL == TRIAL,]
    RowIDs <- unique(data$ROW)

    x <- foreach(
      j = 1:length(RowIDs),
      .combine = rbind
      #  .options.future = list(seed = TRUE)
      #.export = c("CategoryNames", "m"),
      #.packages = c("Rfast", "dqrng")
    ) %do%
      {
        Row <- RowIDs[j]
        ROWS <- data[data$ROW == Row,]

        # Greater than 1 line?
        if (nrow(ROWS) > 1)
        {
          # Is this categorical?
          if (all(!is.na(ROWS$N)))
          {
            COLS <- nrow(ROWS)
            N <- sum(ROWS$N)
            Meanmean <- sum(ROWS$N*ROWS$MEAN) / N
            # The calculation of Meanvar is OK. SD^2 is an unbiased estimate
            # of variance
            Meanvar <-  sum(ROWS$N*ROWS$SD^2) / N

            # However, this next calculatiion is biased. s.u. will correct it
            # If N > 30, then the correction is < 1 %. It blows up if N > 343!
            if (N < 30)
            {
              Meansd <- s.u(sqrt(Meanvar), N)
            } else {
              Meansd <- sqrt(Meanvar)
            }
            # Protect size of simulation
            if ((m*N) < 1000000000) # One billion
            {
              m1 <- m
            } else {
              m1 <- 1000000000 / N
            }
            SEMsample <- Meansd/sqrt(mean(ROWS$N))
            DiffSample <- sum((ROWS$MEAN - Meanmean)^2) # Squared difference of column means
            # Monte Carlo Simulation
            meansim <- dqrnorm(m,mean=Meanmean,sd=SEMsample) # Generate a new mean for each simulation
            MonteCarloMean <- matrix(NA, nrow = m1, ncol = COLS) # I want one row for each simulation
            # Need to do each column separately. Couldn't think of an efficient way to do this without
            # a loop.
            for (i in 1:COLS)
              MonteCarloMean[,i] <-
              round(
                rowmeans(
                  round(
                    # The matrix below will have one row for each replication (m rows),
                    # and one column for each person (N[i] columns)
                    # Cannot use dqrnorm because it won't support the array
                    # of meansim needed for each replication
                    matrix(
                      rnorm(ROWS$N[i] * m1, rep(meansim, ROWS$N[i]), Meansd),
                      nrow = m1, byrow = FALSE
                    ),
                    ROWS$ROUND_OBSERVATION[i]
                  )
                ),
                ROWS$ROUND_OBSERVATION[i]
              )
            N <- matrix(ROWS$N, nrow = m1, ncol = COLS, byrow = TRUE)
            # Calculate the weighted mean, and then round
            MeanSamples <- rowsums (MonteCarloMean * N) / sum(ROWS$N)
            DiffSamples <- rowsums((MonteCarloMean - MeanSamples)^2)

            PEQ <- sum(DiffSamples == DiffSample) / m1
            PLE <- sum(DiffSamples < DiffSample)/m1 + PEQ
            PGE <- sum(DiffSamples > DiffSample)/m1 + PEQ
          } else {
            ROWS <- ROWS[,CategoryNames]
            for (NAME in CategoryNames)
            {
              if (all(is.na(ROWS[,NAME])))
                ROWS[,NAME] <- NULL
            }
            PLE <- chisq.test(ROWS, simulate.p.value=m)$p.value
            PGE <- 1-PLE
          }
          # Need to be sure P != 0 or 1
          if(PLE == 1) PLE <- 0.999
          if(PLE == 0) PLE <- 0.001
          if(PGE == 1) PGE <- 0.999
          if(PGE == 0) PGE <- 0.001
          PLE = as.character(signif(PLE,4))
          PGE = as.character(signif(PGE, 4))
        } else {
          PLE = "Only 1 Row"
          PGE = NA
        }

        c(as.character(Row), PLE, PGE)
      } %seed% TRUE

    # This bizarre code is because if there is only 1 row, R creates a data.frame
    # with 3 columns and 1 row.
    if (length(x) == 3)
    {
      x <- as.data.frame(t(x))
    } else {
    x <- as.data.frame(x)
    }

    x <- cbind(NA, x)
    x[1,1] <- TRIAL
    x <- as.data.frame(x)
    names(x) <- c("TRIAL", "ROW", "PLE", "PGE")
    cat("Row IDs", RowIDs, "\n")
    print(x)
    cat("match results", match(x$ROW, RowIDs), "\n")
    x <- x[match(x$ROW, RowIDs),]
    print(x)

    PLEvalues <- as.numeric(x$PLE)
    PGEvalues <- as.numeric(x$PGE)

    PLEvalues <- PLEvalues[!is.na(PLEvalues)]
    PGEvalues <- PGEvalues[!is.na(PGEvalues)]

    if (length(PLEvalues) > 1)
    {
      PLE <- signif(sumz(PLEvalues)$p,4)
    } else {
      if (length(PLEvalues == 1))
        PLE <- PLEvalues
      if (length(PLEvalues)==0)
        PLE = "No values"
    }

    if (length(PGEvalues) > 1)
    {
      PGE <- signif(sumz(PGEvalues)$p,4)
    } else {
      if (length(PGEvalues == 1))
        PGE <- PGEvalues
      if (length(PGEvalues)==0)
        PGE = "No values"
    }

    lastline <- data.frame(
      TRIAL = c(NA, NA),
      ROW = c("Summary", NA),
      PLE = c(as.character(PLE), NA),
      PGE = c(as.character(PGE), NA)
    )

    x <- rbind(x, lastline)
    outputComments(
      paste0("Trial ", TRIAL,": p = ", PLE, "\n")
    )
    return(x)
  }

  ###########################################################
  # Processing Loop                                         #
  ###########################################################

  observeEvent(
    {
      input$go
    },
    {
      output$stopButton <- NULL
      progress <- shiny::Progress$new(session, style = "notification")
      on.exit(progress$close())
      DATA <<- reactiveDataValidated()
      start_time <- Sys.time()
      progress$set(message = "Processing Trial:", value = 0)
      cores <- detectCores() - 1
      registerDoParallel(cores)
      LengthTrials <- length(TRIALS)
      for (i in 1:LengthTrials)
      {
        TRIAL <- TRIALS[i]
        OUTPUT <<- rbind(
          OUTPUT,
          P_Calc(TRIAL)
        )
        progress$set(
          value = i / LengthTrials,
          detail = paste0(" ",TRIAL, "P = ",OUTPUT$PLE[nrow(OUTPUT)-1]))
      }
      # Not sure which is correct
      with(registerDoFuture(), local = TRUE)
      output$stopButton <-
        renderUI({
          fluidRow(
            column(
              12,
              br(),
              actionBttn("stop", HTML("&nbsp &nbsp EXIT &nbsp &nbsp"), style = "gradient", size = "xs", color = "warning"),
              br()
            )
          )
        })
    outputComments(paste("Execution time", round(Sys.time() - start_time, 2)))
    reactiveDone(TRUE)
    }
  )


  ###########################################################
  # Upload Data Routines                                    #
  ###########################################################

  observeEvent(
    {
      input$upload
    },
    {
      reactiveResults(NULL)
      reactiveDone(FALSE)
      commentsLog(NULL)

      Filename <- input$upload$datapath
      ext <- tools::file_ext(Filename)

      # Switch statement started to fail parsing... ????
      if (!ext %in% c("csv", "xlsx", "xls"))
      {
        outputComments(
          paste0(".",ext,"is not a supported file type."
          )
        )
        return()
      }

      if (ext == "csv")
      {
        DATA <<- read.csv(Filename)
        reactiveData(DATA)
        return()
      }
      if (ext == "xlsx")
      {
        DATA <<- read.xlsx(Filename)
        reactiveData(DATA)
        return()
      }
      if (ext == "xls")
      {
        DATA <<- read.xl(Filename)
        reactiveData(DATA)
        return()
      }
    }
  )

  observeEvent(
    {
      reactiveData()
    },
    {
      FAIL <- FALSE
      DATA <- reactiveData()
      if (is.null(DATA))
      {
        return()
      }

      names(DATA) <- toupper(trimws(names(DATA)))
      ColumnNames <- names(DATA)
      outputComments(paste("Column names:", paste(ColumnNames, collapse = ", ")))

      # Add trial number if necessary
      TRIALS <- grep("TRIAL", ColumnNames)
      if (length(TRIALS) == 0)
      {
        DATA$TRIAL <- 1
      }
      names(DATA)[TRIALS[1]] <- "TRIAL"
      ColumnNames <- names(DATA)

      ################################################
      # Adjust names to accept Carlisle 2016 input file
      MEASURES <- grep("MEASURE", ColumnNames)
      if (length(MEASURES) > 0)
      {
        names(DATA)[MEASURES[1]] <- "ROW"
        DATA$GROUP <- NULL
        DATA$DECSD <- NULL
        ColumnNames <- names(DATA)
      }
      DECMS <- grep("DECM", ColumnNames)
      if (length(DECMS) > 0)
      {
        names(DATA)[DECMS[1]] <- "ROUND_MEAN"
        ColumnNames <- names(DATA)
      }
      NUMBERS <-grep("NUMBER", names(DATA))
      if (length(NUMBERS) > 0)
      {
        names(DATA)[NUMBERS[1]] <- "N"
        ColumnNames <- names(DATA)
      }
      if (length(grep("ROW", ColumnNames)) == 0)
      {
        GROUPS <- grep("GROUP", ColumnNames)
        if (length(GROUPS)> 0)
        {
          names(DATA)[GROUPS[1]] <- "ROW"
        }
      }

      ColumnNames <- names(DATA)

      ##############################################

      # Verify that the necessary rows are in place
      RowColumn <- grep("ROW", ColumnNames)
      if (length(RowColumn) == 0)
      {
        outputComments("Missing column labeled ROW")
        FAIL <- TRUE
      } else {
        names(DATA)[RowColumn[1]] <- "ROW"
        ColumnNames <- names(DATA)
      }

      if (is.null(DATA$N))
      {
        outputComments("Missing column labeled N")
        FAIL <- TRUE
      }

      if (is.null(DATA$MEAN))
      {
        outputComments("Missing column labeled MEAN")
        FAIL <- TRUE
      }
      if (is.null(DATA$SD))
      {
        outputComments("Missing column labeled SD")
        FAIL <- TRUE
      }

      # Add rounding column for the mean
      MeanColumns <- grep("MEAN", ColumnNames)
      RoundMeanColumn <- which(ColumnNames[MeanColumns] != "MEAN")
      if (length(RoundMeanColumn) > 0)
      {
        names(DATA)[MeanColumns[RoundMeanColumn[1]]] <- "ROUND_MEAN"
        ColumnNames <- names(DATA)
      } else {
        if (!is.null(DATA$ROUND))
        {
          names(DATA)[names(DATA) == "ROUND"] <- "ROUND_MEAN"
        } else {
          ObservationColumns <- grep("OBS", ColumnNames)
          if (length(ObservationColumns) > 0)
          {
            names(DATA)[ObservationColumns[1]] <- "ROUND_OBSERVATION"
            DATA$ROUND_MEAN <- DATA$ROUND_OBSERVATION
          }
        }
      }
      # After all of that, if it still doesn't exist, just put in 0
      if (is.null(DATA$ROUND_MEAN))
      {
        DATA$ROUND_MEAN <- 0
      }
      ColumnNames <- names(DATA)

      ObservationColumns <- grep("OBS", ColumnNames)
      if (length(ObservationColumns) == 0)
      {
        DATA$ROUND_OBSERVATION <- DATA$ROUND_MEAN
      } else {
        names(DATA)[ObservationColumns[1]] <- "ROUND_OBSERVATION"
      }
      ColumnNames <- names(DATA)

      # Validate Categories
      CategoryNames <-
        ColumnNames[!ColumnNames %in% c("TRIAL", "ROW", "MEAN","N", "SD", "ROUND_OBSERVATION", "ROUND_MEAN")]
      MiscNames <- NULL
      if (length(CategoryNames) == 0)
      {
        CategoryNames <- NULL
      } else {
        for (i in 1:length(CategoryNames))
        {
          if (!is_category(DATA[,CategoryNames[i]]))
          {
            MiscNames <- c(MiscNames, CategoryNames[i])
            CategoryNames[i] <- "XXXXX"
          }
        }
        CategoryNames <- CategoryNames[CategoryNames != "XXXXX"]
      }

      if (length(CategoryNames) == 0)
      {
        CategoryNames <- NULL
      } else {
        outputComments(paste("Category Names", paste(CategoryNames, collapse=", "), "\n"))
      }

      # Validate each line
      for (i in 1:nrow(DATA))
      {
        if (any(!is.na(DATA[i, CategoryNames]))) # If there is any category entry, continuous columns are set to NA
        {
          DATA$ROUND_MEAN[i] <- DATA$ROUND_OBSERVATION[i] <- NA
          if (any(!is.na(DATA[i, c("N", "MEAN", "SD")])))
          {
            outputComments(paste("Please look at line", i+1))
            message <- NULL
            if (!is.na(DATA$N[i])) message <- paste(message, "N = ", DATA$N[i])
            if (!is.na(DATA$MEAN[i]))
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "MEAN = ", DATA$MEAN[i])
            }
            if (!is.na(DATA$SD[i]))
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "SD = ", DATA$SD[i])
            }
            outputComments(paste("This appears to be a category. However, it has entries for continuous variables."))
            outputComments(paste("Specifically: ", message))
            FAIL <- TRUE
          }
        } else {
          if (any(is.na(DATA[i, c("N", "MEAN", "SD")])))
          {
            outputComments(paste("Please look at line", i+1))
            message <- NULL
            if (is.na(DATA$N[i])) message <- paste(message, "N = ", DATA$N[i])
            if (is.na(DATA$MEAN[i]))
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "MEAN = ", DATA$MEAN[i])
            }
            if (is.na(DATA$SD[i]))
            {
              if (!is.null(message)) message <- paste0(message, ", ")
              message <- paste(message, "SD = ", DATA$SD[i])
            }
            outputComments(paste("This appears to be a continuous variable. However, it has NA entries for required fields."))
            outputComments(paste("Specifically: ", message))
            FAIL <- TRUE
          }
          # Fix MEAN digits if Mean has any decimal digits
          if (DATA$MEAN[i] != as.integer(DATA$MEAN[i]))
          {
            digits <- nchar(sub("^.*\\.", "", as.character(DATA$MEAN[i])))
            if (DATA$ROUND_MEAN[i] < digits) DATA$ROUND_MEAN[i] <- digits
          }
        }
      }

      if (FAIL)
      {
        outputComments("There are one or more errors in the data table. Please review the above messages to address these.")
        return()
      }
      DATA <- DATA[,c("TRIAL", "ROW", "N", "MEAN", "SD",  "ROUND_MEAN", "ROUND_OBSERVATION", CategoryNames, MiscNames)]
      DATA <- DATA[order(DATA$TRIAL, DATA$ROW),]
      TRIALS <- unique(DATA$TRIAL)

      # Assign globally
      DATA <<- DATA
      TRIALS <<- TRIALS
      ColumnNames <<- ColumnNames
      CategoryNames <<- CategoryNames

      LengthTrials <- length(TRIALS)
      cat("length trials: ",length(TRIALS), "\n")
      if (LengthTrials == 1)
      {
        output$downloadButton <- renderUI({
          input_task_button(
            "go", HTML("&nbsp &nbsp Analyze Trial &nbsp &nbsp"), style = "gradient", size = "xs", color = "success")
        })
      } else {
        output$downloadButton <- renderUI({
          HTML("<br>")
          input_task_button(
            "go", HTML(
              paste(
                "&nbsp &nbsp Analyze", LengthTrials, "Trials &nbsp &nbsp")))
        })
      }
      # Set reactive value
      reactiveDataValidated(DATA)

    }
  )



  observeEvent(
    {
      reactiveDone()
    },
    {
      DONE <- reactiveDone()
      if (!DONE)
      {
        output$downloadButton <- NULL
      } else {
        output$downloadButton <- renderUI({
          downloadButton("download", "Download Results")
          })
      }
    }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste0("Integrity Analysis.",format(Sys.time(), format = "%y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) {
      x <- OUTPUT
      names(x) <- c("TRIAL", "ROW", "Fraction <=", "Fraction >=")
      write.xlsx(x, file)
    })

  observeEvent(input$stop, {
    stopApp(returnValue = invisible())
  })
}


