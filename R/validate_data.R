validate_data <- function(raw_data) {
  if (is.null(raw_data)) {
    return()
  }
  FAIL <- FALSE
  DATA <- raw_data

  names(DATA) <- toupper(trimws(names(DATA)))
  ColumnNames <- names(DATA)
  outputComments("Column names:", paste(ColumnNames, collapse = ", "))

  # Add trial number if necessary
  trials <- grep("TRIAL", ColumnNames)
  if (length(trials) == 0)
  {
    DATA$TRIAL <- 1
  }
  names(DATA)[trials[1]] <- "TRIAL"
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
  CategoryNames <- ColumnNames[!ColumnNames %in% COMMON_COL_NAMES]
  categories <- sapply(CategoryNames, function(name) is_category(DATA[, name]))
  CategoryNames <- names(categories[categories == TRUE])
  MiscNames <- names(categories[categories == FALSE])

  if (length(CategoryNames) == 0)
  {
    CategoryNames <- NULL
  } else {
    outputComments("Category Names:", toString(CategoryNames), "\n")
  }

  # Validate each line
  for (i in 1:nrow(DATA))
  {
    if (any(!is.na(DATA[i, CategoryNames]))) # If there is any category entry, continuous columns are set to NA
    {
      DATA$ROUND_MEAN[i] <- DATA$ROUND_OBSERVATION[i] <- NA
      if (any(!is.na(DATA[i, c("N", "MEAN", "SD")])))
      {
        outputComments("Please look at line", i+1)
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
        outputComments("This appears to be a category. However, it has entries for continuous variables.")
        outputComments("Specifically:", message)
        FAIL <- TRUE
      }
    } else {
      if (any(is.na(DATA[i, c("N", "MEAN", "SD")])))
      {
        outputComments("Please look at line", i+1)
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
        outputComments("This appears to be a continuous variable. However, it has NA entries for required fields.")
        outputComments("Specifically:", message)
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
  DATA <- DATA[, c(COMMON_COL_NAMES, CategoryNames, MiscNames)]
  DATA <- DATA[order(DATA$TRIAL, DATA$ROW),]
  trials <- unique(DATA$TRIAL)

  outputComments("# of trials:", length(trials))

  DATA
}
