validate_data <- function(raw_data) {
  if (is.null(raw_data)) {
    return()
  }

  tryCatch({
    data <- raw_data
    data <- standardize_colnames(data)
    data <- verify_cols(data)
    data <- add_missing_cols(data)
    data <- validate_cols(data)
    data <- setup_rounding_cols(data)
    column_types <- classify_cols(data)
    if (length(column_types$cat) > 0) {
      outputComments("Category Names:", toString(column_types$cat))
    }
    data <- validate_row_consistency(data, column_types$cat)
    data <- finalize_data(data, column_types)
    data
  }, error = function(err) {
    stop(err$message)
    NULL
  })
}

#' Standardize column names: convert to uppercase and trim whitespace
standardize_colnames <- function(data) {
  names(data) <- toupper(trimws(names(data)))
  outputComments("Column names:", toString(names(data)))
  data
}

#' Add TRIAL column if missing, otherwise standardize existing TRIAL column name
verify_cols <- function(data) {
  trial_idx <- grep("TRIAL", names(data))
  if (length(trial_idx) == 0) {
    data$TRIAL <- 1
  }
  names(data)[trial_idx[1]] <- "TRIAL"
  data
}

#' Map legacy column names from Carlisle 2016 format to standard names
#' Handles MEASURE->ROW, DECM->ROUND_MEAN, NUMBER->N, GROUP->ROW mappings
#' and removes obsolete columns
add_missing_cols <- function(data) {
  measure_idx  <- grep("MEASURE", names(data))
  if (length(measure_idx) > 0) {
    names(data)[measure_idx[1]] <- "ROW"
    data$GROUP <- NULL
    data$DECSD <- NULL
  }
  decm_idx <- grep("DECM", names(data))
  if (length(decm_idx) > 0) {
    names(data)[decm_idx[1]] <- "ROUND_MEAN"
  }
  number_idx <- grep("NUMBER", names(data))
  if (length(number_idx) > 0) {
    names(data)[number_idx[1]] <- "N"
  }
  if (length(grep("ROW", names(data))) == 0) {
    group_idx <- grep("GROUP", names(data))
    if (length(group_idx) > 0) {
      names(data)[group_idx[1]] <- "ROW"
    }
  }
  data
}

#' Check that all required columns (ROW, N, MEAN, SD) exist
#' Throws an error if there are missing columns
validate_cols <- function(data) {
  missing_cols <- character(0)

  row_idx <- grep("ROW", names(data))
  if (length(row_idx) == 0) {
    missing_cols <- c(missing_cols, "ROW")
  } else {
    names(data)[row_idx[1]] <- "ROW"
  }

  for (col in COLS_CONTINUOUS) {
    if (is.null(data[[col]])) {
      missing_cols <- c(missing_cols, col)
    }
  }

  if (length(missing_cols) > 0) {
    stop("Missing required columns:", toString(missing_cols))
  }

  data
}

#' Create and configure ROUND_MEAN and ROUND_OBSERVATION columns
setup_rounding_cols <- function(data) {
  mean_idx <- grep("MEAN", names(data))
  round_mean_idx <- which(names(data)[mean_idx] != "MEAN")

  if (length(round_mean_idx) > 0) {
    names(data)[mean_idx[round_mean_idx[1]]] <- "ROUND_MEAN"
  } else if (!is.null(data$ROUND)) {
    names(data)[names(data) == "ROUND"] <- "ROUND_MEAN"
  } else {
    observation_idx <- grep("OBS", names(data))
    if (length(observation_idx) > 0) {
      names(data)[observation_idx[1]] <- "ROUND_OBSERVATION"
      data$ROUND_MEAN <- data$ROUND_OBSERVATION
    }
  }
  if (is.null(data$ROUND_MEAN)) {
    data$ROUND_MEAN <- 0
  }

  observation_idx <- grep("OBS", names(data))
  if (length(observation_idx) == 0) {
    data$ROUND_OBSERVATION <- data$ROUND_MEAN
  } else {
    names(data)[observation_idx[1]] <- "ROUND_OBSERVATION"
  }

  data
}

#' Classify extra columns as categorical vs misc
classify_cols <- function(data) {
  extra_cols <- setdiff(names(data), COMMON_COL_NAMES)

  if (length(extra_cols) == 0) {
    return(list(cat = NULL, misc = NULL))
  }

  categories <- sapply(extra_cols, function(name) is_category(data[[name]]))
  categorical <- names(categories[categories == TRUE])
  misc <- names(categories[categories == FALSE])

  list(cat = categorical, misc = misc)
}

#' Validate that each row contains either categorical or continuous data, not both
validate_row_consistency <- function(data, cols_category) {
  errors_found <- FALSE

  for (i in seq_len(nrow(data))) {
    has_categorical_data <- any(!is.na(data[i, cols_category, drop = FALSE]))
    has_continuous_data <- any(!is.na(data[i, COLS_CONTINUOUS]))

    if (has_categorical_data) {
      # Row has categorical data - continuous columns should be NA
      data$ROUND_MEAN[i] <- NA
      data$ROUND_OBSERVATION[i] <- NA

      if (has_continuous_data) {
        report_mixed_data_error(data, i)
        errors_found <- TRUE
      }
    } else {
      # Row should have complete continuous data
      if (any(is.na(data[i, COLS_CONTINUOUS]))) {
        report_missing_continuous_error(data, i)
        errors_found <- TRUE
      }

      # Fix MEAN decimal places
      if (!is.na(data$MEAN[i]) && data$MEAN[i] != as.integer(data$MEAN[i])) {
        digits <- nchar(sub("^.*\\.", "", as.character(data$MEAN[i])))
        if (data$ROUND_MEAN[i] < digits) {
          data$ROUND_MEAN[i] <- digits
        }
      }
    }
  }

  if (errors_found) {
    stop("There are errors in the data table. Please review the above messages.")
  }

  data
}

report_mixed_data_error <- function(data, row_idx) {
  outputComments("Please look at line", row_idx + 1)
  outputComments("This appears to be a category. However, it has entries for continuous variables.")

  messages <- lapply(COLS_CONTINUOUS, function(col) {
    val <- data[row_idx, col]
    if (!is.na(val)) {
      paste(col, "=", val)
    }
  })

  outputComments("Specifically:", toString(unlist(messages)))
}

report_missing_continuous_error <- function(data, row_idx) {
  outputComments("Please look at line", row_idx + 1)
  outputComments("This appears to be a continuous variable. However, it has NA entries for required fields.")

  messages <- lapply(COLS_CONTINUOUS, function(col) {
    val <- data[row_idx, col]
    if (is.na(val)) {
      paste(col, "= NA")
    }
  })
  outputComments("Specifically:", toString(unlist(messages)))
}

#' Final data formatting: order columns, sort rows, report summary
finalize_data <- function(data, column_types) {
  final_cols <- c(COMMON_COL_NAMES, column_types$cat, column_types$misc)
  data <- data[, final_cols]
  data <- data[order(data$TRIAL, data$ROW), ]

  trials <- unique(data$TRIAL)
  outputComments("# of trials:", length(trials))

  data
}
