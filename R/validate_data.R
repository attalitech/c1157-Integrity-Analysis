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
    outputComments(err$message)
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
    stop("Missing required columns: ", toString(missing_cols))
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
  # Rows that have any categorical columns
  rows_categorical <- rowSums(!is.na(data[, cols_category, drop = FALSE])) > 0
  # Rows that have any continuous columns
  rows_continuous <- rowSums(!is.na(data[, COLS_CONTINUOUS])) > 0
  # Rows that have both categorical and continuous columns
  rows_mixed <- rows_categorical & rows_continuous
  # Rows that have continuous but not categorical columns
  rows_continuous_only <- !rows_categorical & rows_continuous
  # Rows that don't have any missing data in continuous columns, and no categorical columns
  rows_continuous_only_not_full <- rows_continuous_only & rowSums(is.na(data[, COLS_CONTINUOUS])) > 0

  data$ROUND_MEAN[rows_categorical] <- NA
  data$ROUND_OBSERVATION[rows_categorical] <- NA

  # Fix MEAN decimal places
  has_decimals <- rows_continuous_only & !is.na(data$MEAN) & data$MEAN != floor(data$MEAN)
  decimal_idx <- which(has_decimals)
  if (length(decimal_idx) > 0) {
    digits <- nchar(sub("^.*\\.", "", as.character(data$MEAN[decimal_idx])))
    needs_update <- data$ROUND_MEAN[decimal_idx] < digits
    update_indices <- decimal_idx[needs_update]
    data$ROUND_MEAN[update_indices] <- digits[needs_update]
  }

  errors_found <- FALSE
  if (sum(rows_mixed) > 0) {
    errors_found <- TRUE
    report_mixed_data_error(data, which(rows_mixed))
  }
  if (sum(rows_continuous_only_not_full) > 0) {
    errors_found <- TRUE
    report_missing_continuous_error(data, which(rows_continuous_only_not_full))
  }
  if (errors_found) {
    stop("There are errors in the data table. Please review the above messages.")
  }

  data
}

report_mixed_data_error <- function(data, row_idx) {
  lapply(row_idx, function(idx) {
    outputComments("Please look at line", idx + 1)
    outputComments("This appears to be a category. However, it has entries for continuous variables.")

    messages <- lapply(COLS_CONTINUOUS, function(col) {
      val <- data[idx, col]
      if (!is.na(val)) {
        paste(col, "=", val)
      }
    })

    outputComments("Specifically:", toString(unlist(messages)))
  })
  invisible()
}

report_missing_continuous_error <- function(data, row_idx) {
  lapply(row_idx, function(idx) {
    outputComments("Please look at line", idx + 1)
    outputComments("This appears to be a continuous variable. However, it has NA entries for required fields.")

    messages <- lapply(COLS_CONTINUOUS, function(col) {
      val <- data[idx, col]
      if (is.na(val)) {
        paste(col, "= NA")
      }
    })
    outputComments("Specifically:", toString(unlist(messages)))
  })
  invisible()
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
