read_input_file <- function(filepath) {
  ext <- tools::file_ext(filepath)

  result <- tryCatch({
    if (ext == "csv") {
      read.csv(filepath)
    } else if (ext == "xlsx") {
      readxl::read_xlsx(filepath)
    } else if (ext == "xls") {
      readxl::read_xls(filepath)
    } else {
      stop(".", ext, " is not a supported file type")
    }
  }, error = function(err) {
    stop("Error reading file: ", err$message)
  })

  if (is.data.frame(result) && nrow(result) == 0) {
    stop("File does not contain any data")
  }

  result
}

is_category <- function(x) {
  # Remove NAs first for efficiency, then check if all values are integers

  # If there are no na values, then it can't be a category
  if (sum(is.na(x)) == 0)
    return(FALSE)

  # If the vector is empty after removing NAs then it is not a category
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0)
    return(FALSE)

  # if there are non-numeric values, it is not a category
  if (!is.numeric(x_clean))
    return(FALSE)

  # Check if all values are equal to their integer representation
  all(x_clean == as.integer(x_clean))
}

outputComments <- function(
    ...,
    echo = getOption("ECHO_OUTPUT_COMMENTS", TRUE),
    sep = " ")
{
  isolate({
    argslist <- list(...)
    if (length(argslist) == 1) {
      text <- argslist[[1]]
    } else {
      text <- paste(argslist, collapse = sep)
    }

    # If this is called within a shiny app, try to get the active session
    # and write to the session's logger
    commentsLog <- function(x) invisible(NULL)
    session <- getDefaultReactiveDomain()
    if (!is.null(session) &&
        is.environment(session$userData) &&
        is.reactive(session$userData$commentsLog))
    {
      commentsLog <- session$userData$commentsLog
    }

    if (is.na(echo)) return()
    if (is.data.frame((text)))
    {
      con <- textConnection("outputString","w",local=TRUE)
      capture.output(print(text, digits = 3), file = con, type="output", split = FALSE)
      close(con)
      if (echo)
      {
        for (line in outputString) cat(line, "\n")
      }
      for (line in outputString) commentsLog(paste0(commentsLog(), "\n", line))
    } else {
      if (echo)
      {
        cat(text, "\n")
      }
      commentsLog(paste0(commentsLog(), "<br>", text))
    }
  })
}
