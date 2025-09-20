# Global.R
library(shiny)
library(openxlsx)
library(readxl)
library(metap)
library(Rfast)
library(shinyjs)
library(rsconnect)
#library(mirai)
#library(promises)
library(future)
library(foreach)
library(doParallel)
library(MBESS)
library(dqrng)
library(OpenMx)
library(digitTests)
library(rsconnect)
library(bslib)
library(shinydashboard)
library(doFuture)
#library(BiocManager)
#BiocManager::install("multtest")

registerDoFuture(flavor = "%dofuture%")
plan(multisession)

# replication number for the Monte Carlo simulation
#MONTE_CARLO_SIM_REPS <- 15000
m <- 15000

DATA <- NULL
Results <- NULL
LineNumber <- 1
TRIALS <- NULL
LengthTrials <- NULL
priorMessage <- NULL
ColumnNames <- NULL
CategoryNames <- NULL

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
      for (line in outputString) commentsLog(paste0(commentsLog(), "<br>", line))
    } else {
      if (echo)
      {
        cat(text, "\n")
      }
      commentsLog(paste0(commentsLog(), "<br>", text))
    }
  })
}

