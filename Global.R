# Global.R
library(shiny)
library(openxlsx)
library(readxl)
library(metap)
library(Rfast)
library(shinyjs)
library(rsconnect)
library(shinyWidgets)
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

registerDoFuture(flavor = "%dofuture%")
plan(multisession)

#library(BiocManager)
#BiocManager::install("multtest")

remove(list=ls())

DATA <- NULL
Results <- NULL
LineNumber <- 1
TRIALS <- NULL
LengthTrials <- NULL
priorMessage <- NULL
ColumnNames <- NULL
CategoryNames <- NULL

# m is the replication number for the Monte Carlo simulation
m <- 100000
m <- 15000


############################################################################
# References                                                               #
# Carlisle JB. The analysis of 168 randomised controlled trials to test    #
# data integrity. Anaesthesia. 2012;67:521-537.                            #
#                                                                          #
# Carlisle JB, Dexter F, Pandit JJ, Shafer SL, Yentis SM. Calculating the  #
# probability of random sampling for continuous variables in submitted or  #
# published randomised controlled trials. Anaesthesia. 2015;70:848-58.     #
#                                                                          #
# Carlisle JB. Data fabrication and other reasons for non-random sampling  #
# in 5087 randomised, controlled trials in anaesthetic and general medical #
# journals. Anaesthesia. 2017;72:944-952                                   #
############################################################################

is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local)
  setwd("g:/projects/Fraud/2025")

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

