# Global.R
library(shiny)
library(metap)
library(Rfast)
library(shinyjs)
library(rsconnect)
#library(mirai)
#library(promises)
library(MBESS)
library(dqrng)
library(OpenMx)
library(digitTests)
library(rsconnect)
library(bslib)
library(shinydashboard)
library(future)
library(foreach)
library(doFuture)
#library(BiocManager)
#BiocManager::install("multtest")

RUN_PARALLEL <- FALSE

if (RUN_PARALLEL) {
  calculate_workers <- function() {
    total_cores <- parallelly::availableCores()

    if (total_cores <= 2) {
      return(1)
    } else if (total_cores <= 4) {
      return(total_cores - 1)
    } else if (total_cores <= 8) {
      return(total_cores - 2)
    } else {
      return(ceiling(total_cores * 0.6))
    }
  }

  plan(multisession, workers = calculate_workers())
}

# replication number for the Monte Carlo simulation
MONTE_CARLO_SIM_REPS <- 15000

COMMON_COL_NAMES <- c("TRIAL", "ROW", "N", "MEAN", "SD",  "ROUND_MEAN", "ROUND_OBSERVATION")
COLS_CONTINUOUS <- c("N", "MEAN", "SD")
