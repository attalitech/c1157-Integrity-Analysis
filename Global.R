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

plan(multisession, workers = parallelly::availableCores(omit = 1))

# replication number for the Monte Carlo simulation
MONTE_CARLO_SIM_REPS <- 15000

COMMON_COL_NAMES <- c("TRIAL", "ROW", "N", "MEAN", "SD",  "ROUND_MEAN", "ROUND_OBSERVATION")
COLS_CONTINUOUS <- c("N", "MEAN", "SD")
