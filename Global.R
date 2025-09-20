# Global.R
library(shiny)
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

Results <- NULL
LineNumber <- 1
TRIALS <- NULL
LengthTrials <- NULL
priorMessage <- NULL
ColumnNames <- NULL
CategoryNames <- NULL
