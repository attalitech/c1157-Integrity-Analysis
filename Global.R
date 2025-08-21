# Global.R
library(shiny)
library(openxlsx)
library(readxl)
library(metap)
library(Rfast)
library(shinyjs)
library(rsconnect)
library(shinyWidgets)
library(mirai)
library(promises)
library(future)
library(MBESS)
library(dqrng)
library(OpenMx)
library(digitTests)
library(rsconnect)
#library(BiocManager)
#BiocManager::install("multtest")

remove(list=ls())

DATA <- NULL
Results <- NULL
LineNumber <- 1
TRIALS <- NULL
priorMessage <- NULL
ColumnNames <- NULL
CategoryNames <- NULL

# m is the replication number for the Monte Carlo simulation
m <- 100000

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
