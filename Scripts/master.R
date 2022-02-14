#SERIAL INTERVAL ESTIMATION
#Cy Geismar

# SETUP -------------------------------------------------------------------
.libPaths("S:/CoronaWatch/r_libraries_4")
library(dplyr)
library(ggplot2)
library(VirusWatch)
library(magrittr)
library(gtsummary)
library(gt)
library(ggpubr)
library(flextable)

home_directory <- 
  "S:/CoronaWatch/Working/Dashboard/symptoms_and_test_analyses/serial-interval/Omicron"

setwd(home_directory)

# DATA IMPORT -------------------------------------------------------------

source("Scripts/load.R")


# PARAMETERS: TO EDIT FOR SENITIVITY ANALYSIS -----------------------------
#raw_df <- read.csv("Input Data/raw_df_2022-01-29.csv") #analysis date (ignore if updating in real time)
#last_update <- "2022-01-29" # analysis date (ignore if updating in real time)


max_si <- 14               # a digit, maximum time period to match a potential infectee to an infector
prune_households = FALSE  # logical (TRUE OR FALSE), if true will keep households of a specified size
household_size = NA     # digit, household size (# of people) you want for the analysis # if prune_households = TRUE, specify digit between 1 and 6. # If prune_households = FALSE, specify household_size = NA



# Cleaning -----------------------------------------------------------
source("Scripts/clean_raw_data.R")


# Compute infector infectee pairs ------------------------------------------------------------
source("Scripts/compute_pairs.R")


# Pruning households ------------------------------------------------------

if(prune_households ==TRUE){
  certain_transmissions <- certain_transmissions %>% 
    filter(no_of_householders== household_size) ## # remove households with more than 2 members
  warning(".........pruning households .........")
  print(paste("keeping households of size", household_size))
} else{
  warning("prune_households == FALSE")
}


# Tables -----------------------------------------------------------------
source("Scripts/tables.R")


# Figures -------------------------------------------------------------------
source("Scripts/figures.R")



# Results ------------------------------------------------------------------
source("Scripts/results.R")


# Saving ----------------------------------------------------------
source("Scripts/save.R")

