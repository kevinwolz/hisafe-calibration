### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME <- "calibration_20171208_1"
RUN.SIMU  <- TRUE

## REQUIRED LIBRARIES
library(hisafer)
library(tidyverse)
library(readr)
library(stringr)
library(DeLuciatoR)
library(lubridate)
library(viridis)
library(grid)

## PATHS
input.path       <- "./raw_data/"
simulation.path  <- "./simulations/"
data.path        <- "./output/processed_data/"
PATH             <- paste0(simulation.path, NAME, "/")

## SCRIPTS
source("simulation.R")
source("field_data.R")
source("trees.R")
source("crops.R")
source("diagnostics.R")
