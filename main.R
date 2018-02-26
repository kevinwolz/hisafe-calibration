### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME <- "calibration_20180226"
BUILD.SIMU  <- TRUE
RUN.SIMU    <- FALSE
CLUSTER     <- TRUE

## REQUIRED LIBRARIES
library(hisafer)
library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(viridis)
library(grid)
library(ggalt)

## PATHS
input.path      <- "./raw_data/"
simulation.path <- "./simulations/"
data.path       <- "./output/processed_data/"
allom.path      <- "./output/allometry/"
PATH            <- paste0(simulation.path, NAME, "/")

## SCRIPTS
source("simulation.R")
source("read_simulation.R")
source("field_data.R")
source("trees.R")
source("crops.R")
source("diagnostics.R")

