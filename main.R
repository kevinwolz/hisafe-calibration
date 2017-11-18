### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME     <- "calibration_20171118"
RUN.SIMU <- TRUE

## REQUIRED LIBRARIES
library(hisafer)
library(tidyverse)
library(readr)
library(stringr)
library(DeLuciatoR)
library(lubridate)

## PATHS
input.path <- "./raw_data/"
simu.path  <- "./simulations/"
plot.path  <- "./output/plots/"
data.path  <- "./output/processed_data/"

## SCRIPTS
# if(RUN.SIMU) {
#   source("simulation.R")
# } else {
#   hop <- read_hisafe(path = "./simulations", simu.name = "restinclieres-A2")
# }
# source("trees.R")