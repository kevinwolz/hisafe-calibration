### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME     <- "calibration_20171119"
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
simulation.path  <- "./simulations/"
plot.path  <- "./output/plots/"
data.path  <- "./output/processed_data/"

## SCRIPTS
# source("simulation.R")
# source("trees.R")