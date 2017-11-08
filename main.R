### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME <- "restinclieres-A2"
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
plot.path  <- "./output/plots/"
data.path  <- "./output/processed_data/"

## SCRIPTS
if(RUN.SIMU) {
  source("simulation.R")
} else {
  hop <- read_hisafe(path = "./simulations", simu.name = "restinclieres-A2")
}
source("trees.R")