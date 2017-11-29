### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME         <- "calibration_20171129_4"
MODELED.SITE <- "Restinclieres-A2"
FIELD.SITE   <- "Restinclieres-A2"

RUN.SIMU  <- TRUE

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## REQUIRED LIBRARIES
library(hisafer)
library(tidyverse)
library(readr)
library(stringr)
library(DeLuciatoR)
library(lubridate)
library(viridis)

## PATHS
input.path       <- "./raw_data/"
simulation.path  <- "./simulations/"
data.path        <- "./output/processed_data/"

## SCRIPTS
source("simulation.R")
source("field_data.R")
source("trees.R")
source("crops.R")