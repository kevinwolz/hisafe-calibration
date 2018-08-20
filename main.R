### hisafe-calibration
### MAIN
### Author: Kevin J. Wolz

NAME <- "calibration_20180820"
MODEL.VERSION <- "Capsis4"

BUILD.SIMU <- TRUE
RUN.SIMU   <- FALSE
CLUSTER    <- TRUE
PROFILES   <- c("annualCells", "trees", "plot", "cellsDetail", "cells", "treesDetail", "climate", "monthCells")#, "plotDetail", "monthCellsDetail")#, "voxels")
cbPalette  <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ALL.SIMULATIONS         <- c("Monocrop-A2", "Monocrop-A3", "Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4")#, "Castries")
CALIBRATION.SIMULATIONS <- c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4")
VALIDATION.SIMULATIONS  <- c("Castries")
STEPS <- c("calibration")#, "validation")

## REQUIRED LIBRARIES
library(hisafer)
library(tidyverse)
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
source("trees_calibration.R")
source("crops_calibration.R")
source("misc_calibration.R")
source("diagnostics.R")
