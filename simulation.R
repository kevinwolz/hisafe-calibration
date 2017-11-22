### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PATH     <- paste0(simulation.path, NAME, "/")
PROFILES <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells", "cells")
WEATHER  <- "./raw_data/restincl_A2-1995-2034.wth"
YEARS    <- 22

## DEFINE
AF.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "agroforestry_default",
                        SimulationName = MODELED.SITE,
                        nbSimulations  = YEARS,
                        weatherFile    = WEATHER)

FC.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "forestry_default",
                        SimulationName = "Forestry",
                        nbSimulations  = YEARS,
                        weatherFile    = WEATHER)

CC.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "monocrop_default",
                        SimulationName = "Monocrop",
                        nbSimulations  = YEARS,
                        weatherFile    = WEATHER)

if(RUN.SIMU) {
  ## BUILD
  build_hisafe(AF.hip)
  build_hisafe(FC.hip)
  build_hisafe(CC.hip)

  ## RUN
  run_hisafe_exp(path      = PATH,
                 parallel  = TRUE,
                 num.cores = 3)
}

## READ
AF.hop <- read_hisafe(AF.hip, profiles = PROFILES)
FC.hop <- read_hisafe(FC.hip, profiles = PROFILES)
CC.hop <- read_hisafe(CC.hip, profiles = PROFILES)

## CREATE FACE
face <- create_face(agroforestry = AF.hop,
                    forestry     = FC.hop,
                    monocrop     = CC.hop,
                    face.path    = PATH)

## DIAGNOSTICS
purrr::walk(as.list(PROFILES[PROFILES %in% c("annualtree", "annualplot", "trees", "plot", "climate")]),
            diag_hisafe_ts,
            hop = face)
diag_hisafe_monthcells(face)
