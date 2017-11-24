### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PATH     <- paste0(simulation.path, NAME, "/")
PROFILES <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells")#, "cells")
WEATHER  <- "./raw_data/restincl_A2-1995-2034.wth"

## DEFINE
AF.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry",
                        SimulationName = MODELED.SITE,
                        mainCropSpecies  = "durum-wheat-allur-restinclieres.plt",
                        mainCropItk      = "durum-wheat-restinclieres.tec",
                        interCropSpecies  = "baresoil.plt",
                        interCropItk      = "baresoil.tec",
                        spacingWithinRows = 9,
                        weatherFile    = WEATHER)

FC.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "forestry_default",
                        SimulationName = "Forestry",
                        nbSimulations       = 22,
                        treeLineOrientation = 100,
                        interCropSpecies    = "baresoil.plt",
                        interCropItk        = "baresoil.tec",
                        weatherFile    = WEATHER)

CC.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_monocrop",
                        mainCropSpecies  = "durum-wheat-allur-restinclieres.plt",
                        mainCropItk      = "durum-wheat-restinclieres.tec",
                        SimulationName = "Monocrop",
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
AF.hop <- read_hisafe(path = PATH, simu.name = MODELED.SITE, profiles = PROFILES)
FC.hop <- read_hisafe(path = PATH, simu.name = "Forestry", profiles = PROFILES)
CC.hop <- read_hisafe(path = PATH, simu.name = "Monocrop", profiles = PROFILES)

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
