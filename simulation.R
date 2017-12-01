### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PATH        <- paste0(simulation.path, NAME, "/")
PROFILES    <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells")#, "cells")
WEATHER     <- "./raw_data/restincl_A2-1995-2034.wth"

## DEFINE
AF.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry",
                        SimulationName = MODELED.SITE,
                        mainCropSpecies   = "durum-wheat-allur-restinclieres-Talbot.plt",
                        interCropSpecies  = "weed-restinclieres-Talbot.plt", # "baresoil.plt",
                        interCropItk      = "weed-restinclieres.tec", # "baresoil.tec",
                        spacingWithinRows = 9,
                        weatherFile    = WEATHER)

FC.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_forestry",
                        SimulationName = "Forestry",
                        interCropSpecies    = "weed-restinclieres-Talbot.plt", # "baresoil.plt",
                        interCropItk        = "weed-restinclieres.tec", # "baresoil.tec",
                        weatherFile    = WEATHER)

CC.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_monocrop",
                        mainCropSpecies  = "durum-wheat-allur-restinclieres-Talbot.plt",
                        SimulationName = "Monocrop",
                        weatherFile    = WEATHER)

if(RUN.SIMU) {
  ## BUILD
  build_hisafe(AF.hip)
  build_hisafe(FC.hip)
  build_hisafe(CC.hip)

  ## RUN
  run_hisafe(path        = PATH,
             parallel    = TRUE,
             num.cores   = 3)
}

## READ
AF.hop <- read_hisafe(path = PATH, simu.names = MODELED.SITE, profiles = PROFILES)
FC.hop <- read_hisafe(path = PATH, simu.names = "Forestry",   profiles = PROFILES)
CC.hop <- read_hisafe(path = PATH, simu.names = "Monocrop",   profiles = PROFILES)

## CREATE FACE
face <- create_face(agroforestry = AF.hop,
                    forestry     = FC.hop,
                    monocrop     = CC.hop,
                    face.path    = PATH)
dir.create(paste0(face$exp.path, "analysis/"), showWarnings = FALSE)
