### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PROFILES    <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells")#, "cells")
A2.WEATHER  <- "./raw_data/restincl_A2-1995-2034.wth"
A3.WEATHER  <- "./raw_data/restincl_A3-1995-2034.wth"
A4.WEATHER  <- "./raw_data/restincl_A4-1995-2034.wth"

## DEFINE
A2.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry",
                        SimulationName = "Restinclieres-A2",
                        #simulationNbrDays = list(rep(365,22)),
                        #mainCropSpecies   = "durum-wheat-allur-restinclieres-Talbot.plt",
                        interCropSpecies  = "weed-restinclieres-Talbot.plt", # "baresoil.plt",
                        interCropItk      = "weed-restinclieres.tec", # "baresoil.tec",
                        treePruningDays = list(rep(215, 20)),
                        #spacingWithinRows = 9,
                            geometryOption = 3,
                            plotWidth = 13,
                            plotHeight = 8,
                            spacingBetweenRows = 13,
                            spacingWithinRows = 8,
                            tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                        #treeCropDistance = 0.5,
                        weatherFile    = A2.WEATHER)

A3.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry",
                        SimulationName = "Restinclieres-A3",
                        #simulationNbrDays = list(rep(365,22)),
                        #mainCropSpecies     = "durum-wheat-allur-restinclieres-Talbot.plt",
                        interCropSpecies    = "weed-restinclieres-Talbot.plt", # "baresoil.plt",
                        interCropItk        = "weed-restinclieres.tec", # "baresoil.tec",
                        treePruningDays = list(rep(215, 20)),
                        treeLineOrientation = 344.3,
                        #spacingWithinRows   = 9,
                            geometryOption = 3,
                            plotWidth = 13,
                            plotHeight = 8,
                            spacingBetweenRows = 13,
                            spacingWithinRows = 8,
                            tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                        #treeCropDistance = 0.5,
                        weatherFile    = A3.WEATHER)


A4.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_forestry",
                        SimulationName = "Restinclieres-A4",
                        interCropSpecies    = "weed-restinclieres-Talbot.plt", # "baresoil.plt",
                        interCropItk        = "weed-restinclieres.tec", # "baresoil.tec",
                        treePruningDays = list(rep(215, 20)),
                            geometryOption = 3,
                            plotWidth = 7,
                            plotHeight = 4,
                            spacingBetweenRows = 7,
                            spacingWithinRows = 4,
                            tree.initialization = tree_init_params(treeX = 3.5, treeY = 1.5),
                        weatherFile    = A4.WEATHER)

A2.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES,
                           template       = "restinclieres_monocrop",
                           #mainCropSpecies = "durum-wheat-allur-restinclieres-Talbot.plt",
                           SimulationName  = "Monocrop-A2",
                           #simulationNbrDays = list(rep(365,22)),
                           weatherFile    = A2.WEATHER)

A3.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES,
                           template       = "restinclieres_monocrop",
                           #mainCropSpecies = "durum-wheat-allur-restinclieres-Talbot.plt",
                           SimulationName  = "Monocrop-A3",
                           #simulationNbrDays = list(rep(365,22)),
                           weatherFile    = A3.WEATHER)


if(RUN.SIMU) {
  ## BUILD
  build_hisafe(A2.hip)
  build_hisafe(A3.hip)
  build_hisafe(A4.hip)
  build_hisafe(A2.CC.hip)
  build_hisafe(A3.CC.hip)

  ## RUN
  run_hisafe(path        = PATH,
             parallel    = TRUE,
             num.cores   = 4)
}

## READ INDIVIDUAL HOPS
# AF.hop <- read_hisafe(path = PATH, simu.names = c("Restinclieres-A2", "Restinclieres-A3"), profiles = PROFILES)
# FC.hop <- read_hisafe(path = PATH, simu.names = "Forestry",   profiles = PROFILES)
# CC.hop <- read_hisafe(path = PATH, simu.names = "Monocrop",   profiles = PROFILES)

## READ MERGED HOP
hop.new <- hop <- read_hisafe(path = PATH,
                   simu.names = c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3"),
                   profiles = PROFILES)
# hop.old <- read_hisafe(path = "./simulations/calibration_20171201_1/",
#                    simu.names = "Restinclieres-A2",
#                    profiles = PROFILES)
#
# hop <- hop_merge(hop.new, hop.old)

## CREATE FACE
# face <- create_face(agroforestry = AF.hop,
#                     forestry     = FC.hop,
#                     monocrop     = CC.hop,
#                     face.path    = PATH)
dir.create(paste0(PATH, "analysis/cycles/"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(PATH, "analysis/calibration/"), showWarnings = FALSE, recursive = TRUE)
