### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PROFILES   <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells", "cells")#, "voxels")
A2.WEATHER <- "./raw_data/restinclieres_A2-1994-2034.wth"
A3.WEATHER <- "./raw_data/restinclieres_A3-1994-2034.wth"
A4.WEATHER <- "./raw_data/restinclieres_A4-1994-2034.wth"

## TREE PRUNING
# A2.PRUNED.PROP    <- list(c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
# A3.PRUNED.PROP    <- list(c(0.4,0.4,0.4,0.4,0.4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
# A2.PRUNED.HEIGHTS <- list(c(2,2,2,2,2,2,2,2,2,2.3,2.6,2.9,3.2,3.5,3.5,3.5,3.5,3.7,4.3,4.3,4.3,4.3))
# A3.PRUNED.HEIGHTS <- list(c(1.2,1.2,1.2,1.2,1.2,1.2,2.1,2.1,2.5,2.9,3.2,3.3,3.4,4,4,4,4,4,4.3,4.3,4.3,4.3))

## SOIL
# A2.SOIL <- layer_params(thick         = c(0.4,   0.4,   0.6,   1,     7),
#                         sand          = c(37.61, 26.92, 13.36, 8.64,  8.64),
#                         clay          = c(16.37, 23.11, 29.94, 32.45, 32.45),
#                         organicMatter = c(1.81,  1.09,  0.91,  0.94,  0.94))
# A3.SOIL <- layer_params(thick         = c(0.4,   0.4,   0.6,   1,     7),
#                         sand          = c(19.61, 14.48, 14.36, 24.45, 6.78),
#                         clay          = c(20.83, 26.01, 25.34, 24.76, 32.08),
#                         organicMatter = c(2.41,  1.57,  1.41,  1.21,  3.19))
# A4.SOIL <- layer_params(thick         = c(0.4,   0.4,   0.6,   1,     7),
#                         sand          = c(17.15, 11.05, 8.05,  3.40,  3.40),
#                         clay          = c(22.88, 26.18, 27.33, 35.30, 35.30),
#                         organicMatter = c(2.67,  1.45,  1.43,  2.02,  2.02))

## DEFINE
A2.hip <- define_hisafe(path                = PATH,
                        profiles            = PROFILES,
                        template            = "restinclieres_agroforestry_A2",
                        SimulationName      = "Restinclieres-A2",
                        # heightDbhAllometricCoeffA  = 222,
                        heightDbhAllometricCoeffB  = 0.71,
                        # crownDbhAllometricCoeffA   = 2291,
                        # crownDbhAllometricCoeffB   = 1.32,
                        stemDbhAllometricCoeffC    = 0.86,
                        leafExpansionDuration      = 76,
                        budBurstToLeafFallDuration = 183,
                        leafAreaCrownVolCoefA      = 5.34,
                        leafAreaCrownVolCoefB      = 0.61,
                        lueMax                     = 0.65,
                        weatherFile          = A2.WEATHER)

A3.hip <- define_hisafe(path                = PATH,
                        profiles            = PROFILES,
                        template            = "restinclieres_agroforestry_A3",
                        SimulationName      = "Restinclieres-A3",
                        # heightDbhAllometricCoeffA  = 222,
                        heightDbhAllometricCoeffB  = 0.71,
                        # crownDbhAllometricCoeffA   = 2291,
                        # crownDbhAllometricCoeffB   = 1.32,
                        stemDbhAllometricCoeffC    = 0.86,
                        leafExpansionDuration      = 76,
                        budBurstToLeafFallDuration = 187,
                        leafAreaCrownVolCoefA      = 5.34,
                        leafAreaCrownVolCoefB      = 0.61,
                        lueMax                     = 0.65,
                        weatherFile          = A3.WEATHER)


A4.hip <- define_hisafe(path                = PATH,
                        profiles            = PROFILES,
                        template            = "restinclieres_forestry_A4",
                        SimulationName      = "Restinclieres-A4",
                        # heightDbhAllometricCoeffA  = 222,
                        heightDbhAllometricCoeffB  = 0.71,
                        # crownDbhAllometricCoeffA   = 2291,
                        # crownDbhAllometricCoeffB   = 1.32,
                        stemDbhAllometricCoeffC    = 0.86,
                        leafExpansionDuration      = 76,
                        budBurstToLeafFallDuration = 180,
                        leafAreaCrownVolCoefA      = 5.34,
                        leafAreaCrownVolCoefB      = 0.61,
                        lueMax                     = 0.65,
                        weatherFile         = A4.WEATHER)

A2.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES,
                           template       = "restinclieres_monocrop_A2",
                           SimulationName = "Monocrop-A2",
                           weatherFile    = A2.WEATHER)

A3.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES,
                           template       = "restinclieres_monocrop_A3",
                           SimulationName = "Monocrop-A3",
                           weatherFile    = A3.WEATHER)

## BUILD
if(BUILD.SIMU) {
  build_hisafe(A2.hip)
  build_hisafe(A3.hip)
  build_hisafe(A4.hip)
  build_hisafe(A2.CC.hip)
  build_hisafe(A3.CC.hip)
}

if(CLUSTER) {
  build_cluster_script(simu.names   = c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3"),
                       hip          = NULL,
                       script.path  = PATH,
                       cluster.path = paste0("/nfs/work/hisafe/isabelle/calibration/", NAME, "/"),
                       email        = "wolzkevin@gmail.com")
}

## RUN
if(RUN.SIMU) {
  run_hisafe(path        = PATH,
             parallel    = TRUE,
             num.cores   = 4,
             capsis.path = "/Applications/Capsis/")
}
