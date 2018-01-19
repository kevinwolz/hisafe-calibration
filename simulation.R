### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PROFILES    <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells")
A2.WEATHER  <- "./raw_data/restincl_A2-1995-2034.wth"
A3.WEATHER  <- "./raw_data/restincl_A3-1995-2034.wth"
A4.WEATHER  <- "./raw_data/restincl_A4-1995-2034.wth"

A2.SOIL <- layer_params()
A3.SOIL <- layer_params()
A4.SOIL <- layer_params()


A2.SOIL <- layer_params(thick         = c(0.4,   0.4,   0.6,   1,     7),
                        sand          = c(37.61, 26.92, 13.36, 8.64,  8.64),
                        clay          = c(16.37, 23.11, 29.94, 32.45, 32.45),
                        organicMatter = c(1.81,  1.09,  0.91,  0.94,  0.94))

A3.SOIL <- layer_params(thick         = c(0.4,   0.4,   0.6,   1,     7),
                        sand          = c(19.61, 14.48, 14.36, 24.45, 6.78),
                        clay          = c(20.83, 26.01, 25.34, 24.76, 32.08),
                        organicMatter = c(2.41,  1.57,  1.41,  1.21,  3.19))

A4.SOIL <- layer_params(thick         = c(0.4,   0.4,   0.6,   1,     7),
                        sand          = c(17.15, 11.05, 8.05,  3.40,  3.40),
                        clay          = c(22.88, 26.18, 27.33, 35.30, 35.30),
                        organicMatter = c(2.67,  1.45,  1.43,  2.02,  2.02))

# rotation.ALLUR <- list(c("durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "rape.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "rape.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "rape.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "pea.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "pea.plt",
#                          "durum-wheat-allur-restinclieres.plt",
#                          "barley.plt",
#                          "pea.plt",
#                          "durum-wheat-allur-restinclieres.plt"))

## DEFINE
A2.hip <- define_hisafe(path                = PATH,
                        profiles            = PROFILES,
                        template            = "restinclieres_agroforestry",
                        SimulationName      = "Restinclieres-A2",
                        geometryOption      = 3,
                        plotWidth           = 13,
                        plotHeight          = 8,
                        spacingBetweenRows  = 13,
                        spacingWithinRows   = 8,
                        # heightDbhAllometricCoeffA  = 222,
                        heightDbhAllometricCoeffB  = 0.71,
                        # crownDbhAllometricCoeffA   = 2291,
                        # crownDbhAllometricCoeffB   = 1.32,
                        stemDbhAllometricCoeffC    = 0.86,
                        leafExpansionDuration      = 76,
                        budBurstToLeafFallDuration = 175,
                        leafAreaCrownVolCoefA      = 5.34,
                        leafAreaCrownVolCoefB      = 0.61,
                        tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                        layers              = A2.SOIL,
                        ph                  = 8.1,
                        lueMax              = 0.7,
                        #treeCropDistance   = 0.5,
                        weatherFile         = A2.WEATHER)

A3.hip <- define_hisafe(path                = PATH,
                        profiles            = PROFILES,
                        template            = "restinclieres_agroforestry",
                        SimulationName      = "Restinclieres-A3",
                        treeLineOrientation = 344.3,
                        geometryOption      = 3,
                        plotWidth           = 13,
                        plotHeight          = 8,
                        spacingBetweenRows  = 13,
                        spacingWithinRows   = 8,
                        # heightDbhAllometricCoeffA  = 222,
                        heightDbhAllometricCoeffB  = 0.71,
                        # crownDbhAllometricCoeffA   = 2291,
                        # crownDbhAllometricCoeffB   = 1.32,
                        stemDbhAllometricCoeffC    = 0.86,
                        leafExpansionDuration      = 76,
                        budBurstToLeafFallDuration = 175,
                        leafAreaCrownVolCoefA      = 5.34,
                        leafAreaCrownVolCoefB      = 0.61,
                        tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                        layers              = A3.SOIL,
                        ph                  = 8.4,
                        lueMax              = 0.7,
                        #treeCropDistance   = 0.5,
                        weatherFile         = A3.WEATHER)


A4.hip <- define_hisafe(path                = PATH,
                        profiles            = PROFILES,
                        template            = "restinclieres_forestry",
                        SimulationName      = "Restinclieres-A4",
                        geometryOption      = 3,
                        plotWidth           = 7,
                        plotHeight          = 4,
                        spacingBetweenRows  = 7,
                        spacingWithinRows   = 4,
                        # heightDbhAllometricCoeffA  = 222,
                        heightDbhAllometricCoeffB  = 0.71,
                        # crownDbhAllometricCoeffA   = 2291,
                        # crownDbhAllometricCoeffB   = 1.32,
                        stemDbhAllometricCoeffC    = 0.86,
                        leafExpansionDuration      = 76,
                        budBurstToLeafFallDuration = 175,
                        leafAreaCrownVolCoefA      = 5.34,
                        leafAreaCrownVolCoefB      = 0.61,
                        tree.initialization = tree_init_params(treeX = 3.5, treeY = 1.5),
                        layers              = A4.SOIL,
                        ph                  = 8.3,
                        lueMax              = 0.7,
                        weatherFile         = A4.WEATHER)

A2.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES,
                           template       = "restinclieres_monocrop",
                           SimulationName = "Monocrop-A2",
                           layers         = A2.SOIL,
                           ph             = 8.1,
                           weatherFile    = A2.WEATHER)

A3.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES,
                           template       = "restinclieres_monocrop",
                           SimulationName = "Monocrop-A3",
                           layers         = A3.SOIL,
                           ph             = 8.4,
                           weatherFile    = A3.WEATHER)


if(BUILD.SIMU) {
  ## BUILD
  build_hisafe(A2.hip)
  build_hisafe(A3.hip)
  build_hisafe(A4.hip)
  build_hisafe(A2.CC.hip)
  build_hisafe(A3.CC.hip)
}

if(CLUSTER) {
  dum <- map(c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3"),
             build_cluster_script,
             script.path  = PATH,
             #cluster.path = paste0("/nfs/work/hisafe/kevin/", NAME, "/"),
             cluster.path = paste0("/nfs/work/hisafe/isabelle/soiltest/", NAME, "/"),
             email        = "wolzkevin@gmail.com")

  # ftpUpload(PATH,
  #           "sftp://wolzk:c5qiIyFtDm5C@muse-login.hpc-lr.univ-montp2.fr:22/nfs/work/hisafe/kevin/upload/")
}

if(RUN.SIMU) {
  ## RUN
  run_hisafe(path        = PATH,
             parallel    = TRUE,
             num.cores   = 4,
             capsis.path = "/Applications/Capsis/")
}

## READ MERGED HOP
hop <- read_hisafe(path       = PATH,
                   simu.names = c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3"),
                   profiles   = PROFILES)

hop <- hop_date_filter(hop,
                       date.min = NA,
                       date.max = "2018-01-01")

dum <- map(paste0(PATH, c("analysis/cycles/", "analysis/calibration/")),
           dir.create,
           showWarnings = FALSE,
           recursive    = TRUE)

## CREATE FACE
# face <- create_face(agroforestry = AF.hop,
#                     forestry     = FC.hop,
#                     monocrop     = CC.hop,
#                     face.path    = PATH)
