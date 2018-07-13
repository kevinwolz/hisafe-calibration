### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

A2.WEATHER <- "./raw_data/restinclieres_A2-1994-2018.wth"
A3.WEATHER <- "./raw_data/restinclieres_A3-1994-2018.wth"
A4.WEATHER <- "./raw_data/restinclieres_A4-1994-2018.wth"
CASTRIES.WEATHER <- "./raw_data/castries.wth"

#tree.params <- list()
winner.tree.params <- list(lueMax = 0.62,
                           rsWaterStressResponsiveness = 1.63,
                           rsNoStressResponsiveness = 0.92,
                           maxTargetLfrRatioDailyVariation = 0.003,
                           targetLfrRatioUpperDrift = 0.31,
                           cRAreaToFRLengthRatio = 8.00E-08,
                           fineRootAnoxiaLifespan = 120,
                           colonisationThreshold = 400,
                           horizontalPreference = 0.63,
                           transpirationCoefficient = 1.13)

tree.params <- winner.tree.params
#tree.params <- c(tree.params, list(stemFlowMax = 0))
crop.params <- list()

# rotation <- rep("durum-wheat-restinclieres.plt", 24)
# rotation[c(4,7,12)] <- "rape.plt"
# rotation[c(16,19,22)] <- "winter-pea.plt"
# rotation[c(1,2,3,5,6,9)] <- "durum-wheat-NEFER.plt"
# rotation[8] <- "durum-wheat-ORJAUNE.plt"
# crop.params <- list(mainCropSpecies = list(rotation))

## DEFINE
A2.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A2",
                        SimulationName = "Restinclieres-A2",
                        bulk.pass      = c(tree.params, crop.params),
                        weatherFile    = A2.WEATHER)

A3.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A3",
                        SimulationName = "Restinclieres-A3",
                        bulk.pass      = c(tree.params, crop.params),
                        weatherFile    = A3.WEATHER)


A4.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_forestry_A4",
                        SimulationName = "Restinclieres-A4",
                        bulk.pass      = tree.params,
                        weatherFile    = A4.WEATHER)

A2.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES[!str_detect(PROFILES, "trees")],
                           template       = "restinclieres_monocrop_A2",
                           SimulationName = "Monocrop-A2",
                           bulk.pass      = crop.params,
                           weatherFile    = A2.WEATHER)

A3.CC.hip <- define_hisafe(path           = PATH,
                           profiles       = PROFILES[!str_detect(PROFILES, "trees")],
                           template       = "restinclieres_monocrop_A3",
                           SimulationName = "Monocrop-A3",
                           bulk.pass      = crop.params,
                           weatherFile    = A3.WEATHER)

castries.hip <- define_hisafe(path             = PATH,
                              profiles         = PROFILES,
                              template         = "castries_agroforestry",
                              SimulationName   = "Castries",
                              mainCropSpecies  = "fescue.plt",
                              interCropSpecies = "fescue.plt",
                              mainCropItk      = "fescue.tec",
                              interCropItk     = "fescue.tec",
                              bulk.pass        = tree.params,
                              weatherFile      = CASTRIES.WEATHER)

## BUILD
if(BUILD.SIMU) {
  build_hisafe(A2.hip)
  build_hisafe(A3.hip)
  build_hisafe(A4.hip)
  build_hisafe(A2.CC.hip)
  build_hisafe(A3.CC.hip)
  build_hisafe(castries.hip)
}

if(CLUSTER) {
  build_cluster_script(simu.names    = ALL.SIMULATIONS,
                       hip           = NULL,
                       script.path   = PATH,
                       cluster.path  = paste0("/lustre/lecomtei/calibration/", NAME, "/"),
                       model.version = MODEL.VERSION,
                       email         = "wolzkevin@gmail.com")
}

## RUN
if(RUN.SIMU) {
  run_hisafe(path        = PATH,
             parallel    = TRUE,
             num.cores   = 4,
             capsis.path = "/Applications/Capsis/")
}
