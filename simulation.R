### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

A2.WEATHER <- "./raw_data/restinclieres_A2-1994-2018.wth"
A3.WEATHER <- "./raw_data/restinclieres_A3-1994-2018.wth"
A4.WEATHER <- "./raw_data/restinclieres_A4-1994-2018.wth"
CASTRIES.WEATHER <- "./raw_data/castries.wth"

#common.params <- list()
common.params <- winner.common.params

## DEFINE
A2.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A2",
                        SimulationName = "Restinclieres-A2",
                        bulk.pass      = common.params,
                        weatherFile    = A2.WEATHER)

A3.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A3",
                        SimulationName = "Restinclieres-A3",
                        bulk.pass      = common.params,
                        weatherFile    = A3.WEATHER)


A4.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_forestry_A4",
                        SimulationName = "Restinclieres-A4",
                        bulk.pass      = common.params,
                        weatherFile    = A4.WEATHER)

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

castries.hip <- define_hisafe(path             = PATH,
                              profiles         = PROFILES,
                              template         = "castries_agroforestry",
                              SimulationName   = "Castries",
                              mainCropSpecies  = "durum-wheat-allur-restinclieres.plt",
                              interCropSpecies = "durum-wheat-allur-restinclieres.plt",
                              mainCropItk      = "durum-wheat-restinclieres.tec",
                              interCropItk     = "durum-wheat-restinclieres.tec",
                              bulk.pass        = common.params,
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
  build_cluster_script(simu.names   = ALL.SIMULATIONS,
                       hip          = NULL,
                       script.path  = PATH,
                       cluster.path = paste0("/lustre/lecomtei/calibration/", NAME, "/"),
                       email        = "wolzkevin@gmail.com")
}

## RUN
if(RUN.SIMU) {
  run_hisafe(path        = PATH,
             parallel    = TRUE,
             num.cores   = 4,
             capsis.path = "/Applications/Capsis/")
}
