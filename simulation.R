### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

PATH     <- paste0(simu.path, NAME)
PROFILES <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells")
WEATHER  <- R.utils::getAbsolutePath("./raw_data/restincl_A2-1995-2034.wth")
YEARS    <- 22

## DEFINE
AF.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "agroforestry_default",
                        SimulationName = "Agroforestry",
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

## BUILD
build_hisafe(AF.hip)
build_hisafe(FC.hip)
build_hisafe(CC.hip)

## RUN
log <- run_hisafe(path = PATH)

## READ
AF.hop <- read_hisafe(AF.hip)
FC.hop <- read_hisafe(FC.hip)
CC.hop <- read_hisafe(CC.hip)

## DIAGNOSTICS
purrr::walk2(list(AF.hop, FC.hop, CC.hop),
             list("annualtree", "annualcrop", "annualplot", "trees", "plot", "climate"),
             diag_hisfae_ts)
# diag_hisafe_ts(AF.hop, "annualtree")
# diag_hisafe_ts(AF.hop, "annualcrop")
# diag_hisafe_ts(AF.hop, "annualplot")
# diag_hisafe_ts(AF.hop, "trees")
# diag_hisafe_ts(AF.hop, "plot")
# diag_hisafe_ts(AF.hop, "climate")

purrr::walk(list(AF.hop, FC.hop, CC.hop), diag_hisafe_monthcells)
# diag_hisafe_monthcells(hop)

## CREATE FACE
# face <- create_face(agroforestry = AF.hop,
#                     forestry     = FC.hop,
#                     monocrop     = CC.hop)