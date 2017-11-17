### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

## DEFINE
AF.hip <- define_hisafe(path           = paste0("/Users/kevinwolz/Desktop/", NAME),
                        profiles       = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells"),
                        template       = "agroforestry_default",
                        SimulationName = "Agroforestry",
                        nbSimulations  = 22,
                        weatherFile    = R.utils::getAbsolutePath("./raw_data/restincl_A2-1995-2034.wth"))

FC.hip <- define_hisafe(path           = paste0("/Users/kevinwolz/Desktop/", NAME),
                        profiles       = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells"),
                        template       = "forestry_default",
                        SimulationName = "Forestry",
                        nbSimulations  = 22,
                        weatherFile    = R.utils::getAbsolutePath("./raw_data/restincl_A2-1995-2034.wth"))

CC.hip <- define_hisafe(path           = paste0("/Users/kevinwolz/Desktop/", NAME),
                        profiles       = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells"),
                        template       = "monocrop_default",
                        SimulationName = "Monocrop",
                        nbSimulations  = 22,
                        weatherFile    = R.utils::getAbsolutePath("./raw_data/restincl_A2-1995-2034.wth"))

## BUILD
build_hisafe(AF.hip)
build_hisafe(FC.hip)
build_hisafe(CC.hip)

## RUN
log <- run_hisafe(hip)

## READ
hop <- read_hisafe(hip)

## DIAGNOSTICS
diag_hisafe_ts(hop, "annualtree")
diag_hisafe_ts(hop, "annualcrop")
diag_hisafe_ts(hop, "annualplot")
diag_hisafe_ts(hop, "trees")
diag_hisafe_ts(hop, "plot")
diag_hisafe_ts(hop, "climate")
diag_hisafe_monthcells(hop)
