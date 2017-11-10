### hisafe-calibration
### SIMULATION
### Author: Kevin J. Wolz

## DEFINE
hip <- define_hisafe(SimulationName = NAME,
                     nbSimulations  = 22,
                     weatherFile = "./raw_data/restincl_A2-1995-2034.wth")

## BUILD
dir.create("./simulations", showWarnings = FALSE)
hip <- build_hisafe(hip = hip,
                    path = "./simulations",
                    profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells"),
                    saveProjectOption = FALSE)

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
