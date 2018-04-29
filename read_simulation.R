### hisafe-calibration
### READ
### Author: Kevin J. Wolz

## READ
hop <- read_hisafe(path       = PATH,
                   max.size   = 1e9,
                   simu.names = ALL.SIMULATIONS,
                   profiles   = PROFILES,
                   date.max   = "2018-01-01")

#hop <- hop_filter(hop,  c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3"))

dum <- map(paste0(PATH, c("analysis/cycles/", "analysis/calibration/", "analysis/validation/")),
           dir.create,
           showWarnings = FALSE,
           recursive    = TRUE)
