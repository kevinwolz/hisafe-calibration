### hisafe-calibration
### READ
### Author: Kevin J. Wolz

## READ
hop <- read_hisafe(path       = PATH,
                   max.size   = 1e9,
                   simu.names = ALL.SIMUATIONS,
                   profiles   = PROFILES,
                   date.min   = "1995-01-01",
                   date.max   = "2018-01-01")

hop <- hop_filter(hop,  c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3"))

dum <- map(paste0(PATH, c("analysis/cycles/", "analysis/calibration/")),
           dir.create,
           showWarnings = FALSE,
           recursive    = TRUE)
