### hisafe-calibration
### READ
### Author: Kevin J. Wolz

hop <- read_hisafe(path       = PATH,
                   max.size   = 1000,
                   simu.names = ALL.SIMULATIONS,
                   profiles   = PROFILES,
                   date.max   = "2018-01-01")

dum <- map(paste0(PATH, c("analysis/cycles/", "analysis/calibration/", "analysis/validation/")),
           dir.create,
           showWarnings = FALSE,
           recursive    = TRUE)
