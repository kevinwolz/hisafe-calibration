### hisafe-calibration
### DIAGNOSTICS
### Author: Kevin J. Wolz

analyze_hisafe(hop,
               carbon          = TRUE,
               light           = TRUE,
               nitrogen        = TRUE,
               water           = TRUE,
               carbon.daily    = TRUE,
               light.daily     = TRUE,
               nitrogen.daily  = TRUE,
               water.daily     = TRUE)

hop.snapshot <- hop_filter(hop,
                           dates      = paste0(1995:2017, "-06-01"),
                           simu.names = hop$exp.plan$SimulationName[!str_detect(hop$exp.plan$SimulationName, "Monocrop")])
hisafe_snapshot(hop           = hop.snapshot,
                dates         = paste0(1995:2017, "-06-01"),
                file.prefix   = "Calibration_Snapshot",
                complete.only = TRUE, plot.x = "x", cells = FALSE)

hisafe_snapshot(hop           = hop,
                cells.var     = "grainBiomass",
                dates         = seq(ymd("2017-6-1"), ymd("2017-7-1"), "1 day"),
                rel.dates     = seq(ymd("2017-6-1"), ymd("2017-7-1"), "1 day"),
                simu.names    = c("Monocrop-A2", "Monocrop-A3", "Restinclieres-A2", "Restinclieres-A3"),
                file.prefix   = "Yield_Snapshot",
                slice = FALSE,
                complete.only = TRUE, plot.x = "x", cells = TRUE)

diag_hisafe(hop,
            trees       = TRUE,
            plot        = TRUE,
            climate     = TRUE,
            annualCells = TRUE,
            monthCells  = TRUE,
            cells       = TRUE,
            voxels      = FALSE,
            facet.year = FALSE,
            tree.ids    = 1,
            facet.simu = TRUE,
            facet.crop = TRUE)

mineralization.plot <- plot_hisafe_ts(hop, "nitrogenHumusMineralisation", "cells", facet.year = TRUE, cumulative = TRUE)
ggsave_fitmax(paste0(PATH, "analysis/diagnostics/nitrogenHumusMineralisation.png"), mineralization.plot, scale = 1.5)
