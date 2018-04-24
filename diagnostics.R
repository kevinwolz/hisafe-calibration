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

hisafe_snapshot(hop           = hop,
                dates         = paste0(1995:2017, "-06-01"),
                simu.names    = hop$exp.plan$SimulationName[!str_detect(hop$exp.plan$SimulationName, "Monocrop")],
                file.prefix   = "Calibration_Snapshot",
                complete.only = TRUE)

diag_hisafe(hop,
            annualTrees = FALSE,
            annualPlot  = FALSE,
            trees       = TRUE,
            plot        = TRUE,
            climate     = FALSE,
            annualCells = FALSE,
            monthCells  = FALSE,
            cells       = FALSE,
            voxels      = FALSE,
            facet.year = FALSE,
            tree.id    = 1)
