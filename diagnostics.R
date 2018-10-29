### hisafe-calibration
### DIAGNOSTICS
### Author: Kevin J. Wolz

## H2O & N BALANCE
water.budget.day    <- hisafe_budget(hop = hop, cycle = "water",    freq = "day")
nitrogen.budget.day <- hisafe_budget(hop = hop, cycle = "nitrogen", freq = "day")

water.budget.month    <- hisafe_budget(hop = hop, cycle = "water",    freq = "month")
nitrogen.budget.month <- hisafe_budget(hop = hop, cycle = "nitrogen", freq = "month")

water.budget.year    <- hisafe_budget(hop = hop, cycle = "water",    freq = "year")
nitrogen.budget.year <- hisafe_budget(hop = hop, cycle = "nitrogen", freq = "year")

## SNAPSHOTS
hop.snapshot <- hop_filter(hop,
                           dates      = paste0(1995:2017, "-06-01"),
                           simu.names = hop$exp.plan$SimulationName[!str_detect(hop$exp.plan$SimulationName, "Monocrop")])
hisafe_snapshot(hop           = hop.snapshot,
                dates         = paste0(1995:2017, "-06-01"),
                file.prefix   = "Calibration_Snapshot",
                complete.only = TRUE, plot.x = "x", cells = TRUE)

grain.date.range <- hop$cells %>%
  filter(Year == 2017) %>%
  filter(grainBiomass > 0) %>%
  .$Date %>%
  range()

grain.date.range <- seq(grain.date.range[1], grain.date.range[2], "1 day")

hisafe_snapshot(hop           = hop,
                cells.var     = "grainBiomass",
                dates         = grain.date.range,
                rel.dates     = grain.date.range,
                simu.names    = c("Restinclieres-A2", "Restinclieres-A3"),
                file.prefix   = "Yield_Snapshot",
                slice = FALSE)

## DIAGNOSTICS
diag_hisafe(hop,
            trees       = TRUE,
            plot        = FALSE,
            climate     = FALSE,
            annualCells = FALSE,
            monthCells  = TRUE,
            cells       = TRUE,
            voxels      = FALSE,
            facet.year  = FALSE,
            tree.ids    = 1,
            facet.simu  = TRUE,
            facet.crop  = TRUE)

## CYCLES
analyze_hisafe(hop      = hop,
               tree.ids = 1,
               carbon   = TRUE,
               light    = FALSE,
               nitrogen = FALSE,
               water    = FALSE,
               carbon.daily   = TRUE,
               light.daily    = FALSE,
               nitrogen.daily = FALSE,
               water.daily    = FALSE,
               carbon.increment  = TRUE,
               carbon.allocation = TRUE)

## NITROGEN MINERALIZATION
mineralization.plot <- plot_hisafe_ts(hop, "nitrogenHumusMineralisation", "cells", facet.year = TRUE, cumulative = TRUE, save = TRUE)

## WATER TABLE & ROOT DEPTH
bg.plot <- plot_hisafe_bg(hop        = hop,
                          simu.names = hop$exp.plan$SimulationName[!str_detect(hop$exp.plan$SimulationName, "Monocrop")],
                          years      = 2016:2017)
ggsave_fitmax(paste0(PATH, "analysis/diagnostics/RootDepth-WaterTable.png"), bg.plot, scale = 2)
