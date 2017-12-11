### hisafe-calibration
### DIAGNOSTICS
### Author: Kevin J. Wolz

## CYCLE PLOTS
carbon.plot <- plot_hisafe_cycle(hop, "carbon", simu.names = c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4"))
ggsave(paste0(PATH, "analysis/cycles/carbon.png"), carbon.plot, height = 4, width = 11)

light.plot <- plot_hisafe_cycle(hop, "light")
ggsave(paste0(PATH, "analysis/cycles/light.png"), light.plot, height = 4, width = 7, scale = 1.5)

# nitrogen.plot <- plot_cycle(face, "nitrogen")
# ggsave(paste0(PATH, "analysis/cycles/nitrogen.png"), nitrogen.plot, height = 4, width = 11)
#
# water.plot <- plot_cycle(face, "water")
# ggsave(paste0(PATH, "analysis/cycles/water.png"), water.plot, height = 4, width = 11)

## DAILY USE PLOT
sim.names <- c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4", "Monocrop-A2", "Monocrop-A3")
cycles    <- c("water", "nitrogen", "light")
for(cycle in cycles) {
  use.plots <- purrr::map(sim.names,
                          plot_hisafe_use,
                          hop   = hop,
                          cycle = cycle,
                          years = "all")
  purrr::walk2(as.list(paste0(PATH, "analysis/cycles/", cycle, "_use_", sim.names, ".png")),
              use.plots,
              ggsave,
              scale = 1, height = 15, width = 15)
}

## CORE DIAGNOSTIC PLOTS
purrr::walk(as.list(PROFILES[PROFILES %in% c("annualtree", "annualplot", "trees", "plot", "climate")]),
            diag_hisafe_ts,
            hop = hop)
diag_hisafe_annualcrop(hop)
diag_hisafe_monthcells(hop)
