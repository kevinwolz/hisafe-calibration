### hisafe-calibration
### DIAGNOSTICS
### Author: Kevin J. Wolz

## CYCLES
carbon.plot <- plot_annual_cycle(hop, "carbon", simu.names = c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4"))
ggsave(paste0(PATH, "analysis/carbon.png"), carbon.plot, height = 4, width = 11)

light.plot <- plot_annual_cycle(hop, "light")
ggsave(paste0(PATH, "analysis/light.png"), light.plot, height = 4, width = 7, scale = 1.5)

# nitrogen.plot <- plot_annual_cycle(face, "nitrogen")
# ggsave(paste0(face$face.path, "nitrogen.png"), nitrogen.plot, height = 4, width = 11)
#
# water.plot <- plot_annual_cycle(face, "water")
# ggsave(paste0(face$face.path, "water.png"), water.plot, height = 4, width = 11)

## DIAGNOSTICS
purrr::walk(as.list(PROFILES[PROFILES %in% c("annualtree", "annualplot", "trees", "plot", "climate")]),
            diag_hisafe_ts,
            hop = hop)
diag_hisafe_monthcells(hop)
