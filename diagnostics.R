### hisafe-calibration
### DIAGNOSTICS
### Author: Kevin J. Wolz

## DIAGNOSTICS
purrr::walk(as.list(PROFILES[PROFILES %in% c("annualtree", "annualplot", "trees", "plot", "climate")]),
            diag_hisafe_ts,
            hop = face)
diag_hisafe_monthcells(face)

## CYCLES
carbon.plot <- plot_annual_cycle(face, "carbon")
ggsave(paste0(face$face.path, "carbon.png"), carbon.plot, height = 4, width = 11)

light.plot <- plot_annual_cycle(face, "light")
ggsave(paste0(face$face.path, "light.png"), light.plot, height = 4, width = 11)

# nitrogen.plot <- plot_annual_cycle(face, "nitrogen")
# ggsave(paste0(face$face.path, "nitrogen.png"), nitrogen.plot, height = 4, width = 11)
#
# water.plot <- plot_annual_cycle(face, "water")
# ggsave(paste0(face$face.path, "water.png"), water.plot, height = 4, width = 11)
