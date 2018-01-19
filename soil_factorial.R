
PATH        <- "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe_testing/"
PROFILES    <- c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate")#, "monthCells")
A3.WEATHER  <- "./raw_data/restincl_A3-1995-2034.wth"


NAME <- "soil_A3_anoxia_off"
NAME <- "soil_A3_initial"

A3.SOIL.OLD <- layer_params()

A3.SOIL.NEW <- layer_params(thick         = c(0.4, 0.4, 0.6, 1,   7),
                            sand          = c(20,  14,  14,  24,  7),
                            clay          = c(21,  26,  25,  25,  32),
                            organicMatter = c(2.4, 1.6, 1.4, 1.2, 3.2))

A3.SOIL.HI.SAND <- layer_params(sand = c(20,  14,  14,  24,  7) + 10)
A3.SOIL.LO.SAND <- layer_params(sand = c(20,  14,  14,  24,  10) - 10)
A3.SOIL.HI.CLAY <- layer_params(clay = c(21,  26,  25,  25,  32) + 10)
A3.SOIL.LO.CLAY <- layer_params(clay = c(21,  26,  25,  25,  32) - 10)
A3.SOIL.HI.OM   <- layer_params(organicMatter = c(2.4, 1.6, 1.4, 1.2, 3.2) + 1)
A3.SOIL.LO.OM   <- layer_params(organicMatter = c(2.4, 1.6, 1.4, 1.2, 3.2) - 1)

A3.soil.hip <- define_hisafe(path           = PATH,
                             exp.name       = NAME,
                             profiles            = PROFILES,
                             template            = "restinclieres_agroforestry",
                             SimulationName      = c("old", "new",
                                                     "high-sand", "low-sand",
                                                     "high-clay", "low-clay",
                                                     "high-om",   "low-om"),
                             coarseRootAnoxiaResistance = 1000,
                             rootAnoxiaHalfLife         = 1000,
                             treeLineOrientation = 344.3,
                             geometryOption      = 3,
                             plotWidth           = 13,
                             plotHeight          = 8,
                             spacingBetweenRows  = 13,
                             spacingWithinRows   = 8,
                             # heightDbhAllometricCoeffB  = 0.71,
                             # stemDbhAllometricCoeffC    = 0.86,
                             # leafExpansionDuration      = 76,
                             # budBurstToLeafFallDuration = 175,
                             # leafAreaCrownVolCoefA      = 5.34,
                             # leafAreaCrownVolCoefB      = 0.61,
                             tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                             layers              = c(A3.SOIL.OLD, A3.SOIL.NEW,
                                                     A3.SOIL.HI.SAND, A3.SOIL.LO.SAND,
                                                     A3.SOIL.HI.CLAY, A3.SOIL.LO.CLAY,
                                                     A3.SOIL.HI.OM, A3.SOIL.LO.OM),
                             ph                  = 8.4,
                             lueMax              = 0.7,
                             weatherFile         = A3.WEATHER)

build_hisafe(A3.soil.hip)

dum <- map(c("old", "new",
             "high-sand", "low-sand",
             "high-clay", "low-clay",
             "high-om",   "low-om"),
           build_cluster_script,
           script.path  = paste0(PATH, NAME),
           #cluster.path = paste0("/nfs/work/hisafe/kevin/", NAME, "/"),
           cluster.path = paste0("/nfs/work/hisafe/isabelle/soiltest/", NAME, "/"),
           email        = "wolzkevin@gmail.com")

# run_hisafe(#path         = "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/simulations/soil_A3/",
#            hip = A3.soil.hip,
#            parallel    = TRUE,
#            num.cores   = 4,
#            capsis.path = "/Applications/Capsis/")

hop <- read_hisafe(path = paste0(PATH, NAME),
                   simu.names = c("old", "new",
                                  "high-sand", "low-sand",
                                  "high-clay", "low-clay",
                                  "high-om",   "low-om"),
                   profiles   = PROFILES)

hop <- hop_date_filter(hop     ,
                       date.min = NA,
                       date.max = "2018-01-01")

dum <- map(paste0(PATH, NAME, c("/analysis/cycles/", "/analysis/calibration/")),
           dir.create,
           showWarnings = FALSE,
           recursive    = TRUE)

carbon.plot <- plot_hisafe_cycle(hop, "carbon")
ggsave_fitmax(paste0(PATH, NAME, "/analysis/cycles/carbon.png"), carbon.plot, scale = 1.5)

light.plot <- plot_hisafe_cycle(hop, "light")
ggsave_fitmax(paste0(PATH, NAME, "/analysis/cycles/light.png"), light.plot, scale = 1.5)

sim.names <- c("old", "new",
               "high-sand", "low-sand",
               "high-clay", "low-clay",
               "high-om",   "low-om")
cycles    <- "light" #c("water", "nitrogen", "light")
for(cycle in cycles) {
  use.plots <- purrr::map(sim.names,
                          plot_hisafe_use,
                          hop   = hop,
                          cycle = cycle,
                          years = "all")
  purrr::walk2(as.list(paste0(PATH, NAME, "/analysis/cycles/", cycle, "_use_", sim.names, ".png")),
               use.plots,
               ggsave,
               scale = 1, height = 15, width = 15)
}

purrr::walk(as.list(PROFILES[PROFILES %in% c("annualtree", "annualplot", "trees", "plot", "climate")]),
            diag_hisafe_ts,
            hop = hop)

##### PHENO #####
NAME <- "A3_pheno"
hip <- define_hisafe(factorial      = TRUE,
                     path           = PATH,
                     exp.name       = NAME,
                     profiles            = PROFILES,
                     template            = "restinclieres_agroforestry",
                     coarseRootAnoxiaResistance = 1000,
                     rootAnoxiaHalfLife         = 1000,
                     treeLineOrientation = 344.3,
                     geometryOption      = 3,
                     plotWidth           = 13,
                     plotHeight          = 8,
                     spacingBetweenRows  = 13,
                     spacingWithinRows   = 8,
                     # heightDbhAllometricCoeffB  = 0.71,
                     # stemDbhAllometricCoeffC    = 0.86,
                     # leafExpansionDuration      = 76,
                     budBurstToLeafFallDuration = seq(160,200,5),
                     # leafAreaCrownVolCoefA      = 5.34,
                     # leafAreaCrownVolCoefB      = 0.61,
                     tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                     ph                  = 8.4,
                     lueMax              = c(0.6, 0.7),
                     weatherFile         = A3.WEATHER)

build_hisafe(hip)

build_cluster_script(hip          = hip,
                     cluster.path = paste0("/nfs/work/hisafe/isabelle/pheno/", NAME, "/"),
                     email        = "wolzkevin@gmail.com")

hop <- read_hisafe(hip        = hip,
                   simu.names = c("Sim_1", "Sim_3", "Sim_9", "Sim_12"),
                   profiles   = PROFILES,
                   date.max   = "2018-01-01")

hop <- hop_rename(hop       = hop,
                  old.names = hop$exp.plan$SimulationName,
                  new.names = paste0(hop$exp.plan$budBurstToLeafFallDuration, "-", hop$exp.plan$lueMax))

analyze_hisafe(hop,
               aes.cols = list(color    = "budBurstToLeafFallDuration",
                               linetype = "lueMax"))

six   <- hop$exp.plan$SimulationName[str_detect(hop$exp.plan$SimulationName, "0.6")]
seven <- hop$exp.plan$SimulationName[str_detect(hop$exp.plan$SimulationName, "0.7")]

carbon.plot <- plot_hisafe_cycle(hop_filter(hop, six), "carbon")
ggsave_fitmax(paste0(PATH, NAME, "/analysis/cycles/carbon_6.png"), carbon.plot, scale = 1.5)
carbon.plot <- plot_hisafe_cycle(hop_filter(hop, seven), "carbon")
ggsave_fitmax(paste0(PATH, NAME, "/analysis/cycles/carbon_7.png"), carbon.plot, scale = 1.5)

light.plot <- plot_hisafe_cycle(hop_filter(hop, six), "light")
ggsave_fitmax(paste0(PATH, NAME, "/analysis/cycles/light_6.png"), light.plot, scale = 1.5)
light.plot <- plot_hisafe_cycle(hop_filter(hop, seven), "light")
ggsave_fitmax(paste0(PATH, NAME, "/analysis/cycles/light_7.png"), light.plot, scale = 1.5)

##### WATER TABLE OFF #####
NAME <- "soil_A3_wtoff"
A3.soil.hip <- define_hisafe(path           = PATH,
                             exp.name       = NAME,
                             profiles            = PROFILES,
                             template            = "restinclieres_agroforestry",
                             SimulationName      = c("old", "new",
                                                     "high-sand", "low-sand",
                                                     "high-clay", "low-clay",
                                                     "high-om",   "low-om"),
                             #coarseRootAnoxiaResistance = 1000,
                             #rootAnoxiaHalfLife         = 1000,
                             waterTable = "false",
                             treeLineOrientation = 344.3,
                             geometryOption      = 3,
                             plotWidth           = 13,
                             plotHeight          = 8,
                             spacingBetweenRows  = 13,
                             spacingWithinRows   = 8,
                             # heightDbhAllometricCoeffB  = 0.71,
                             # stemDbhAllometricCoeffC    = 0.86,
                             # leafExpansionDuration      = 76,
                             # budBurstToLeafFallDuration = 175,
                             # leafAreaCrownVolCoefA      = 5.34,
                             # leafAreaCrownVolCoefB      = 0.61,
                             tree.initialization = tree_init_params(treeX = 6.5, treeY = 3.5),
                             layers              = c(A3.SOIL.OLD, A3.SOIL.NEW,
                                                     A3.SOIL.HI.SAND, A3.SOIL.LO.SAND,
                                                     A3.SOIL.HI.CLAY, A3.SOIL.LO.CLAY,
                                                     A3.SOIL.HI.OM, A3.SOIL.LO.OM),
                             ph                  = 8.4,
                             lueMax              = 0.7,
                             weatherFile         = A3.WEATHER)

build_hisafe(A3.soil.hip)

dum <- map(c("old", "new",
             "high-sand", "low-sand",
             "high-clay", "low-clay",
             "high-om",   "low-om"),
           build_cluster_script,
           script.path  = paste0(PATH, NAME),
           #cluster.path = paste0("/nfs/work/hisafe/kevin/", NAME, "/"),
           cluster.path = paste0("/nfs/work/hisafe/isabelle/soiltest/", NAME, "/"),
           email        = "wolzkevin@gmail.com")

hop <- read_hisafe(path = paste0(PATH, NAME),
                   simu.names = c("old", "new",
                                  "high-sand", "low-sand",
                                  "high-clay", "low-clay",
                                  "high-om",   "low-om"),
                   profiles   = PROFILES,
                   date.max   = "2018-01-01")

analyze_hisafe(hop)

##### TREE PLACEMENT #####
NAME <- "tree_placement_full"

x <- seq(0.5, 12.5, 2)
y <- seq(0.5, 7.5, 2)
positions <- expand.grid(x = x, y = y)
create_tree_init <- function(x, y) tree_init_params(treeX = x, treeY = y)[[1]]
TREE.INITIALIZATION <- purrr::map2(positions$x, positions$y, create_tree_init)

hip <- define_hisafe(path                 = PATH,
                      exp.name            = NAME,
                      profiles            = c("annualplot", "annualtree"), #PROFILES,
                      template            = "restinclieres_agroforestry",
                      treeLineOrientation = 344.3,
                      geometryOption      = 3,
                      plotWidth           = 13,
                      plotHeight          = 8,
                      spacingBetweenRows  = 13,
                      spacingWithinRows   = 8,
                      ph                  = 8.4,
                      lueMax              = 0.7,
                      weatherFile         = A3.WEATHER,
                      tree.initialization = TREE.INITIALIZATION)

build_hisafe(hip)

build_cluster_script(hip          = hip,
                     cluster.path = paste0("/nfs/work/hisafe/isabelle/treeloc/", NAME, "/"),
                     email        = "wolzkevin@gmail.com")

hop <- read_hisafe(hip        = hip,
                   profiles   = c("annualtree"), #PROFILES,
                   date.max   = "2018-01-01")

diag_hisfe_ts(hop, "annualtree")
#analyze_hisafe(hop)
