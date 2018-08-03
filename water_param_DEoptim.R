### hisafe water module parameter optimization
### Author: Kevin J. Wolz

library(hisafer)
library(tidyverse)
library(parallel)
library(DEoptim)
library(plotly)

N.ITER <- 3
TRACE  <- TRUE
YEARS  <- 1994
METHOD <- "DISCRETE" # TOTAL
CROPS  <- c("durum-wheat-restinclieres", "weed-restinclieres", "rape", "winter-pea")

BASE.PATH <- "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe_testing/"
REFERENCE.PROFILES <- c("cells", "voxels", "voxelsDetail", "plot")
DELETE <- TRUE

input.path <- "./raw_data/"
BASE.PATH  <- "./output/water_param_optimization/"
PARAMS <- read_csv(paste0(input.path, "crop_water_calibration_parameters.csv"), col_types = cols())

for(CROP in CROPS) {
  #CROP = CROPS[1]
  PATH <- paste0(BASE.PATH, CROP, "/", METHOD, "/")
  dir.create(PATH, showWarnings = FALSE, recursive = TRUE)

  common.params <- list(nbSimulations        = length(YEARS),
                        waterTable           = 0,
                        simulationYearStart  = YEARS[1],
                        mainCropSpecies      = paste0(CROP, ".plt"),
                        mainCropItk          = paste0(CROP, ".tec"),
                        interCropSpecies     = paste0(CROP, ".plt"),
                        interCropItk         = paste0(CROP, ".tec"),
                        layers               = layer_params(template = "monocrop",
                                                            thick = c(0.4, 0.4, 0.6, 0.6)))

  ##### REFERENCE SIMULATION #####
  ref.hip <- define_hisafe(path                 = PATH,
                           template             = "monocrop",
                           profiles             = REFERENCE.PROFILES,
                           SimulationName       = "stics",
                           sticsWaterExtraction = 1,
                           bulk.pass            = common.params)
  build_hisafe(ref.hip, plot.scene = FALSE, summary.files = FALSE)
  run_hisafe(ref.hip, capsis.path = "/Applications/Capsis/")
  ref.hop <- read_hisafe(path          = PATH,
                         simu.name     = "stics",
                         profiles      = REFERENCE.PROFILES,
                         show.progress = FALSE,
                         read.inputs   = FALSE)

  lai.output <- ref.hop$cells %>%
    dplyr::select(Day, Month, Year, JulianDay, lai)
  write_delim(lai.output, paste0(PATH, "lai.obs"), delim = "\t")

  GROWTH.DATES <- ref.hop$cells %>%
    dplyr::filter(lai > 0) %>%
    .$Date %>%
    range()

  if(METHOD == "ALL") {
    STICS <- ref.hop$voxels %>%
      dplyr::filter(Date >= GROWTH.DATES[1], Date <= GROWTH.DATES[2]) %>%
      dplyr::select(Date, z, cropWaterUptake) %>%
      dplyr::rename(stics = cropWaterUptake)
  } else if(METHOD == "TOTAL") {
    STICS <- ref.hop$voxels %>%
      dplyr::filter(Date >= GROWTH.DATES[1], Date <= GROWTH.DATES[2]) %>%
      .$cropWaterUptake %>%
      sum()
  }

  ##### WATER COMP FUNCTION #####
  water_comp <- function(params) {
    cat(paste0("\n", paste(params, collapse = "\t")), file = paste0(PATH, "log_file.txt"), append = TRUE)
    params <- list(cropRootDiameter                = params[1],
                   cropRootConductivity            = params[2],
                   cropAlpha                       = params[3],
                   cropMinTranspirationPotential   = params[4],
                   cropMaxTranspirationPotential   = params[4] + params[5],
                   cropBufferPotential             = params[6],
                   cropLongitudinalResistantFactor = params[7])

    NAME <- gsub("0\\.", "", paste("sim", paste(params, collapse = "_"), sep = "_"))

    hip <- define_hisafe(path                 = PATH,
                         template             = "monocrop",
                         profiles             = "voxelsOptim",
                         SimulationName       = NAME,
                         sticsWaterExtraction = 0,
                         laiFileName          = "lai.obs",
                         bulk.pass            = c(common.params, params))
    build_hisafe(hip, plot.scene = FALSE)
    dum <- file.copy(paste0(PATH, "lai.obs"), paste0(hip$path, "/", NAME, "/lai.obs"))

    run_hisafe(hip, capsis.path = "/Applications/Capsis/", quietly = TRUE)

    hop <- read_hisafe(hip,
                       profiles      = "voxelsOptim",
                       show.progress = FALSE,
                       read.inputs   = FALSE)

    if(DELETE) dum <- unlink(paste0(hip$path, "/", NAME), recursive = TRUE)

    if(METHOD == "ALL") {
      HISAFE <- hop$voxels %>%
        dplyr::filter(Date >= GROWTH.DATES[1], Date <= GROWTH.DATES[2]) %>%
        dplyr::select(Date, z, cropWaterUptake) %>%
        dplyr::rename(hisafe = cropWaterUptake)

      rmse <- HISAFE %>%
        dplyr::left_join(STICS, by = c("Date", "z")) %>%
        dplyr::mutate(sqdif = (hisafe - stics) ^ 2) %>%
        dplyr::summarize(rmse = sqrt(mean(sqdif))) %>%
        .$rmse
    } else if(METHOD == "TOTAL") {
      HISAFE <- hop$voxels %>%
        dplyr::filter(Date >= GROWTH.DATES[1], Date <= GROWTH.DATES[2]) %>%
        .$cropWaterUptake %>%
        sum()

      rmse <- abs(STICS - HISAFE)
    }

    cat(paste0("\t", rmse), file = paste0(PATH, "log_file.txt"), append = TRUE)
    return(rmse)
  }

  ##### OPTIMIZATION #####
  # INITIAL.POP <- as.matrix(expand.grid(c(0.005,   0.065, 0.02),
  #                                      c(0.20,     0.12, 0.1),
  #                                      c(-27000, -24000, -30000),
  #                                      c(25000,   20000)))

  mappingFun <- function(x) {
    x[1] <- round(x[1] / 0.001)    * 0.001
    x[2] <- round(x[2] / 0.000001) * 0.000001
    x[3] <- round(x[3] / 0.01)     * 0.01
    x[4] <- round(x[4] / 1000)     * 1000
    x[5] <- round(x[5] / 1000)     * 1000
    x[6] <- round(x[6] / 0.01)     * 0.01
    x[7] <- round(x[7] / 1)        * 1
    return(x)
  }

  set.seed(333)
  DEout <- DEoptim(fn      = water_comp,
                   lower   = PARAMS$param.min,
                   upper   = PARAMS$param.mx,
                   control = DEoptim.control(itermax = N.ITER,
                                             trace   = TRACE),
                   fnMap   = mappingFun)
  #DEout$optim
  #DEout$member

  out <- DEout$optim$bestmem %>%
    matrix(nrow = 1) %>%
    as_tibble()
  names(out) <- PARAMS$param.name
  write_csv(out, paste0(PATH, CROP, "_", METHOD, "_optimized_water_params.csv"))
  save(DEout, file = paste0(PATH, CROP, "_", METHOD, "_Water_Param_Optimization.RData"))

  ##### TEST FINAL SOLUTION #####
  old.winner <- as.numeric(PARAMS[CROP])
  new.winner <- DEout$optim$bestmem

  params <- list(cropRootDiameter                = c(new.winner[1], old.winner[1]),
                 cropRootConductivity            = c(new.winner[2], old.winner[2]),
                 cropAlpha                       = c(new.winner[3], old.winner[3]),
                 cropMinTranspirationPotential   = c(new.winner[4], old.winner[4]),
                 cropMaxTranspirationPotential   = c(new.winner[4] + new.winner[5], old.winner[4] + old.winner[5]),
                 cropBufferPotential             = c(new.winner[6], old.winner[6]),
                 cropLongitudinalResistantFactor = c(new.winner[7], old.winner[7]))

  win.hip <- define_hisafe(path                 = PATH,
                           exp.name             = "hisafe",
                           template             = "monocrop",
                           profiles             = REFERENCE.PROFILES,
                           SimulationName       = c("new_winner", "old_winner"),
                           sticsWaterExtraction = 0,
                           laiFileName          = "lai.obs",
                           bulk.pass            = c(common.params, params))
  build_hisafe(win.hip, plot.scene = FALSE)
  dum <- file.copy(paste0(BASE.PATH, "lai.obs"), paste0(win.hip$path, "/", c("new_winner", "old_winner"), "/lai.obs"))

  run_hisafe(win.hip,
             capsis.path = "/Applications/Capsis/",
             parallel    = TRUE,
             num.cores   = 2,
             quietly     = TRUE)

  win.hop <- read_hisafe(win.hip,
                         profiles      = REFERENCE.PROFILES,
                         show.progress = FALSE,
                         read.inputs   = FALSE)

  hop <- hop_merge(ref.hop, win.hop)
  hop$exp.path <- PATH
  dum <- purrr::map(paste0(PATH, c("voxels", "cells")), dir.create, showWarnings = FALSE)

  ##### PLOTS #####
  voxels.to.plot <- seq(0.1, 1.1, 0.2)
  diag_hisafe_voxels(hop,
                     output.path = PATH,
                     date.min    = paste0(min(YEARS), "-12-01"),
                     date.min    = paste0(max(YEARS + 1), "-7-01"),
                     X           = voxels.to.plot,
                     facet.simu  = FALSE,
                     facet.z     = TRUE)

  diag_hisafe_ts(hop,
                 profile     = "cells",
                 output.path = PATH,
                 date.min    = paste0(min(YEARS), "-12-01"),
                 date.min    = paste0(max(YEARS + 1), "-7-01"))

  diag_hisafe_ts(hop,
                 profile     = "plot",
                 output.path = PATH,
                 date.min    = paste0(min(YEARS), "-12-01"),
                 date.min    = paste0(max(YEARS + 1), "-7-01"))

  ## GA Diagnostics
  DE <- tibble(RMSE = DEout$member$bestvalit) %>%
    mutate(Generation = 1:nrow(.))

  de.plot <- ggplot(DE, aes(x = Generation, y = RMSE)) +
    geom_line() +
    theme_hisafe_ts() +
    theme(panel.grid = element_blank())
  ggsave_fitmax(paste0(PATH, CROP, "_", METHOD, "_RMSE_GA_Trajectory.png"), de.plot)

  # plot_hisafe_voxels(hop,
  #                    variable   = "cropWaterUptake",
  #                    date.min   = paste0(min(YEARS), "-12-01"),
  #                    date.min   = paste0(max(YEARS + 1), "-7-01"),
  #                    X          = voxels.to.plot,
  #                    facet.simu = FALSE,
  #                    facet.z    = TRUE)

  # test.plot <- ggplot(filter(hop$voxels, z <= 1.1),
  #                     aes(x     = Date,
  #                         y     = cropNitrogenUptake,
  #                         color = SimulationName)) +
  #   geom_line(size = 1, na.rm = TRUE) +
  #   scale_x_date(limits = lubridate::ymd(c(paste0(YEAR,     "-12-01"),
  #                                          paste0(YEAR + 1, "-7-01")))) +
  #   facet_wrap(~z, ncol = 1) +
  #   scale_color_manual(values = c("black", "red", "blue"))
  # ggplotly(test.plot)

  # test.plot <- ggplot(hop$plot,
  #                     aes(x     = Date,
  #                         y     = mainCropMeanBiomass,
  #                         color = SimulationName)) +
  #   geom_line(size = 1, na.rm = TRUE) +
  #   scale_x_date(limits = lubridate::ymd(c(paste0(YEAR,     "-12-01"),
  #                                          paste0(YEAR + 1, "-7-01")))) +
  #   scale_color_manual(values = c("black", "red", "blue"))
  # ggplotly(test.plot)
  #
  #
  # plot_hop <- hop %>%
  #   hop_filter(c("stics", "new_winner"))# %>%
  # #hop_rename(c("stics", "new_winner"), c("stics", "hisafe"))
  #
  # for(i in names(hop$voxels)[13:60]) {
  #   voxel.plot <- ggplot(filter(plot_hop$voxels, z <= 1.1),
  #                        aes_string(x     = "Date",
  #                                   y     = i,
  #                                   color = "SimulationName")) +
  #     geom_line(size = 1, na.rm = TRUE) +
  #     scale_x_date(limits = lubridate::ymd(c(paste0(YEAR,     "-12-01"),
  #                                            paste0(YEAR + 1, "-7-01")))) +
  #     facet_wrap(~z, ncol = 1) +
  #     scale_color_manual(values = c("black", "red", "blue"))
  #   ggsave_fitmax(paste0(PATH, "voxels/", i, ".png"), voxel.plot)
  # }
  #
  # for(i in names(hop$cells)[13:28]) {
  #   cell.plot <- ggplot(plot_hop$cells,
  #                       aes_string(x     = "Date",
  #                                  y     = i,
  #                                  color = "SimulationName")) +
  #     geom_line(size = 1, na.rm = TRUE) +
  #     scale_x_date(limits = lubridate::ymd(c(paste0(YEAR,     "-12-01"),
  #                                            paste0(YEAR + 1, "-7-01")))) +
  #     scale_color_manual(values = c("black", "red", "blue"))
  #   ggsave_fitmax(paste0(PATH, "cells/", i, ".png"), cell.plot)
  # }
  #
  # for(i in names(hop$plot)[11:155]) {
  #   plot.plot <- ggplot(plot_hop$plot,
  #                       aes_string(x     = "Date",
  #                                  y     = i,
  #                                  color = "SimulationName")) +
  #     geom_line(size = 1, na.rm = TRUE) +
  #     scale_x_date(limits = lubridate::ymd(c(paste0(YEAR,     "-12-01"),
  #                                            paste0(YEAR + 1, "-7-01")))) +
  #     scale_color_manual(values = c("black", "red", "blue"))
  #   ggsave_fitmax(paste0(PATH, "plot/", i, ".png"), plot.plot)
  # }

}
