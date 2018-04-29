### hisafe-calibration
### BUILD WATER TABLE
### Author: Kevin J. Wolz

OPTIMIZE <- FALSE

PLOTS       <- paste0("A", 2:4)
LATITUDE    <- 43.7
PARAM.NAMES <- c("init", "a", "b", "l", "m", "n", "ru", "prof_nappe_max")
LB          <- c(-500, 1e-6, 2, 10,  1,  0.1, 100, 300)
UB          <- c(-100, 1e-4, 3, 150, 10, 1,   300, 700)
N.ITER      <- 1000
N.POP       <- 100

input.path <- "./raw_data/"
wt.path    <- "./output/water_table/"

library(hisafer)
library(tidyverse)
library(lubridate)
library(DEoptim)
library(lhs)
source("water_table_functions.R")

##### BASE RESTINCLIERES WEATHER #####
## Data sources:
## Restinclieres Chatau/Plots: temp, humidity, rain, wind
## Lavalette: radiation
## Fixed: CO2
## File name appendix refers to when the data was last updated
last.updated <- "20180416"
base.wth <- read_csv(paste0(input.path, "restinclieres_weather_base-1994-2018_", last.updated, ".csv"),
                     guess_max = 10000,
                     col_types = cols()) %>%
  mutate(Tavg  = (Tmax + Tmin) / 2) %>%
  mutate(RHavg = (RHmax + RHmin) / 2) %>%
  mutate(date  = lubridate::ymd(paste(year, month, day, sep = "-")))

## Clean up obvious errors
# ggplot(base.wth, aes(x = JULIAN, y = RG)) + geom_line() + facet_wrap(~YR)
base.wth$Rg[base.wth$year == 1996 & base.wth$month == 12 & base.wth$day == 3]  <- 5
base.wth$Rg[base.wth$year == 2008 & base.wth$month == 3  & base.wth$day == 31] <- 5
base.wth$Rg[base.wth$year == 2008 & base.wth$month == 4  & base.wth$day == 4]  <- 5
base.wth$Rg[base.wth$year == 2015 & base.wth$month == 9  & base.wth$day == 11] <- 5
base.wth$Rg[base.wth$year == 2007 & base.wth$Rg > 35] <- 30

##### PIEZOMETER DATA #####
piezo.info <- read_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/raw_data/restinclieres_piezometer_info.csv",
                       col_types = cols()) %>%
  select(-tube.height.orig, -tube.height.6603)

piezo <- read_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/raw_data/restinclieres_piezometer_data.csv",
                  col_types = cols()) %>%
  left_join(piezo.info, by = c("plot", "rowtree.id")) %>%
  mutate(date  = lubridate::mdy(date)) %>%
  mutate(doy   = lubridate::yday(date)) %>%
  mutate(year  = lubridate::year(date)) %>%
  mutate(depth = pmin(-(depth - tube.height) / 100, 0)) %>%
  rename(watertable = depth) %>%
  filter(!is.na(watertable)) %>%
  filter(include == TRUE) %>%
  select(-include, -tube.height) %>%
  group_by(plot, year, doy) %>%
  summarize(watertable = mean(watertable)) %>%
  ungroup()

##### OPTIMIZATION #####
# A4, A3, A2
WINNER.SEED <- matrix(c(c(-150.196435, -241.7456884, -422.0801519),
                        c(1.01E-05,    1.98E-05,     1.94E-05),
                        c(2.335965839, 2.20898286,   2.467391203),
                        c(50.06030294, 51.73952448,  127.2751257),
                        c(5.44952316,  3.000046226,  8.700402468),
                        c(0.563544897, 0.691394394,  0.642158705),
                        c(142.7697915, 117.5991857,  118.8626063),
                        c(350.3129838, 446.0560438,  656.6459902)), nrow = 3)

INITIAL.POP <- improvedLHS(n = N.POP - 3, k = length(PARAM.NAMES))
for(i in 1:ncol(INITIAL.POP)) INITIAL.POP[, i] <- scales::rescale(INITIAL.POP[, i], c(LB[i], UB[i]), c(0, 1))
INITIAL.POP <- rbind(INITIAL.POP, WINNER.SEED)

if(OPTIMIZE){
  for(PLOT in PLOTS) {
    piezo.plot <- piezo %>%
      filter(plot == PLOT)
    DEcontrol <- DEoptim.control(itermax      = N.ITER,
                                 NP           = N.POP,
                                 initialpop   = INITIAL.POP,
                                 trace        = TRUE,
                                 parallelType = 1,
                                 packages     = "dplyr",
                                 parVar       = c("compute_nappe", "getETP", "getDelta", "getVpSat", "getExtraRad", "predNappe"))

    set.seed(333)
    DEout <- DEoptim(fn      = water_table_comp,
                     lower   = LB,
                     upper   = UB,
                     control = DEcontrol,
                     wth        = base.wth,
                     latitude   = LATITUDE,
                     piezo.data = piezo.plot)
    out <- DEout$optim$bestmem %>%
      matrix(nrow = 1) %>%
      as_tibble()
    names(out) <- PARAM.NAMES
    write_csv(out, paste0(wt.path, PLOT, "_optimized_water_table_params.csv"))
    save(DEout, file = paste0(wt.path, PLOT, "_Water_Table_Optimization.RData"))
  }
}

##### PLOTS #####
for(PLOT in PLOTS) {
  optimized.params <- read_csv(paste0(wt.path, PLOT, "_optimized_water_table_params.csv"), col_types = cols()) %>%
    as_vector()
  wth.new <- compute_nappe(wth = base.wth, params = optimized.params, latitude = LATITUDE) %>%
    select(-Tavg, -RHavg, -date)
  #write_weather(wth.new, paste0(wt.path, "restinclieres_", PLOT, "-1994-2018_NEW.wth"))
  wth.old <- read_weather(paste0(input.path, "old_weather_files/restinclieres_", PLOT, "-1994-2018_OLD.wth"))
  piezo.plot <- piezo %>%
    filter(plot == PLOT) %>%
    select(-plot)

  ts.data <- wth.old %>%
    select(doy, year, watertable) %>%
    rename(old = watertable) %>%
    left_join(wth.new, by = c("year", "doy")) %>%
    select(doy, year, old, watertable) %>%
    rename(new = watertable) %>%
    gather(key = "category", value = "modeled", old, new) %>%
    mutate(category = factor(category, levels = c("old", "new")))

  comp.data <- ts.data %>%
    left_join(piezo.plot, by = c("year", "doy")) %>%
    rename(measured = watertable) %>%
    filter(!is.na(measured))

  improv <- comp.data %>%
    group_by(category) %>%
    summarize(rmse = sqrt(mean((measured - modeled) ^ 2))) %>%
    mutate(rmse = paste("RMSE =", format(rmse, digits = 2))) %>%
    mutate(modeled  = -7,
           measured = 0)

  ts.plot <- ggplot(ts.data, aes(x = doy)) +
    labs(x     = "Day of year",
         y     = "Water table depth (m)",
         color = NULL) +
    scale_y_continuous(limits = c(-7, 0)) +
    geom_line(aes(y = modeled, color = category)) +
    scale_color_manual(values = c("red", "black")) +
    geom_point(data = piezo.plot, aes(y = watertable), color = "black") +
    facet_wrap(~year)
  ggsave_fitmax(paste0(wt.path, PLOT, "_Water_Table_TS_Comparison.png"), ts.plot)

  mvm.plot <- ggplot(comp.data, aes(x = modeled,
                                    y = measured)) +
    labs(x = "Modeled depth (m)",
         y = "Measured depth (m)") +
    scale_x_continuous(limits = c(-7, 0)) +
    scale_y_continuous(limits = c(-7, 0)) +
    geom_point() +
    geom_abline() +
    geom_text(data = improv, aes(label = rmse), hjust = -0.1, vjust = 1) +
    facet_wrap(~category) +
    theme_hisafe_ts() +
    theme(panel.grid = element_blank())
  ggsave_fitmax(paste0(wt.path, PLOT, "_Water_Table_MvM.png"), mvm.plot)

  ## GA Diagnostics
  load(paste0(wt.path, PLOT, "_Water_Table_Optimization.RData"))
  DE <- tibble(RMSE = DEout$member$bestvalit) %>%
    mutate(Generation = 1:nrow(.))

  de.plot <- ggplot(DE, aes(x = Generation, y = RMSE)) +
    geom_line() +
    theme_hisafe_ts() +
    theme(panel.grid = element_blank())
  ggsave_fitmax(paste0(wt.path, PLOT, "_RMSE_GA_Trajectory.png"), de.plot)
}
