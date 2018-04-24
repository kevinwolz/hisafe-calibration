### hisafe-calibration
### CALIBRATION
### Author: Kevin J. Wolz

REGENERATE.LHS <- TRUE
N.SIMUS <- 250

library(hisafer)
library(tidyverse)
library(ggradar)
library(lhs)
library(scales)
library(rPref)

cbPalette  <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

input.path      <- "./raw_data/"
simulation.path <- "./simulations/"
lhs.output.path <- "./output/LHS/"

PARAMS <- read_csv(paste0(input.path, "hisafe_calibration_parameters.csv"), col_types = cols()) %>%
  filter(calibrate == TRUE)

N.PARAMS <- nrow(PARAMS)
N.SWEEPS <- 2#100

round_vals <- function(df) {
  for(i in names(df)) {
    sig <- PARAMS$sig.level[PARAMS$param.name == i]
    df[i] <- round(df[i] / sig) * sig
  }
  return(df)
}

PATH       <- paste0(simulation.path, "LHS")
PROFILES   <- "annualDBH"
FILES      <- c("sim", "tree")
A2.WEATHER <- "./raw_data/restinclieres_A2-1994-2018.wth"
A3.WEATHER <- "./raw_data/restinclieres_A3-1994-2018.wth"
A4.WEATHER <- "./raw_data/restinclieres_A4-1994-2018.wth"

##### GENERATE LATIN HYPRECUBE SAMPLE SET #####
if(REGENERATE.LHS) {
  calib.sample <- lat.hyp.samp <- optimumLHS(n         = N.SIMUS,
                                             k         = N.PARAMS,
                                             maxSweeps = N.SWEEPS,
                                             verbose   = TRUE)
  write.table(lat.hyp.samp, paste0(lhs.output.path, "hisafe_calibration_LHS_raw.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

  for(i in 1:ncol(calib.sample)) calib.sample[, i] <- scales::rescale(calib.sample[, i], c(PARAMS$param.min[i], PARAMS$param.max[i]), c(0, 1))
  calib.sample <- as_tibble(calib.sample)
  names(calib.sample) <- PARAMS$param.name
  calib.sample <- calib.sample %>%
    round_vals() %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, everything())
  write_csv(calib.sample, paste0(lhs.output.path, "hisafe_calibration_LHS.csv"))
} else {
  calib.sample <- read_csv(paste0(lhs.output.path, "hisafe_calibration_LHS.csv"), col_types = cols())
}

##### BULID DEFAULT FOLDERS #####
A3.template <- define_hisafe(path     = paste0(simulation.path, "LHS_default_folders"),
                             template = "restinclieres_agroforestry_A3",
                             SimulationName = "A3",
                             weatherFile = A3.WEATHER)
build_hisafe(hip           = A3.template,
             files         = c("tec", "plt", "pro", "par", "pld", "wth"),
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/raw_data/A3_horizontal_root_data.cal",
                 to   = paste0(simulation.path, "LHS_default_folders/A3/A3_horizontal_root_data.cal"))

A2.template <- define_hisafe(path     = paste0(simulation.path, "LHS_default_folders"),
                             template = "restinclieres_agroforestry_A2",
                             SimulationName = "A2",
                             weatherFile = A2.WEATHER)
build_hisafe(hip           = A2.template,
             files         = c("tec", "plt", "pro", "par", "pld", "wth"),
             plot.scene    = FALSE,
             summary.files = FALSE)

A4.template <- define_hisafe(path     = paste0(simulation.path, "LHS_default_folders"),
                             template = "restinclieres_forestry_A4",
                             SimulationName = "A4",
                             weatherFile = A4.WEATHER)
build_hisafe(hip           = A4.template,
             files         = c("tec", "plt", "pro", "par", "pld", "wth"),
             plot.scene    = FALSE,
             summary.files = FALSE)

##### BUILD A3 SIMUILATIONS #####
common.params <- calib.sample %>%
  select(-id) %>%
  as.list()

A3.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A3",
                        SimulationName = paste0("LHS_A3_", calib.sample$id),
                        bulk.pass      = common.params,
                        exp.name       = "LHS_A3",
                        weatherFile    = A3.WEATHER)

build_hisafe(hip           = A3.hip,
             files         = FILES,
             plot.scene    = FALSE,
             summary.files = FALSE)

build_cluster_script(hip            = A3.hip,
                     launch.call    = "ScriptCalib",
                     default.folder = "A3",
                     cluster.path   = "/lustre/lecomtei/LHS/LHS_A3",
                     script.path    = PATH,
                     script.name    = "LHS_A3_Launch",
                     email.type     = "ALL",
                     email          = "wolzkevin@gmail.com")

##### DETERMINE WHICH A3 SIMULATIONS WERE ABORTED AND WHY #####
abort.files <- list.files(paste0(PATH, "/LHS_A3"), pattern = "abort\\.txt", full.names = TRUE, recursive = TRUE)
abort <- purrr::map(abort.files,
                    read_delim,
                    delim     = ";",
                    col_names = c("SimulationName", "year", "month", "day", "dist.4m", "dist.6m", "full.scene", "dist"),
                    col_types = cols()) %>%
  bind_rows() %>%
  mutate(event = paste0(year, "-", month)) %>%
  mutate(id    = as.numeric(str_remove(SimulationName, "LHS_A[2-4]_"))) %>%
  left_join(calib.sample,  by = "id")

table(abort$dist)

aborted.ids <- abort$SimulationName %>%
  str_remove("LHS_A3_") %>%
  as.numeric()
non.aborted.ids <- calib.sample$id[!(calib.sample$id %in% aborted.ids)]

initial.summary <- tibble(event = "initial", remaining.simulations = N.SIMUS)
abort.summary <- abort %>%
  group_by(event) %>%
  summarize(remaining.simulations = -n()) %>%
  bind_rows(initial.summary, .) %>%
  mutate(remaining.simulations = cumsum(remaining.simulations))

print(as.data.frame(abort.summary), row.names = FALSE)

##### BUILD A2 & A4 SIMULATIONS #####
non.aborted.params <- calib.sample %>%
  filter(id %in% non.aborted.ids) %>%
  select(-id) %>%
  as.list()

A2.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A2",
                        SimulationName = paste0("LHS_A2_", non.aborted.ids),
                        bulk.pass      = non.aborted.params,
                        exp.name       = "LHS_A2",
                        weatherFile    = A2.WEATHER)

build_hisafe(hip           = A2.hip,
             files         = FILES,
             plot.scene    = FALSE,
             summary.files = FALSE)

build_cluster_script(hip            = A2.hip,
                     launch.call    = "ScriptCalib",
                     default.folder = "A2",
                     cluster.path   = "/lustre/lecomtei/LHS/LHS_A2",
                     script.path    = PATH,
                     script.name    = "LHS_A2_Launch",
                     email.type     = "ALL",
                     email          = "wolzkevin@gmail.com")

A4.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_forestry_A4",
                        SimulationName = paste0("LHS_A4_", non.aborted.ids),
                        bulk.pass      = non.aborted.params,
                        exp.name       = "LHS_A4",
                        weatherFile    = A4.WEATHER)

build_hisafe(hip           = A4.hip,
             files         = FILES,
             plot.scene    = FALSE,
             summary.files = FALSE)

build_cluster_script(hip            = A4.hip,
                     launch.call    = "ScriptCalib",
                     default.folder = "A4",
                     cluster.path   = "/lustre/lecomtei/LHS/LHS_A4",
                     script.path    = PATH,
                     script.name    = "LHS_A4_Launch",
                     email.type     = "ALL",
                     email          = "wolzkevin@gmail.com")

##### READ ALL SIMULATIONS #####
## rename all output files to trees.txt
output.files <- list.files(PATH, pattern = "annualDBH\\.txt", full.names = TRUE, recursive = TRUE)
new.names    <- str_replace(output.files, "annualDBH\\.txt$", "annualtree.txt")
dum <- file.rename(output.files, new.names)

## read simulations that did not abort
A2.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A2"),
                        simu.names    = paste0("LHS_A2_", non.aborted.ids),
                        profiles      = "annualtree",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$annualtree
A3.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A3"),
                        simu.names    = paste0("LHS_A3_", non.aborted.ids),
                        profiles      = "annualtree",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$annualtree
A4.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A4"),
                        simu.names    = paste0("LHS_A4_", non.aborted.ids),
                        profiles      = "annualtree",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$annualtree

modeled.data <- bind_rows(A2.trees, A3.trees, A4.trees) %>%
  filter(id == 1) %>% # remove second tree
  mutate(id   = as.numeric(str_remove(SimulationName, "LHS_A[2-4]_"))) %>%
  mutate(plot = purrr::map_chr(str_split(SimulationName, "_"), 2)) %>%
  mutate(year = Year - 1) %>%
  rename(modeled.dbh = dbh) %>%
  mutate(modeled.dbh.inc = c(NA, diff(modeled.dbh))) %>%
  select(plot, id, year, modeled.dbh, modeled.dbh.inc)

##### SETUP DBH MvM #####
data.path       <- "./output/processed_data/"
CALIBRATION.SIMUATIONS <- c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4")
source("field_data.R")

measured.data <- cal.measured.annual %>%
  select(plot, year, measured.dbh, measured.dbh.inc)

dbh.data <- modeled.data %>%
  left_join(measured.data, by = c("plot", "year")) %>%
  mutate(dbh.er2     = (measured.dbh     - modeled.dbh)     ^ 2) %>%
  mutate(dbh.inc.er2 = (measured.dbh.inc - modeled.dbh.inc) ^ 2)

rmse_by_plot <- dbh.data %>%
  group_by(id, plot) %>%
  summarize(rmse.dbh     = sqrt(mean(dbh.er2,     na.rm = TRUE)),
            rmse.dbh.inc = sqrt(mean(dbh.inc.er2, na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(id) %>%
  summarize(rmse.dbh.p     = mean(rmse.dbh),
            rmse.dbh.inc.p = mean(rmse.dbh.inc))

rmse_together <- dbh.data %>%
  group_by(id) %>%
  summarize(rmse.dbh.t     = sqrt(mean(dbh.er2,     na.rm = TRUE)),
            rmse.dbh.inc.t = sqrt(mean(dbh.inc.er2, na.rm = TRUE)))

lhs.data <- rmse_by_plot %>%
  left_join(rmse_together, by = "id") %>%
  left_join(calib.sample,  by = "id")

##### ANALYZE DBH MvM #####
rmse.common <- list(scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)),
                    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)),
                    coord_fixed(),
                    theme_hisafe_ts(panel.grid = element_blank()))

## Compare RMSE calculation approaches
rmse.dbh.comp.plot <- ggplot(lhs.data, aes(x = rmse.dbh.p, y = rmse.dbh.t)) +
  labs(x = "Mean plot DBH RMSE",
       y = "Global DBH RMSE") +
  geom_point() +
  rmse.common +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")
ggsave_fitmax(paste0(lhs.output.path, "RMSE_DBH_Approach_Comp.png"), rmse.dbh.comp.plot)

rmse.dbh.inc.comp.plot <- ggplot(lhs.data, aes(x = rmse.dbh.inc.p, y = rmse.dbh.inc.t)) +
  labs(x = "Mean plot DBH increment RMSE",
       y = "Global DBH increment RMSE") +
  geom_point() +
  rmse.common +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")
ggsave_fitmax(paste0(lhs.output.path, "RMSE_DBHinc_Approach_Comp.png"), rmse.dbh.inc.comp.plot)

lhs.data <- lhs.data %>%
  rename(rmse.dbh     = rmse.dbh.p) %>%
  rename(rmse.dbh.inc = rmse.dbh.inc.p) %>%
  select(-rmse.dbh.t, -rmse.dbh.inc.t)

## Identify Pareto Set
pareto.ids <- psel(lhs.data, low(rmse.dbh) * low(rmse.dbh.inc)) %>%
  .$id %>%
  as.numeric()

lhs.data <- lhs.data %>%
  mutate(pareto = id %in% pareto.ids)

## Visualize solution space
solution.space <- ggplot(lhs.data, aes(x     = rmse.dbh,
                                       y     = rmse.dbh.inc,
                                       size  = pareto,
                                       color = pareto,
                                       fill  = lueMax)) +
  labs(x = "DBH RMSE",
       y = "DBH increment RMSE") +
  geom_point(shape = 21) +
  scale_color_manual(values = c("transparent", "green")) +
  scale_size_manual(values  = c(1, 3)) +
  viridis::scale_fill_viridis(option = "magma") +
  guides(color = FALSE, size = FALSE) +
  rmse.common
ggsave_fitmax(paste0(lhs.output.path, "LHS_Solution_Space.png"), solution.space)


scale_from_ranges <- function(x) {
  orig.names <- names(x)
  temp   <- as.list(x)[names(x) %in% PARAMS$param.name]
  ranges <- cbind(PARAMS$param.min, PARAMS$param.max) %>%
    split(., row(.))
  names(ranges) <- names(temp) <- NULL
  out <- purrr::pmap(list(x    = temp,
                          from = ranges),
                     .f = scales::rescale)
  names(out) <- orig.names[names(x) %in% PARAMS$param.name]
  out <- x[,!(names(x) %in% PARAMS$param.name)] %>%
    bind_cols(as_tibble(out))
  return(out)
}

radar.data <- lhs.data %>%
  filter(pareto) %>%
  select(-pareto) %>%
  scale_from_ranges() %>%
  rename(group = id) %>%
  select(-rmse.dbh, -rmse.dbh.inc)

radar.plot <- ggradar(radar.data,
                      gridline.mid.colour = "grey",
                      group.point.size    = 2,
                      group.line.width    = 1,
                      grid.label.size     = 4,
                      axis.label.size     = 1.5,
                      axis.label.offset   = 1.1,
                      legend.title        = "Simulation ID",
                      legend.text.size    = 10) +
  scale_color_manual(values = cbPalette)
ggsave_fitmax(paste0(lhs.output.path, "Pareto_Radar_Plot.png"), radar.plot)
