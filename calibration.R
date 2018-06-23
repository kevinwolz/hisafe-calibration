### hisafe-calibration
### CALIBRATION
### Author: Kevin J. Wolz

REGENERATE.LHS <- FALSE
N.SIMUS <- 1000

library(hisafer)
library(tidyverse)
library(ggradar)
library(lhs)
library(scales)
library(rPref)
library(lubridate)

cbPalette  <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

input.path      <- "./raw_data/"
simulation.path <- "./simulations/"
lhs.output.path <- "./output/LHS/"

PARAMS <- read_csv(paste0(input.path, "hisafe_calibration_parameters.csv"), col_types = cols()) %>%
  filter(calibrate == TRUE)

N.PARAMS <- nrow(PARAMS)

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
old.top.sims <- read_csv(paste0("./output/LHS_Top_Sims.csv"), col_types = cols()) %>%
  mutate(id = (N.SIMUS + 1):(N.SIMUS + nrow(.)))

if(REGENERATE.LHS) {
  # calib.sample <- lat.hyp.samp <- geneticLHS(n         = N.SIMUS,
  #                                            k         = N.PARAMS,
  #                                            pop       = 100,
  #                                            gen       = 10,
  #                                            pMut      = 0.25,
  #                                            verbose   = TRUE)
  calib.sample <- lat.hyp.samp <- improvedLHS(n = N.SIMUS,
                                              k = N.PARAMS)
  write.table(lat.hyp.samp, paste0(lhs.output.path, "hisafe_calibration_LHS_raw.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

  for(i in 1:ncol(calib.sample)) calib.sample[, i] <- scales::rescale(calib.sample[, i], c(PARAMS$param.min[i], PARAMS$param.max[i]), c(0, 1))
  calib.sample <- as_tibble(calib.sample)
  names(calib.sample) <- PARAMS$param.name
  calib.sample <- calib.sample %>%
    round_vals() %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, everything()) %>%
    bind_rows(old.top.sims)
  write_csv(calib.sample, paste0(lhs.output.path, "hisafe_calibration_LHS.csv"))
} else {
  calib.sample <- read_csv(paste0(lhs.output.path, "hisafe_calibration_LHS.csv"), col_types = cols()) %>%
    bind_rows(old.top.sims)
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

# table(abort$dist)

aborted.ids <- abort$SimulationName %>%
  str_remove("LHS_A3_") %>%
  as.numeric()
non.aborted.ids <- calib.sample$id[!(calib.sample$id %in% aborted.ids)]

## FAILURE DIAGNOSTICS
# filter(calib.sample, id %in% non.aborted.ids)
# filter(abort, event == "2007-11") %>% select(-(SimulationName:full.scene)) #46
# filter(abort, event == "2004-8")  %>% select(-(SimulationName:full.scene)) #4
# filter(abort, event == "2002-4")  %>% select(-(SimulationName:full.scene)) #4
# ggplot(filter(abort, event == "2002-4"),
#        aes(x = colonisationThreshold)) +
#   geom_histogram() +
#   facet_wrap(~dist)

initial.summary <- tibble(event = "initial", remaining.simulations = nrow(calib.sample))
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
                     launch.call    = "ScriptGen",
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
                     launch.call    = "ScriptGen",
                     default.folder = "A4",
                     cluster.path   = "/lustre/lecomtei/LHS/LHS_A4",
                     script.path    = PATH,
                     script.name    = "LHS_A4_Launch",
                     email.type     = "ALL",
                     email          = "wolzkevin@gmail.com")

##### READ ALL SIMULATIONS #####
## rename all output files to trees.txt
output.files <- list.files(PATH, pattern = "annualDBH\\.txt", full.names = TRUE, recursive = TRUE)
new.names    <- str_replace(output.files, "annualDBH\\.txt$", "annualTrees.txt")
dum <- file.rename(output.files, new.names)

## read simulations that did not abort
A2.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A2"),
                        simu.names    = paste0("LHS_A2_", non.aborted.ids),
                        profiles      = "annualTrees",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$annualTrees
A3.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A3"),
                        simu.names    = paste0("LHS_A3_", 1:N.SIMUS),
                        profiles      = "annualTrees",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$annualTrees
A4.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A4"),
                        simu.names    = paste0("LHS_A4_", non.aborted.ids),
                        profiles      = "annualTrees",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$annualTrees

modeled.data.all <- bind_rows(A2.trees, A3.trees, A4.trees) %>% # A3.trees
  filter(id == 1) %>% # remove second tree
  mutate(id   = as.numeric(str_remove(SimulationName, "LHS_A[2-4]_"))) %>%
  mutate(plot = purrr::map_chr(str_split(SimulationName, "_"), 2)) %>%
  mutate(year = Year - 1) %>%
  rename(modeled.dbh = dbh) %>%
  group_by(plot, id) %>%
  mutate(modeled.dbh.inc = c(NA, diff(modeled.dbh))) %>%
  ungroup() %>%
  mutate(aborted = id %in% aborted.ids) %>%
  select(plot, id, year, modeled.dbh, modeled.dbh.inc, aborted)

#ggplot(modeled.data.all, aes(x = year, y = modeled.dbh, group = id, color = aborted)) + geom_line()

modeled.data <- modeled.data.all %>%
  filter(aborted == FALSE) %>%
  select(-aborted)

##### SETUP DBH MvM #####
data.path <- "./output/processed_data/"
CALIBRATION.SIMULATIONS <- c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4")
VALIDATION.SIMULATIONS  <- c("Castries")
source("field_data.R")

measured.trees <- cal.measured.trees %>%
  select(plot, year, measured.dbh, measured.dbh.inc)
measured.annual <- cal.measured.annual %>%
  select(plot, year, measured.dbh, measured.dbh.inc)

dbh.data <- modeled.data %>%
  left_join(measured.annual, by = c("plot", "year")) %>%
  mutate(dbh.er2     = (measured.dbh     - modeled.dbh)     ^ 2) %>%
  mutate(dbh.inc.er2 = (measured.dbh.inc - modeled.dbh.inc) ^ 2)

## Compute RMSE
rmse.by.plot <- dbh.data %>%
  group_by(id, plot) %>%
  summarize(rmse.dbh     = sqrt(mean(dbh.er2,     na.rm = TRUE)),
            rmse.dbh.inc = sqrt(mean(dbh.inc.er2, na.rm = TRUE))) %>%
  ungroup()

rmse.by.plot.dbh <- rmse.by.plot %>%
  select(-rmse.dbh.inc) %>%
  mutate(plot = factor(plot,
                       levels = paste0("A", 2:4),
                       labels = paste0("rmse.dbh.A", 2:4))) %>%
  spread(key = "plot", value = "rmse.dbh")

rmse.by.plot.dbh.inc <- rmse.by.plot %>%
  select(-rmse.dbh) %>%
  mutate(plot = factor(plot,
                       levels = paste0("A", 2:4),
                       labels = paste0("rmse.dbh.inc.A", 2:4))) %>%
  spread(key = "plot", value = "rmse.dbh.inc")

rmse.plot.mean <- rmse.by.plot %>%
  group_by(id) %>%
  summarize(rmse.dbh.p     = mean(rmse.dbh),
            rmse.dbh.inc.p = mean(rmse.dbh.inc))

rmse.together <- dbh.data %>%
  group_by(id) %>%
  summarize(rmse.dbh.t     = sqrt(mean(dbh.er2,     na.rm = TRUE)),
            rmse.dbh.inc.t = sqrt(mean(dbh.inc.er2, na.rm = TRUE)))

lhs.data <- rmse.plot.mean %>%
  left_join(rmse.together,    by = "id") %>%
  left_join(rmse.by.plot.dbh, by = "id") %>%
  left_join(rmse.by.plot.dbh.inc, by = "id") %>%
  left_join(calib.sample,     by = "id")

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
names.to.plot <- names(lhs.data)[which(names(lhs.data) == "rmse.dbh.A2"):which(names(lhs.data) == tail(PARAMS$param.name, 1))]
for(i in names.to.plot) {
  solution.space <- ggplot(lhs.data, aes_string(x     = "rmse.dbh",
                                                y     = "rmse.dbh.inc",
                                                size  = "pareto",
                                                color = "pareto",
                                                fill  = i)) +
    labs(x = "DBH RMSE",
         y = "DBH increment RMSE") +
    geom_point(shape = 21) +
    scale_color_manual(values = c("transparent", "green")) +
    scale_size_manual(values  = c(1, 3)) +
    viridis::scale_fill_viridis(option = "magma") +
    guides(color = FALSE, size = FALSE) +
    rmse.common
  ggsave_fitmax(paste0(lhs.output.path, "LHS_Solution_Space_", i, ".png"), solution.space)
}

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
  select(-(rmse.dbh:rmse.dbh.inc.A4))

# radar.data <- calib.sample %>%
#   filter(id %in% non.aborted.ids) %>%
#   scale_from_ranges() %>%
#   rename(group = id)

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

##### DBH TIMESERIES PLOTS #####
## ALL
plot.annotation <- data.frame(plot = c("A2", "A3", "A4"))
plot.annotation$year <- min(measured.trees$year, na.rm = TRUE)
plot.annotation$measured.dbh <- max(measured.trees$measured.dbh, na.rm = TRUE)

all.dbh.ts.plot <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)") +
  facet_wrap(~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  #scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(data = modeled.data.all, aes(y = modeled.dbh, color = id, group = id, size = aborted), size = 0.75) +
  geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
  scale_size_manual(values = c(0.25, 0.5)) +
  guides(color = FALSE) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries_ALL_LHS_Simulations.png"), all.dbh.ts.plot)

## INTERESTING
A2.best <- modeled.data.all %>%
  filter(id == lhs.data$id[which(lhs.data$rmse.dbh.A2 == min(lhs.data$rmse.dbh.A2, na.rm = TRUE))]) %>%
  mutate(sim = "A2 best")
A3.best <- modeled.data.all %>%
  filter(id == lhs.data$id[which(lhs.data$rmse.dbh.A3 == min(lhs.data$rmse.dbh.A3, na.rm = TRUE))]) %>%
  mutate(sim = "A3 best")
A4.best <- modeled.data.all %>%
  filter(id == lhs.data$id[which(lhs.data$rmse.dbh.A4 == min(lhs.data$rmse.dbh.A4, na.rm = TRUE))]) %>%
  mutate(sim  = "A4 best")
pareto.sims <- modeled.data.all %>%
  filter(id %in% pareto.ids) %>%
  mutate(sim = as.character(id))
best.data <- bind_rows(A2.best, A3.best, A4.best, pareto.sims)

best.dbh.ts.plot <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)",
       color   = NULL) +
  facet_wrap(~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  #scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(data = best.data, aes(y = modeled.dbh, color = sim), size = 0.75) +
  geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries.png"), best.dbh.ts.plot)

##### GENERATE FULL CALIBRATION SIMULATION & SAVE TOP SIMS TO INCLUDE NEXT TIME #####
winner.common.params <- calib.sample %>%
  filter(id == pareto.ids[3]) %>%
  select(-id)

top.sims <- lhs.data %>%
  filter(pareto == TRUE | rmse.dbh %in% head(sort(lhs.data$rmse.dbh), 5) | rmse.dbh.inc %in% head(sort(lhs.data$rmse.dbh.inc), 5))
top.sims <- top.sims[, names(top.sims) %in% PARAMS$param.name]
write_csv(top.sims, paste0("./output/LHS_Top_Sims.csv"), append = TRUE)
