### hisafe-calibration
### CALIBRATION
### Author: Kevin J. Wolz

REGENERATE.LHS <- FALSE
N.SIMUS    <- 10000
BATCH.SIZE <- 1000
MAX.RANK <- 10

library(hisafer)
library(tidyverse)
library(ggradar)
library(lhs)
library(scales)
library(rPref)
library(lubridate)
library(rpart)

cbPalette  <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

input.path      <- "./raw_data/"
simulation.path <- "./simulations/"
lhs.output.path <- "./output/LHS/"
dir.create(lhs.output.path, showWarnings = FALSE, recursive = TRUE)

PARAMS <- read_csv(paste0(input.path, "hisafe_LHS_parameters.csv"), col_types = cols()) %>%
  filter(calibrate == TRUE) %>%
  select(-fixed, -calibrate)

fixed.params <- read_csv(paste0(input.path, "hisafe_LHS_parameters.csv"), col_types = cols()) %>%
  filter(calibrate == FALSE) %>%
  select(param.name, fixed) %>%
  spread(key = "param.name", value = "fixed") %>%
  as.list()

N.PARAMS <- nrow(PARAMS)

round_vals <- function(df) {
  for(i in names(df)) {
    sig <- PARAMS$sig.level[PARAMS$param.name == i]
    df[i] <- round(df[i] / sig) * sig
  }
  return(df)
}

reldiff <- function(x) diff(x) / x[-length(x)]

PATH       <- paste0(simulation.path, "LHS")
PROFILES   <- "annualDBH"
FILES      <- c("sim", "tree")
A2.WEATHER <- "./raw_data/restinclieres_A2-1994-2018.wth"
A3.WEATHER <- "./raw_data/restinclieres_A3-1994-2018.wth"
A4.WEATHER <- "./raw_data/restinclieres_A4-1994-2018.wth"

##### GENERATE LATIN HYPRECUBE SAMPLE SET #####
# old.top.sims <- read_csv(paste0("./output/LHS_Top_Sims.csv"), col_types = cols()) %>%
#   mutate(id = (N.SIMUS + 1):(N.SIMUS + nrow(.)))

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
    mutate(batch = ceiling(id / BATCH.SIZE)) %>%
    mutate(id = id - BATCH.SIZE * (batch - 1)) %>%
    mutate(id = paste0(batch, "-", id)) %>%
    select(batch, id, everything()) #%>%
  #bind_rows(old.top.sims)
  write_csv(calib.sample, paste0(lhs.output.path, "hisafe_calibration_LHS.csv"))
} else {
  calib.sample <- read_csv(paste0(lhs.output.path, "hisafe_calibration_LHS.csv"), col_types = cols()) #%>%
  #bind_rows(old.top.sims)
}

##### BULID DEFAULT FOLDERS #####
A3.template <- define_hisafe(path     = paste0(simulation.path, "LHS_default_folders"),
                             profiles = "all-private",
                             template = "restinclieres_agroforestry_A3",
                             SimulationName = "A3",
                             weatherFile = A3.WEATHER)
build_hisafe(hip           = A3.template,
             files         = c("tec", "plt", "pro", "par", "pld", "wth"),
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = paste0(input.path, "A3.rootcal"),
                 to   = paste0(simulation.path, "LHS_default_folders/A3/A3.rootcal"))
dum <- file.copy(from = paste0(input.path, "A3.dbhcal"),
                 to   = paste0(simulation.path, "LHS_default_folders/A3/A3.dbhcal"))


A2.template <- define_hisafe(path     = paste0(simulation.path, "LHS_default_folders"),
                             profiles = "all-private",
                             template = "restinclieres_agroforestry_A2",
                             SimulationName = "A2",
                             weatherFile = A2.WEATHER)
build_hisafe(hip           = A2.template,
             files         = c("tec", "plt", "pro", "par", "pld", "wth"),
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = paste0(input.path, "A2.dbhcal"),
                 to   = paste0(simulation.path, "LHS_default_folders/A2/A2.dbhcal"))


A4.template <- define_hisafe(path     = paste0(simulation.path, "LHS_default_folders"),
                             profiles = "all-private",
                             template = "restinclieres_forestry_A4",
                             SimulationName = "A4",
                             weatherFile = A4.WEATHER)
build_hisafe(hip           = A4.template,
             files         = c("tec", "plt", "pro", "par", "pld", "wth"),
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = paste0(input.path, "A4.dbhcal"),
                 to   = paste0(simulation.path, "LHS_default_folders/A4/A4.dbhcal"))

##### BUILD A3 SIMUILATIONS #####
for(i in unique(calib.sample$batch)) {
  print(paste("Defining batch", i, "of 10"))

  batch.ids <- calib.sample %>%
    #filter(id == "5-594") %>%
    filter(batch == i) %>%
    .$id

  common.params <- calib.sample %>%
    #filter(id == "5-594") %>%
    filter(batch == i) %>%
    select(-batch, -id) %>%
    as.list()

  A3.hip <- define_hisafe(path           = paste0(PATH, "/LHS_A3"),
                          profiles       = PROFILES,
                          template       = "restinclieres_agroforestry_A3",
                          SimulationName = paste0("LHS_A3_", batch.ids),
                          bulk.pass      = c(common.params, fixed.params),
                          exp.name       = paste0("LHS_A3_", i),
                          weatherFile    = A3.WEATHER)

  print(paste("Building batch", i, "of 10"))
  build_hisafe(hip           = A3.hip,
               files         = FILES,
               plot.scene    = FALSE,
               summary.files = FALSE)

  build_cluster_script(simu.names     = paste0("LHS_A3_", batch.ids),
                       simu.prefix    = paste0("LHS_A3_", i, "-"),
                       launch.call    = "ScriptCalib",
                       default.folder = "A3",
                       cluster.path   = paste0("/lustre/lecomtei/LHS/LHS_A3/LHS_A3_", i),
                       script.path    = paste0(PATH, "/LHS_A3"),
                       script.name    = paste0("LHS_A3_Launch_", i),
                       email.type     = "ALL",
                       email          = "wolzkevin@gmail.com")
}

# ## BEFORE
# for(i in 4001:8000) {
#   file.rename(paste0(PATH, "/LHS_A3/LHS_A3_", i), paste0(PATH, "/LHS_A3/LHS_A3_2_", i - 4000))
# }
# for(i in 4001:8000) {
#   file.rename(paste0(PATH, "/LHS_A3/LHS_A3_2_", i - 4000, "/LHS_A3_", i, ".sim"),
#               paste0(PATH, "/LHS_A3/LHS_A3_2_", i - 4000, "/LHS_A3_", i-4000, ".sim"))
# }
# for(i in 8001:10000) {
#   file.rename(paste0(PATH, "/LHS_A3/LHS_A3_", i), paste0(PATH, "/LHS_A3/LHS_A3_3_", i - 8000))
# }
# for(i in 8001:10000) {
#   file.rename(paste0(PATH, "/LHS_A3/LHS_A3_3_", i - 8000, "/LHS_A3_", i, ".sim"),
#               paste0(PATH, "/LHS_A3/LHS_A3_3_", i - 8000, "/LHS_A3_3_", i-8000, ".sim"))
# }
#
#
# ## AFTER
# for(i in 4001:8000) {
#   file.rename(paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_2_", i - 4000),
#               paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i))
# }
# for(i in 4001:8000) {
#   file.rename(paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_2_", i-4000),
#               paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_", i))
# }
# for(i in 4001:8000) {
#   file.rename(paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_", i, "/LHS_A3_2_", i-4000, "_annualDBH.txt"),
#               paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_", i, "/LHS_A3_", i, "_annualDBH.txt"))
# }
#
#
# for(i in 8001:10000) {
#   file.rename(paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_3_", i - 8000),
#               paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i))
# }
# for(i in 8001:10000) {
#   file.rename(paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_3_", i-8000),
#               paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_", i))
# }
# for(i in 8001:10000) {
#   file.rename(paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_", i, "/LHS_A3_3_", i-8000, "_annualDBH.txt"),
#               paste0(PATH, "/LHS_A3/COMPLETE/LHS_A3_", i, "/output-LHS_A3_", i, "/LHS_A3_", i, "_annualDBH.txt"))
# }
#
#
# to.delete <- list.files(paste0(PATH, "/LHS_A3/COMPLETE"), pattern = "*\\.sim", recursive = TRUE, full.names = TRUE)
# file.remove(to.delete)
# to.delete <- list.files(paste0(PATH, "/LHS_A3/COMPLETE"), pattern = "*\\.tree", recursive = TRUE, full.names = TRUE)
# file.remove(to.delete)
# to.delete <- list.files(paste0(PATH, "/LHS_A3/COMPLETE"), pattern = "treeSpecies", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
# file.remove(to.delete)

files <- list.files(paste0(PATH, "/LHS_A3/COMPLETE")) %>%
  str_remove("LHS_A3_") %>%
  as.numeric() %>%
  sort()

length(files)
missing <- (1:N.SIMUS)[!(1:N.SIMUS %in% files)]

##### DETERMINE WHICH A3 SIMULATIONS WERE ABORTED AND WHY #####
root.abort.files <- list.files(paste0(PATH, "/LHS_A3"), pattern = "root\\.abord", full.names = TRUE, recursive = TRUE)
dbh.abort.files  <- list.files(paste0(PATH, "/LHS_A3"), pattern = "dbh\\.abord",  full.names = TRUE, recursive = TRUE)
root.abort <- purrr::map_df(root.abort.files,
                            read_delim,
                            delim     = ";",
                            col_names = c("SimulationName", "year", "month", "day", "idTree", "dist.4m", "dist.6m", "full.scene", "value"),
                            col_types = cols()) %>%
  mutate(abort = "root")
dbh.abort <- purrr::map_df(dbh.abort.files,
                           read_delim,
                           delim     = ";",
                           col_names = c("SimulationName", "year", "month", "day", "idTree", "dbh.min", "dbh.max", "value"),
                           col_types = cols()) %>%
  mutate(abort = "dbh")

abort <- root.abort %>%
  bind_rows(dbh.abort) %>%
  select(SimulationName, year, month, day, abort, value) %>%
  mutate(event   = paste(year, month, abort, sep = "-")) %>%
  mutate(id      = str_remove(SimulationName, "LHS_A3_")) %>%
  left_join(calib.sample, by = "id")

# batch.split <- function(x) {
#   out <- str_split(x, "_")
#   for(i in 1:length(out)) {
#     if(length(out[[i]]) == 1) out[[i]] <- c(1, out[[i]])
#   }
#   out <- purrr::map_chr(out, 1)
#   return(out)
# }
# abort <- abort %>%
#   mutate(batch = batch.split(id)) %>%
#   mutate(id    = purrr::map_chr(str_split(id, "_"), tail, n = 1)) %>%
#   mutate(batch = as.numeric(batch)) %>%
#   mutate(id = as.numeric(id)) %>%
#   mutate(id    = id + 4000 * (batch - 1))
#
# aborted.ids     <- abort$id
# non.aborted.ids <- sort((1:10000)[!((1:10000) %in% aborted.ids)])

aborted.ids     <- abort$id
non.aborted.ids <- sort(calib.sample$id[!(calib.sample$id %in% aborted.ids)])

## FAILURE DIAGNOSTICS
# filter(calib.sample, id %in% non.aborted.ids)
# filter(abort, event == "2007-11") %>% select(-(SimulationName:full.scene)) #46
# filter(abort, event == "2004-8")  %>% select(-(SimulationName:full.scene)) #4
# filter(abort, event == "2002-4")  %>% select(-(SimulationName:full.scene)) #4
# ggplot(filter(abort, event == "2002-4"),
#        aes(x = colonisationThreshold)) +
#   geom_histogram() +
#   facet_wrap(~dist)

initial.summary <- tibble(event = "initial", remaining.simulations = nrow(calib.sample), failed.simulations = 0)
abort.summary <- abort %>%
  group_by(event) %>%
  summarize(failed.simulations = n()) %>%
  mutate(remaining.simulations = -failed.simulations) %>%
  bind_rows(initial.summary, .) %>%
  mutate(remaining.simulations = cumsum(remaining.simulations))

print(as.data.frame(abort.summary), row.names = FALSE)
write_csv(abort.summary, paste0(lhs.output.path, "/A3_Abort_Summary.csv"))

##### BUILD A2 & A4 SIMULATIONS #####
non.aborted <- calib.sample %>%
  filter(id %in% non.aborted.ids) %>%
  mutate(seq.id = 1:nrow(.)) %>%
  select(batch, id, seq.id, everything())

non.aborted.params <- non.aborted %>%
  select(-batch, -id, -seq.id) %>%
  as.list()

A2.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_agroforestry_A2",
                        SimulationName = paste0("LHS_A2_", non.aborted$seq.id),
                        bulk.pass      = c(non.aborted.params, fixed.params),
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
                     script.path    = paste0(PATH, "/LHS_A2"),
                     script.name    = "LHS_A2_Launch",
                     email.type     = "ALL",
                     email          = "wolzkevin@gmail.com")

A4.hip <- define_hisafe(path           = PATH,
                        profiles       = PROFILES,
                        template       = "restinclieres_forestry_A4",
                        SimulationName = paste0("LHS_A4_", non.aborted$seq.id),
                        bulk.pass      = c(non.aborted.params, fixed.params),
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
                     script.path    = paste0(PATH, "/LHS_A4"),
                     script.name    = "LHS_A4_Launch",
                     email.type     = "ALL",
                     email          = "wolzkevin@gmail.com")

##### RENAME A2 & A4 Simulations to match actual ids #####
id.lookup.table <- non.aborted %>%
  select(batch, id, seq.id) #%>%
#mutate(bad.id = non.aborted.ids)

for(p in c(2,4)) { #c(2,4)
  for(iterator in 1:nrow(id.lookup.table)) {
    i <- id.lookup.table$seq.id[iterator]
    j <- id.lookup.table$id[iterator]

    file.rename(paste0(PATH, "/LHS_A", p, "/LHS_A", p, "_", i),
                paste0(PATH, "/LHS_A", p, "/TRUE_LHS_A", p, "_", j))
  }

  for(iterator in 1:nrow(id.lookup.table)) {
    i <- id.lookup.table$seq.id[iterator]
    j <- id.lookup.table$id[iterator]

    file.rename(paste0(PATH, "/LHS_A", p, "/TRUE_LHS_A", p, "_", j),
                paste0(PATH, "/LHS_A", p, "/LHS_A", p, "_", j))

    file.rename(paste0(PATH, "/LHS_A", p, "/LHS_A", p, "_", j, "/output-LHS_A", p, "_", i),
                paste0(PATH, "/LHS_A", p, "/LHS_A", p, "_", j, "/output-LHS_A", p, "_", j))

    file.rename(paste0(PATH, "/LHS_A", p, "/LHS_A", p, "_", j, "/output-LHS_A", p, "_", j, "/LHS_A", p, "_", i, "_annualDBH.txt"),
                paste0(PATH, "/LHS_A", p, "/LHS_A", p, "_", j, "/output-LHS_A", p, "_", j, "/LHS_A", p, "_", j, "_annualDBH.txt"))
  }
}

##### READ ALL SIMULATIONS #####
## rename all output files to trees.txt
output.files <- list.files(PATH, pattern = "annualDBH\\.txt", full.names = TRUE, recursive = TRUE)
new.names    <- str_replace(output.files, "annualDBH\\.txt$", "trees.txt")
dum <- file.rename(output.files, new.names)

## read simulations that did not abort
read_A3 <- function(i) {
  out <- read_hisafe(path          = paste0(PATH, "/LHS_A3/LHS_A3_", i),
                     simu.names    = paste0("LHS_A3_", i, "-", 1:BATCH.SIZE),
                     profiles      = "trees",
                     show.progress = FALSE,
                     read.inputs   = FALSE)$trees
  out}
A3.trees <- purrr::map_df(unique(calib.sample$batch), read_A3)

A2.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A2"),
                        simu.names    = paste0("LHS_A2_", id.lookup.table$id),
                        profiles      = "trees",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$trees
A4.trees <- read_hisafe(path          = paste0(PATH, "/LHS_A4"),
                        simu.names    = paste0("LHS_A4_", id.lookup.table$id),
                        profiles      = "trees",
                        show.progress = FALSE,
                        read.inputs   = FALSE)$trees

modeled.data.all <- bind_rows(A2.trees, A3.trees, A4.trees) %>%
  filter(idTree == 1) %>% # remove second tree
  mutate(id   = str_remove(SimulationName, "LHS_A[2-4]_")) %>%
  mutate(plot = purrr::map_chr(str_split(SimulationName, "_"), 2)) %>%
  mutate(year = Year - 1) %>%
  rename(modeled.dbh = dbh) %>%
  group_by(plot, id) %>%
  # mutate(modeled.dbh.inc = c(NA, diff(modeled.dbh))) %>%    # ABSOLUTE DIFFERENCES
  mutate(modeled.dbh.inc = c(NA, reldiff(modeled.dbh))) %>% # RELATIVE DIFFERENCES
  ungroup() %>%
  mutate(aborted = id %in% aborted.ids) %>%
  select(plot, id, year, modeled.dbh, modeled.dbh.inc, aborted)

## Sometimes simulations fail on the cluster for no apparent reason!
failed <- modeled.data.all %>%
  filter(aborted == FALSE) %>%
  group_by(plot, id) %>%
  summarize(n = n()) %>%
  filter(n < 24) %>%
  .$id

failed.at.start <- modeled.data.all %>%
  filter(aborted == FALSE) %>%
  filter(!(id %in% failed)) %>%
  group_by(id) %>%
  summarize(n = n()) %>%
  filter(n < 24 * 3) %>%
  .$id

failed <- c(failed, failed.at.start)

modeled.data <- modeled.data.all %>%
  filter(aborted == FALSE) %>%
  filter(!(id %in% failed)) %>%
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
  left_join(rmse.together,        by = "id") %>%
  left_join(rmse.by.plot.dbh,     by = "id") %>%
  left_join(rmse.by.plot.dbh.inc, by = "id") %>%
  left_join(calib.sample,         by = "id")

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
  filter(!is.na(rmse.dbh) & !is.na(rmse.dbh.inc)) %>%
  arrange(rmse.dbh) %>%
  .$id

plot.pareto.ids <- psel(lhs.data, low(rmse.dbh.A2) * low(rmse.dbh.A3) * low(rmse.dbh.A4)) %>%
  filter(!is.na(rmse.dbh.A2) & !is.na(rmse.dbh.A3) & !is.na(rmse.dbh.A4)) %>%
  arrange(rmse.dbh) %>%
  .$id

top <- lhs.data %>%
  arrange(rmse.dbh) %>%
  mutate(rank = 1:nrow(.)) %>%
  select(id, rank)

top.ids <- top$id[1:MAX.RANK]

lhs.data <- lhs.data %>%
  mutate(pareto = id %in% pareto.ids) %>%
  mutate(plot.pareto = id %in% plot.pareto.ids) %>%
  left_join(top, by = "id") %>%
  select(id, rank, pareto, plot.pareto, everything(), -batch) %>%
  arrange(rank)

pareto.data <- lhs.data %>%
  filter(pareto)

plot.pareto.data <- lhs.data %>%
  filter(plot.pareto)

##### Visualize solution space #####
names.to.plot <- names(lhs.data)[which(names(lhs.data) == "rmse.dbh.A2"):which(names(lhs.data) == tail(PARAMS$param.name, 1))]
for(i in names.to.plot) {
  solution.space <- ggplot(lhs.data, aes_string(x     = "rmse.dbh",
                                                y     = "rmse.dbh.inc",
                                                size  = "pareto",
                                                color = "pareto",
                                                fill  = i)) +
    labs(x = "DBH RMSE",
         y = "DBH relative increment RMSE") +
    geom_point(shape = 21) +
    geom_point(data = pareto.data, shape = 21, size = 3) + #, aes(color = id)
    scale_color_manual(values = c("transparent", "green")) + #cbPalette) + #
    scale_size_manual(values  = c(1, 3)) +
    viridis::scale_fill_viridis(option = "magma") +
    guides(size = FALSE, color = FALSE) +
    rmse.common
  ggsave_fitmax(paste0(lhs.output.path, "LHS_Solution_Space_", i, ".png"), solution.space, scale = 1.5)

  # rank.space <- ggplot(lhs.data, aes_string(x     = "rank",
  #                                           y     = i)) +
  #   labs(x = "Solution rank",
  #        y = i) +
  #   geom_line() +
  #   geom_point(data = pareto.data, shape = 21, size = 3, color = "green") +
  #   rmse.common
  # ggsave_fitmax(paste0(lhs.output.path, "LHS_Rank_Space_", i, ".png"), rank.space, scale = 1.5)
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

top.radar.data <- lhs.data %>%
  filter(id %in% top.ids) %>%
  select(-plot.pareto, -pareto, -rank) %>%
  scale_from_ranges() %>%
  rename(group = id) %>%
  select(-(rmse.dbh:rmse.dbh.inc.A4))

top.radar.plot <- ggradar(top.radar.data,
                          gridline.mid.colour = "grey",
                          group.point.size    = 2,
                          group.line.width    = 1,
                          grid.label.size     = 4,
                          axis.label.size     = 1.5,
                          axis.label.offset   = 1.1,
                          legend.title        = "Simulation ID",
                          legend.text.size    = 10) +
  scale_color_manual(values = rep(cbPalette, 10))
ggsave_fitmax(paste0(lhs.output.path, "Pareto_Radar_Plot-TOP_DBH.png"), top.radar.plot)

pareto.radar.data <- lhs.data %>%
  filter(pareto) %>%
  select(-plot.pareto, -pareto, -rank) %>%
  scale_from_ranges() %>%
  rename(group = id) %>%
  select(-(rmse.dbh:rmse.dbh.inc.A4))

pareto.radar.plot <- ggradar(pareto.radar.data,
                             gridline.mid.colour = "grey",
                             group.point.size    = 2,
                             group.line.width    = 1,
                             grid.label.size     = 4,
                             axis.label.size     = 1.5,
                             axis.label.offset   = 1.1,
                             legend.title        = "Simulation ID",
                             legend.text.size    = 10) +
  scale_color_manual(values = rep(cbPalette, 10))
ggsave_fitmax(paste0(lhs.output.path, "Pareto_Radar_Plot-PARETO.png"), pareto.radar.plot)

plot.pareto.radar.data <- lhs.data %>%
  filter(plot.pareto) %>%
  select(-plot.pareto, -pareto, -rank) %>%
  scale_from_ranges() %>%
  rename(group = id) %>%
  select(-(rmse.dbh:rmse.dbh.inc.A4))

plot.pareto.radar.plot <- ggradar(plot.pareto.radar.data,
                                  gridline.mid.colour = "grey",
                                  group.point.size    = 2,
                                  group.line.width    = 1,
                                  grid.label.size     = 4,
                                  axis.label.size     = 1.5,
                                  axis.label.offset   = 1.1,
                                  legend.title        = "Simulation ID",
                                  legend.text.size    = 10) +
  scale_color_manual(values = rep(cbPalette, 10))
ggsave_fitmax(paste0(lhs.output.path, "Pareto_Radar_Plot-PLOT_PARETO.png"), plot.pareto.radar.plot)

##### VISUALIZE PARAMETER SPACE #####
param.space.data <- lhs.data %>%
  filter(rmse.dbh < 2.5 | rmse.dbh.inc < 0.7) %>%
  select_at(PARAMS$param.name) %>%
  gather(key = "param.name", value = "value")

ggplot(param.space.data, aes(y = value)) +
  facet_wrap(~param.name, scales = "free") +
  geom_boxplot() +
  geom_hline(data = PARAMS, aes(yintercept = param.max), color = "red") +
  geom_hline(data = PARAMS, aes(yintercept = param.min), color = "red")

##### CART #####
mytree <- rpart(rmse.dbh ~  lueMax +
                  lueWaterStressResponsiveness +
                  lueNitrogenStressResponsiveness +
                  transpirationCoefficient +
                  maxTargetLfrRatio +
                  initialTargetLfrRatio +
                  maxTargetLfrRatioDailyVariation +
                  targetLfrRatioUpperDrift +
                  rsWaterStressResponsiveness +
                  rsNitrogenStressResponsiveness +
                  rsNoStressResponsiveness +
                  targetNCoefficient +
                  cRAreaToFRLengthRatio +
                  coarseRootAnoxiaResistance +
                  fineRootAnoxiaLifespan +
                  colonisationThreshold +
                  horizontalPreference +
                  localWaterUptakeFactor +
                  localNitrogenUptakeFactor +
                  sinkDistanceEffect +
                  treeMinTranspirationPotential +
                  treeMaxTranspirationPotential,
                data = lhs.data)

mytree <- rpart(penalty ~  lueMax +
                  lueWaterStressResponsiveness +
                  lueNitrogenStressResponsiveness +
                  transpirationCoefficient +
                  maxTargetLfrRatio +
                  initialTargetLfrRatio +
                  maxTargetLfrRatioDailyVariation +
                  targetLfrRatioUpperDrift +
                  rsWaterStressResponsiveness +
                  rsNitrogenStressResponsiveness +
                  rsNoStressResponsiveness +
                  targetNCoefficient +
                  cRAreaToFRLengthRatio +
                  coarseRootAnoxiaResistance +
                  fineRootAnoxiaLifespan +
                  colonisationThreshold +
                  horizontalPreference +
                  localWaterUptakeFactor +
                  localNitrogenUptakeFactor +
                  sinkDistanceEffect +
                  treeMinTranspirationPotential +
                  treeMaxTranspirationPotential,
                data = abort)

printcp(mytree) # display the results
plotcp(mytree) # visualize cross-validation results
summary(mytree) # detailed summary of splits

# plot tree
plot(mytree, uniform=TRUE,
     main="Classification Tree for LHS")
text(mytree, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(mytree, file = "./tree.ps",
     title = "Classification Tree for LHS")

# prune the tree
pfit <- prune(mytree, cp=   mytree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for LHS")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "./ptree.ps",
     title = "Pruned Classification Tree for LHS")

##### LM #####
mylm <- lm(rmse.dbh ~  lueMax +
             lueWaterStressResponsiveness +
             lueNitrogenStressResponsiveness +
             transpirationCoefficient +
             maxTargetLfrRatio +
             initialTargetLfrRatio +
             maxTargetLfrRatioDailyVariation +
             targetLfrRatioUpperDrift +
             rsWaterStressResponsiveness +
             rsNitrogenStressResponsiveness +
             rsNoStressResponsiveness +
             targetNCoefficient +
             cRAreaToFRLengthRatio +
             coarseRootAnoxiaResistance +
             fineRootAnoxiaLifespan +
             colonisationThreshold +
             horizontalPreference +
             localWaterUptakeFactor +
             localNitrogenUptakeFactor +
             sinkDistanceEffect +
             treeMinTranspirationPotential +
             treeMaxTranspirationPotential,
           data = lhs.data)

# mylm <- lm(rmse.dbh ~  lueWaterStressResponsiveness *
#              rsWaterStressResponsiveness *
#              coarseRootAnoxiaResistance *
#              fineRootAnoxiaLifespan *
#              localWaterUptakeFactor,
#            data = lhs.data)

# mylm <- lm(rmse.dbh ~  lueMax *
#              initialTargetLfrRatio *
#              maxTargetLfrRatioDailyVariation *
#              targetLfrRatioUpperDrift *
#              maxTargetLfrRatio,
#            data = lhs.data)
#
# mylm <- lm(rmse.dbh ~  lueNitrogenStressResponsiveness *
#              rsNitrogenStressResponsiveness *
#              targetNCoefficient *
#              localNitrogenUptakeFactor *
#              lueMax,
#            data = lhs.data)

mylm <- lm(rmse.dbh ~  lueMax +
             lueWaterStressResponsiveness +
             #lueNitrogenStressResponsiveness *
             transpirationCoefficient +
             maxTargetLfrRatio +
             initialTargetLfrRatio +
             #maxTargetLfrRatioDailyVariation *
             #targetNCoefficient *
             cRAreaToFRLengthRatio +
             colonisationThreshold,
           data = lhs.data)

summary(mylm)

##### PCA #####
pca.data <- lhs.data %>%
  #filter(rmse.dbh < 3) %>%
  select(lueMax:treeMaxTranspirationPotential) %>%
  as.data.frame()
PCA <- prcomp(~., data = pca.data, retx = T, center = T, scale. = T)
summary(PCA)

##### DBH TIMESERIES PLOTS #####
## ALL
plot.annotation <- data.frame(plot = c("A2", "A3", "A4"))
plot.annotation$year <- min(measured.trees$year, na.rm = TRUE)
plot.annotation$measured.dbh <- max(measured.trees$measured.dbh, na.rm = TRUE)

# ids.to.plot <- lhs.data %>%
#   filter(horizontalPreference < 0.3) %>%
#   filter(lueWaterStressResponsiveness > 4) %>%
#   filter(rsWaterStressResponsiveness > 2.5) %>%
#   .$id
modeled.data.all.plot <- modeled.data.all# %>%
#filter(id %in% ids.to.plot)

all.dbh.ts.plot <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)") +
  facet_wrap(~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  #scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(data = modeled.data.all.plot, aes(y = modeled.dbh, color = id, group = id, size = aborted), size = 0.75) +
  geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
  scale_size_manual(values = c(0.25, 0.5)) +
  guides(color = FALSE) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries_ALL_LHS_Simulations.png"), all.dbh.ts.plot)

## EXTRACT INTERESTING SIMULATIONS
A2.best.id <- lhs.data$id[which(lhs.data$rmse.dbh.A2 == min(lhs.data$rmse.dbh.A2, na.rm = TRUE))]
A3.best.id <- lhs.data$id[which(lhs.data$rmse.dbh.A3 == min(lhs.data$rmse.dbh.A3, na.rm = TRUE))]
A4.best.id <- lhs.data$id[which(lhs.data$rmse.dbh.A4 == min(lhs.data$rmse.dbh.A4, na.rm = TRUE))]

top.ids.to.extract <- c(top.ids, A2.best.id, A3.best.id, A4.best.id)
top.id.labels      <- c(top.ids, "A2 best", "A3 best", "A4 best")
top.data <- modeled.data %>%
  filter(id %in% top.ids.to.extract) %>%
  mutate(id = factor(id, levels = top.ids.to.extract, labels = top.id.labels)) %>%
  left_join(measured.annual, by = c("plot", "year")) %>%
  mutate(sim = as.character(id))

pareto.ids.to.extract <- c(pareto.ids)#, A2.best.id, A3.best.id, A4.best.id)
pareto.id.labels      <- c(pareto.ids)#, "A2 best", "A3 best", "A4 best")
pareto.data <- modeled.data %>%
  filter(id %in% pareto.ids.to.extract) %>%
  mutate(id = factor(id, levels = pareto.ids.to.extract, labels = pareto.id.labels)) %>%
  left_join(measured.annual, by = c("plot", "year")) %>%
  mutate(sim = as.character(id))

plot.pareto.ids.to.extract <- c(plot.pareto.ids)#, A2.best.id, A3.best.id, A4.best.id)
plot.pareto.id.labels      <- c(plot.pareto.ids)#, "A2 best", "A3 best", "A4 best")
plot.pareto.data <- modeled.data %>%
  filter(id %in% plot.pareto.ids.to.extract) %>%
  mutate(id = factor(id, levels = plot.pareto.ids.to.extract, labels = plot.pareto.id.labels)) %>%
  left_join(measured.annual, by = c("plot", "year")) %>%
  mutate(sim = as.character(id))

##### PLOTS OF TOP SIMULATIONS #####
top.dbh.ts.plot <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)",
       color   = NULL) +
  facet_wrap(~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(data = top.data, aes(y = modeled.dbh, color = sim), size = 0.75) +
  geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries-TOP_DBH.png"), top.dbh.ts.plot, scale = 2)

top.dbh.ts.plot.grid <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)",
       color   = NULL) +
  facet_grid(sim~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  geom_line(data = top.data, aes(y = modeled.dbh), size = 0.75) +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries_Indiv-TOP_DBH.png"), top.dbh.ts.plot.grid, scale = 2)

top.mvm.inc.plot <- ggplot(top.data, aes(x = modeled.dbh.inc, y = measured.dbh.inc, fill = plot)) +
  labs(x    = "Modeled DBH increment (cm)",
       y    = "Measured DBH increment (cm)",
       fill = NULL) +
  facet_wrap(~sim) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "grey70", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_inc_scatterplot-TOP_DBH.png"), top.mvm.inc.plot, scale = 2)

top.mvm.inc.plot <- ggplot(top.data, aes(x = modeled.dbh, y = measured.dbh, fill = plot)) +
  labs(x    = "Modeled DBH (cm)",
       y    = "Measured DBH (cm)",
       fill = NULL) +
  facet_wrap(~sim) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "grey70", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_scatterplot-TOP_DBH.png"), top.mvm.inc.plot, scale = 2)

##### PLOTS OF PARETO SIMULATIONS #####
pareto.dbh.ts.plot <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)",
       color   = NULL) +
  facet_wrap(~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(data = pareto.data, aes(y = modeled.dbh, color = sim), size = 0.75) +
  geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries-PARETO.png"), pareto.dbh.ts.plot, scale = 2)

pareto.dbh.ts.plot.grid <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)",
       color   = NULL) +
  facet_grid(sim~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  geom_line(data = pareto.data, aes(y = modeled.dbh), size = 0.75) +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries_Indiv-PARETO.png"), pareto.dbh.ts.plot.grid, scale = 2)

pareto.mvm.inc.plot <- ggplot(pareto.data, aes(x = modeled.dbh.inc, y = measured.dbh.inc, fill = plot)) +
  labs(x    = "Modeled DBH increment (cm)",
       y    = "Measured DBH increment (cm)",
       fill = NULL) +
  facet_wrap(~sim) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "grey70", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_inc_scatterplot-PARETO.png"), pareto.mvm.inc.plot, scale = 2)

pareto.mvm.inc.plot <- ggplot(pareto.data, aes(x = modeled.dbh, y = measured.dbh, fill = plot)) +
  labs(x    = "Modeled DBH (cm)",
       y    = "Measured DBH (cm)",
       fill = NULL) +
  facet_wrap(~sim) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "grey70", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_scatterplot-PARETO.png"), pareto.mvm.inc.plot, scale = 2)

##### PLOTS OF PLOT PARETO SIMULATIONS #####
plot.pareto.dbh.ts.plot <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (cm)",
       color   = NULL) +
  facet_wrap(~plot) +
  geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(data = plot.pareto.data, aes(y = modeled.dbh, color = sim), size = 0.75) +
  geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries-PLOT_PARETO.png"), plot.pareto.dbh.ts.plot, scale = 2)

for(i in 1:round(length(plot.pareto.ids)/ 10)) {
  subset.ids <- plot.pareto.ids[(1:10)+(i-1)*10]
  if(i == round(length(plot.pareto.ids)/ 10)) subset.ids <- plot.pareto.ids[(1+(i-1)*10):length(plot.pareto.ids)]
  plot.pareto.data.subset <- plot.pareto.data %>%
    filter(id %in% subset.ids)

  plot.pareto.dbh.ts.plot.grid <- ggplot(measured.trees, aes(x = year, y = measured.dbh)) +
    labs(x       = "Year",
         y       = "DBH (cm)",
         color   = NULL) +
    facet_grid(sim~plot) +
    geom_boxplot(aes(group = year), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
    geom_line(data = plot.pareto.data.subset, aes(y = modeled.dbh), size = 0.75) +
    theme_hisafe_ts(panel.grid = element_blank())
  ggsave_fitmax(paste0(lhs.output.path, "DBH_timeseries_Indiv-PLOT_PARETO_", i, ".png"), plot.pareto.dbh.ts.plot.grid, scale = 2)
}

plot.pareto.mvm.inc.plot <- ggplot(plot.pareto.data, aes(x = modeled.dbh.inc, y = measured.dbh.inc, fill = plot)) +
  labs(x    = "Modeled DBH increment (cm)",
       y    = "Measured DBH increment (cm)",
       fill = NULL) +
  facet_wrap(~sim) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "grey70", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_inc_scatterplot-PLOT_PARETO.png"), plot.pareto.mvm.inc.plot, scale = 2)

plot.pareto.mvm.inc.plot <- ggplot(plot.pareto.data, aes(x = modeled.dbh, y = measured.dbh, fill = plot)) +
  labs(x    = "Modeled DBH (cm)",
       y    = "Measured DBH (cm)",
       fill = NULL) +
  facet_wrap(~sim) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "grey70", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(lhs.output.path, "DBH_scatterplot-PLOT_PARETO.png"), plot.pareto.mvm.inc.plot, scale = 2)

##### GENERATE FULL CALIBRATION SIMULATION & SAVE TOP SIMS TO INCLUDE NEXT TIME #####
winner.common.params <- calib.sample %>%
  filter(id %in% top.ids) %>%
  #filter(id %in% plot.pareto.ids) %>%
  select(-batch, -id) %>%
  as.list() %>%
  c(fixed.params)
write_csv(top.sims, paste0("./output/LHS_Top_Sims.csv"), append = TRUE)

top.sims <- lhs.data %>%
  filter(plot.pareto == TRUE)
top.sims <- top.sims[, names(top.sims) %in% PARAMS$param.name]
write_csv(top.sims, paste0("./output/LHS_Top_Sims.csv"), append = TRUE)




##### FOR GA INITIAL POPULATION #####
MU <- 100

search.data <- lhs.data %>%
  dplyr::select(-rank, -pareto, -plot.pareto, -rmse.dbh, -rmse.dbh.inc, -rmse.dbh.inc.A2, -rmse.dbh.inc.A3, -rmse.dbh.inc.A4)
INITIAL.POP <- dplyr::tibble()
i <- 1

## recurssively pull off the Pareto set until at least 250 solutions are found
while(nrow(INITIAL.POP) < MU) {
  initial.pop <- psel(search.data, low(rmse.dbh.A2) * low(rmse.dbh.A3) * low(rmse.dbh.A4)) %>%
    dplyr::mutate(iter = i)

  INITIAL.POP <- dplyr::bind_rows(INITIAL.POP, initial.pop)

  search.data <- search.data %>%
    dplyr::filter(!(id %in% initial.pop$id))

  i <- i + 1
}

## only keep 250 solutions, discarding the worst of the last Pareto iteration based on their hypervolume
INITIAL.POP <- INITIAL.POP %>%
  dplyr::mutate(volume = rmse.dbh.A2 * rmse.dbh.A3 * rmse.dbh.A4) %>%
  dplyr::arrange(iter, volume) %>%
  .[1:MU,] %>%
  select(iter, id, volume, rmse.dbh.A2, rmse.dbh.A3, rmse.dbh.A4, everything())


## give the first tier Pareto front the actual values for the less sensitive parameters that were used in the LHS
first.tier <- INITIAL.POP %>%
  dplyr::filter(iter == 1)

FIXED <- purrr::map(fixed.params, rep, times = nrow(first.tier)) %>%
  as_tibble()

first.tier <- first.tier %>%
  dplyr::bind_cols(FIXED)

## remaining teirs get the less sensitive parameters from a uniform sampling across their allowed ranges
other.tiers <- INITIAL.POP %>%
  dplyr::filter(iter != 1)

ALL.PARAMS <- read_csv(paste0(input.path, "hisafe_LHS_parameters.csv"), col_types = cols()) %>%
  select(-fixed, -calibrate, -sig.level)

for(p.name in names(fixed.params)) {
  this.param <- ALL.PARAMS %>%
    dplyr::filter(param.name == p.name)
  other.tiers[p.name] <- runif(n = nrow(other.tiers),
                               min = this.param$param.min,
                               max = this.param$param.max)
}

INITIAL.POP <- first.tier %>%
  dplyr::bind_rows(other.tiers) %>%
  dplyr::select(-iter, -id, -volume, -rmse.dbh.A2, -rmse.dbh.A3, -rmse.dbh.A4)


write_csv(INITIAL.POP, paste0(lhs.output.path, "GA_INITIAL_POP.csv"))

# pareto.ids <- psel(lhs.data, low(rmse.dbh) * low(rmse.dbh.inc)) %>%
#   .$id
#
# non.pareto.ids <- sample(non.aborted.ids[!(non.aborted.ids %in% plot.pareto.ids)], 1)
#
# GA.init <- calib.sample %>%
#   filter(id %in% c(plot.pareto.ids, non.pareto.ids))
#
#
# GA.init.ids <- GA.init$id
#
# GA.init.params <- GA.init %>%
#   select(-batch, -id)
#
# write_csv(GA.init.params,
#           "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/GA1/input/INITIAL_POP.csv")

# GA.init.fitness <- lhs.data %>%
#   select(id, rmse.dbh.A2, rmse.dbh.A3, rmse.dbh.A4) %>%
#   filter(id %in% GA.init.ids)
#
# GA.init.fitness <- GA.init %>%
#   select(id) %>%
#   left_join(GA.init.fitness, by = "id") %>%
#   select(-id) %>%
#   as.matrix() %>%
#   t() %>%
#   replace_na(999)
#
# GA.init.fitness <- GA.init %>%
#   select(id) %>%
#   left_join(GA.init.fitness, by = "id") %>%
#   select(-id)
#
# write_csv(GA.init.fitness,
#           "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/GA1/input/INITIAL_FITNESS.csv")
#
#
# ggplot(GA.init.fitness, aes(x = rmse.dbh.A2, y = rmse.dbh.A3, color = rmse.dbh.A4)) +
#   geom_point()
