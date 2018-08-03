### hisafe-calibration
### MISC
### Author: Kevin J. Wolz

SIMS <- CALIBRATION.SIMULATIONS
NEW.SIM.NAMES <- purrr::map_chr(str_split(CALIBRATION.SIMULATIONS, "-"), 2)

##### CROP HARVEST DATE #####
measured.hd <- readr::read_csv("./raw_data/restinclieres_crop_management.csv", col_types = readr::cols()) %>%
  mutate(harvest.doy.measured = harvest.doy - 365) %>%
  rename(Year = year) %>%
  select(Year, harvest.doy.measured)

modeled.hd <- hop$plot %>%
  filter(SimulationName != "Restinclieres-A4") %>%
  filter(mainCropName == "durum-wheat") %>%
  filter(JulianDay > 100, JulianDay < 200) %>%
  filter(mainCropMeanBiomass == 0) %>%
  group_by(Year, SimulationName) %>%
  summarize(harvest.doy.modeled = min(JulianDay))

harvest.data <- modeled.hd %>%
  left_join(measured.hd, by = "Year") %>%
  filter(!is.na(harvest.doy.modeled), !is.na(harvest.doy.measured)) %>%
  mutate(SimulationName = factor(SimulationName,
                                 levels = c("Restinclieres-A2", "Restinclieres-A3", "Monocrop-A2", "Monocrop-A3"),
                                 labels = c("Silvoarable-A2",   "Silvoarable-A3",   "Monocrop-A2", "Monocrop-A3")))

harvest.doy.plot <- ggplot(harvest.data, aes(x = harvest.doy.modeled, y = harvest.doy.measured, color = Year)) +
  labs(x = "Modeled harvest day",
       y = "Measured harvest day") +
  facet_wrap(~SimulationName) +
  geom_point() +
  geom_abline() +
  coord_equal() +
  theme_hisafe_ts(panel.grid = element_blank())
ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_harvesty_day.png"), harvest.doy.plot, scale = 1.5)

##### INTERCROP BIOMASS #####
## The only field data on this is that intercrop biomass is ~3.6 tons/ha in 2014 in A2.
intercrop.biomass.measured <- tibble(Date = ymd(c("2014-01-01", "2014-12-31")), biomass = 3.6, SimulationName = "A2")

intercrop.biomass.modeled <- hop$cells %>%
  filter(SimulationName %in% SIMS) %>%
  filter(cropType == "interCrop") %>%
  group_by(SimulationName, Date) %>%
  summarize(biomass = mean(biomass)) %>%
  ungroup() %>%
  mutate(SimulationName = factor(SimulationName, levels = SIMS, labels = NEW.SIM.NAMES))

plot.annotation <- data.frame(SimulationName = NEW.SIM.NAMES)
plot.annotation$Date <- min(intercrop.biomass.modeled$Date, na.rm = TRUE)
plot.annotation$biomass <- max(intercrop.biomass.modeled$biomass, na.rm = TRUE)

ic.biomass.plot <- ggplot(intercrop.biomass.modeled, aes(x = Date, y = biomass)) +
  labs(x = "Year",
       y = "Understory biomass (t/ha)") +
  facet_wrap(~SimulationName) +
  geom_line(color = "grey50") +
  geom_line(data = intercrop.biomass.measured, color = "red", size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_text(data = plot.annotation, aes(label = SimulationName), hjust = 0, vjust = 1, size = 5) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())
ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_interCrop_biomass.png"), ic.biomass.plot, scale = 1.5)

##### A2 PIT SOIL MOISTURE #####
# pit.sm <- readr::read_csv("./raw_data/restinclieres_A2_pit_soil_moisture.csv", col_types = readr::cols()) %>%
#   mutate(date = dmy(date))
#
# bad.tree.dates <- c(dmy(c("25/11/15", "08/04/16", "18/07/16", "28/04/17", "19/05/17", "31/05/17")),
#                     seq(dmy("19/09/14"), dmy("19/12/14"), "1 day"))
# bad.btwn.dates <- c(dmy(c("31/08/16", "25/04/17", "06/07/17")),
#                     seq(dmy("19/09/14"), dmy("19/12/14"), "1 day"),
#                     seq(dmy("18/10/16"), dmy("14/12/16"), "1 day"))
# pit.sm$theta.measured[which(pit.sm$date %in% bad.tree.dates & pit.sm$location == "tree")] <- NA
# pit.sm$theta.measured[which(pit.sm$date %in% bad.btwn.dates & pit.sm$location == "between-tree")] <- NA
#
# #  mutate(harvest.doy.measured = harvest.doy - 365) %>%
# #  rename(Year = year) %>%
# #  select(Year, harvest.doy.measured)
# ggplot(pit.sm, aes(x = date, y = theta.measured)) +
#   labs(x = "Date", y = "Soil moisture") +
#   facet_grid(depth~location) +
#   geom_line() +
#   theme_hisafe_ts()

##### TREE CANOPY RADIUS #####
## THIS ISN'T REALLY A GREAT COMPARISON TO MAKE BECAUSE THE MEASURED DATA WAS ON SPECIFIC TREES
## FROM A2 THAT HAD NO NEIGHBORS AND WERE CONSEQUENTLY "OPEN-GROWN".

# rest.crowns <- read_csv(paste0(input.path, "Restinclieres_2018_Crown_Diam_Data.csv"), col_types = cols()) %>%
#   filter(plot == "A2") %>%
#   mutate(date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>%
#   mutate(measured.crown.radius.interrow = (N + S) / 2) %>%
#   mutate(measured.crown.radius.treeline = (E + W) / 2) %>%
#   select(date, measured.crown.radius.interrow, measured.crown.radius.treeline)
#
# plot.annotation <- data.frame(plot = "A2")
# plot.annotation$date <- min(modeled.trees$date, na.rm = TRUE)
# plot.annotation$measured.crown.radius.interrow <- max(rest.crowns$measured.crown.radius.interrow, na.rm = TRUE)
# plot.annotation$measured.crown.radius.treeline <- max(rest.crowns$measured.crown.radius.treeline, na.rm = TRUE)
#
# interrow.plot <- ggplot(rest.crowns, aes(x = date, y = measured.crown.radius.interrow)) +
#   labs(x = "Year",
#        y = "Crown interrow radius (m)") +
#   geom_boxplot(na.rm = TRUE, outlier.shape = NA, width = 100) +
#   stat_summary(fun.y = mean, color = "grey30", geom = "point", size = 1, na.rm = TRUE) +
#   geom_line(data = filter(modeled.trees, plot == "A2"), aes(y = modeled.crown.radius.interrow), color = "grey50") +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
#   theme_hisafe_ts(panel.grid       = element_blank())
# ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_crownRadiusInterRow.png"), interrow.plot, scale = 1)
#
# treeline.plot <- ggplot(rest.crowns, aes(x = date, y = measured.crown.radius.treeline)) +
#   labs(x = "Year",
#        y = "Crown within-row radius (m)") +
#   geom_boxplot(na.rm = TRUE, outlier.shape = NA, width = 100) +
#   stat_summary(fun.y = mean, color = "grey30", geom = "point", size = 1, na.rm = TRUE) +
#   geom_line(data = filter(modeled.trees, plot == "A2"), aes(y = modeled.crown.radius.treeline), color = "grey50") +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
#   theme_hisafe_ts(panel.grid       = element_blank())
# ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_crownRadiusTreeLine.png"), treeline.plot, scale = 1)
