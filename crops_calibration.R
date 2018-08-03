### hisafe-calibration
### CROPS
### Author: Kevin J. Wolz

## REFERENCE CELLS
# Restinclieres-A2 (13x8, with two trees)
A2.NORTH  <- c(70,70) #c(69,70) # LHS of Scene
A2.MIDDLE <- c(66,78)
A2.SOUTH  <- c(74,74)#c(74,75) # RHS of Scene

# Restinclieres-A3 (13x8, with two trees)
A3.EAST   <- c(69,70) # LHS of Scene
A3.MIDDLE <- c(66,78)
A3.WEST   <- c(74,75) # RHS of Scene

CELL.IDS  <- c(A2.NORTH, A2.MIDDLE, A2.SOUTH, A3.EAST, A3.MIDDLE, A3.WEST)
REF.CELLS <- tibble(plot     = c(rep(c("Restinclieres-A2", "Restinclieres-A3"), each = 6), "Monocrop-A2", "Monocrop-A3"),
                    location = c(rep(c("North", "Middle", "South"), each = 2), rep(c("East", "Middle", "West"), each = 2), "Monocrop", "Monocrop"),
                    idCell   = c(CELL.IDS, 1, 1)) %>%
  distinct()

## MODELED CROP YIELD (raw units tons ha-1)
modeled.yield <- hop$annualCells %>%
  filter(SimulationName %in% c("Restinclieres-A2", "Restinclieres-A3", "Monocrop-A2", "Monocrop-A3")) %>%
  select(SimulationName, Year, idCell, x, y, yieldMax) %>%
  rename(plot = SimulationName) %>%
  mutate(Year = Year - 1) %>% # the Year in Hi-sAFe annual export data is the YEAR AFTER HARVEST
  filter(Year > 1995) %>%
  left_join(REF.CELLS, by = c("plot", "idCell")) %>%
  filter(!is.na(location)) %>%
  group_by(plot, Year, location) %>%
  rename(year = Year) %>%
  summarize(modeled.yield = mean(yieldMax))

modeled.yield$plot[modeled.yield$plot == "Monocrop-A2"] <- "Restinclieres-A2"
modeled.yield$plot[modeled.yield$plot == "Monocrop-A3"] <- "Restinclieres-A3"

## COMBINED CROP YIELD
yield <- modeled.yield %>%
  left_join(measured.yield, by = c("plot", "year", "location")) %>%
  mutate(comparable = as.numeric(year %in% c(2004, 2005, 2008, 2009, 2012)) + 1) %>%
  mutate(group = purrr::map_chr(strsplit(plot, "-"),2)) %>%
  filter(group == "A2")
yield$comparable <- factor(yield$comparable, c("1", "2"), c("no", "yes"))

yield$location[yield$location == "East"] <- "North"
yield$location[yield$location == "West"] <- "South"
yield$location <- factor(yield$location,
                         c("Monocrop", "North",  "Middle",    "South"),
                         c("Monocrop", "AF-N/E", "AF-Middle", "AF-S/W"))

sd.yield <- yield %>%
  filter(location != "Monocrop") %>%
  group_by(plot, group, year, crop) %>%
  summarize(modeled.sd  = sd(modeled.yield,  na.rm = TRUE),
            measured.sd = sd(measured.yield, na.rm = TRUE)) %>%
  mutate(comparable = as.numeric(year %in% c(2004, 2005, 2008, 2009, 2012)) + 1)
sd.yield$comparable <- factor(sd.yield$comparable, c("1", "2"), c("no", "yes"))

AF.yield <- yield %>%
  ungroup() %>%
  filter(location != "Monocrop") %>%
  select(-plot, -measured.yield.sd) %>%
  rename(modeled.yield.AF  = modeled.yield) %>%
  rename(measured.yield.AF = measured.yield)

CC.yield <- yield %>%
  ungroup() %>%
  filter(location == "Monocrop") %>%
  select(-plot, -location, -measured.yield.sd, -comparable) %>%
  rename(modeled.yield.CC  = modeled.yield) %>%
  rename(measured.yield.CC = measured.yield)

rel.yield <- AF.yield %>%
  left_join(CC.yield, by = c("group", "year", "crop")) %>%
  mutate(modeled.rel.yield  = modeled.yield.AF  / modeled.yield.CC) %>%
  mutate(measured.rel.yield = measured.yield.AF / measured.yield.CC) %>%
  select(-modeled.yield.AF, -modeled.yield.CC, -measured.yield.AF, -measured.yield.CC)

## MEASURED vs. MODELED TIME SERIES
# crop.ts.plot <- ggplot(yield, aes(x = year, y = measured.yield)) +
#   labs(x = "Year", y = "Crop yield (ton ha-1)", color = "", fill = "") +
#   geom_errorbar(aes(ymin = measured.yield - measured.yield.sd,
#                     ymax = measured.yield + measured.yield.sd), na.rm = TRUE, width = 0.5) +
#   geom_line(aes(color = location), linetype = "solid", na.rm = TRUE) +
#   geom_point(aes(fill = location), shape = 21, na.rm = TRUE) +
#   scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(1994,2017), breaks = seq(1995, 2015, 5)) +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_line(aes(y = modeled.yield, color = location), linetype = "dashed", na.rm = TRUE) +
#   geom_point(aes(y = modeled.yield, fill = location), shape = 21, na.rm = TRUE) +
#   scale_color_manual(values = cbPalette) +
#   scale_fill_manual(values = cbPalette) +
#   theme_hisafe_ts()
#
# ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_", gsub("\\.", "_", i), ".jpg"), crop.ts.plot)

## MEASURED vs. MODELED SCATTERPLOT
LIMITS <- c(0, 6)
# plot.annotation <- data.frame(group = paste0("A", 2:3))
# plot.annotation$modeled.yield  <- LIMITS[1]
# plot.annotation$measured.yield <- LIMITS[2]
yield$year.label <- as.character(yield$year)
crop.scatterplot <- ggplot(yield, aes(x = modeled.yield, y = measured.yield)) +
  labs(x     = "Modeled wheat yield (ton ha-1)",
       y     = "Measured wheat yield (ton ha-1)",
       shape = "Zone",
       fill  = "Year",
       #size  = "DW after DW"
       color = NULL) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = measured.yield - measured.yield.sd,
                    ymax = measured.yield + measured.yield.sd), na.rm = TRUE) +
  #geom_point(aes(fill = year, shape = location, size = comparable, color = group), na.rm = TRUE) +
  geom_point(aes(fill = year, shape = location, color = crop), na.rm = TRUE, size = 2) +
  #geom_text(aes(label = year.label), size = 0.5) +
  facet_wrap(~plot) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_color_manual(values = c("black", "green")) +
  #scale_size_manual(values = c(2, 3)) +
  scale_fill_viridis(option = "magma") +
  guides(color = FALSE) +
  annotate("text", x = LIMITS[2], y = LIMITS[1], label = mvm_annotation(yield$modeled.yield, yield$measured.yield), hjust = 1, vjust = 0) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())

ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_crop_yield.jpg"), crop.scatterplot, scale = 1.2)


## MEASURED vs. MODELED STDEV SCATTERPLOT
LIMITS <- c(0, 1.7)
# plot.annotation <- data.frame(group = paste0("A", 2:3))
# plot.annotation$modeled.sd  <- LIMITS[1]
# plot.annotation$measured.sd <- LIMITS[2]

sd.scatterplot <- ggplot(sd.yield, aes(x = modeled.sd, y = measured.sd)) +
  labs(x     = "Modeled wheat yield SD (ton ha-1)",
       y     = "Measured wheat yield SD (ton ha-1)",
       fill  = "Year",
       color = NULL) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = year, color = crop), size = 3, shape = 21, na.rm = TRUE) +
  #geom_point(aes(fill = year, size = comparable), shape = 21, na.rm = TRUE) +
  #geom_point(aes(color = group, size = comparable), shape = 21, fill = "transparent", na.rm = TRUE) +
  facet_wrap(~group) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
  scale_fill_viridis(option = "magma") +
  scale_color_manual(values = c("black", "green")) +
  guides(color = FALSE) +
  annotate("text", x = LIMITS[2], y = LIMITS[1], label = mvm_annotation(sd.yield$modeled.sd, sd.yield$measured.sd), hjust = 1, vjust = 0) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())

ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_crop_yield_SD.jpg"), sd.scatterplot, scale = 1.7)

## MEASURED vs. MODELED RELATIVE YIELD SCATTERPLOT
LIMITS <- c(0, 1.5)
# plot.annotation <- data.frame(group = paste0("A", 2:3))
# plot.annotation$modeled.rel.yield  <- LIMITS[1]
# plot.annotation$measured.rel.yield <- LIMITS[2]
rel.yield$year.label <- as.character(rel.yield$year)
rel.scatterplot <- ggplot(rel.yield, aes(x = modeled.rel.yield, y = measured.rel.yield)) +
  labs(x     = "Modeled wheat relative yield",
       y     = "Measured wheat relative yield",
       shape = "Zone",
       fill  = "Year",
       color = NULL) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  #geom_point(aes(fill = year, shape = location, size = comparable), na.rm = TRUE) +
  geom_point(aes(fill = year, shape = location, color = crop), size = 3, na.rm = TRUE) +
  #geom_point(aes(color = group, shape = location, size = comparable), fill = "transparent", na.rm = TRUE) +
  #geom_text(aes(label = year.label), size = 0.5) +
  facet_wrap(~group) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
  scale_shape_manual(values = c(22, 23, 24)) +
  scale_color_manual(values = c("black", "green")) +
  scale_fill_viridis(option = "magma") +
  scale_size_manual(values = c(2, 3)) +
  guides(color = FALSE) +
  annotate("text", x = LIMITS[1], y = LIMITS[2], label = mvm_annotation(rel.yield$modeled.rel.yield, rel.yield$measured.rel.yield), hjust = 0, vjust = 1) +
  theme_hisafe_ts(strip.background = element_blank(),
                  strip.text       = element_blank(),
                  panel.grid       = element_blank())

ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_crop_yield_relative.jpg"), rel.scatterplot, scale = 1.2)
