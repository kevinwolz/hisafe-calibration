### hisafe-calibration
### CROPS
### Author: Kevin J. Wolz

## REFERENCE CELLS
# 13x8
# NORTH  <- c(48,49,61,62) # RHS of Scene
# MIDDLE <- c(40,52,53,65)
# SOUTH  <- c(43,44,56,57) # LHS of Scene

# 13x9
NORTH  <- c(61,62) # RHS of Scene
MIDDLE <- c(53,65)
SOUTH  <- c(56,57) # LHS of Scene

CELL.IDS  <- c(NORTH, MIDDLE, SOUTH)
REF.CELLS <- tibble(System = c(rep("Agroforestry", length(CELL.IDS)), "Monocrop"),
                    location = c(rep(c("North", "Middle", "South"), each = 2), "Monocrop"),
                    id = c(CELL.IDS, 1))

## MODELED CROP YIELD (raw units tons ha-1)
modeled.yield <- face$annualcrop %>%
  filter(System != "Forestry") %>%
  filter(SimulationName %in% c("Monocrop", MODELED.SITE)) %>%
  select(System, Year, id, x, y, yieldMax) %>%
  mutate(Year = Year - 1) %>% # the Year in Hi-sAFe annual export data is the YEAR AFTER HARVEST
  filter(Year > 1995) %>%
  left_join(REF.CELLS, by = c("System", "id")) %>%
  filter(!is.na(location)) %>%
  group_by(System, Year, location) %>%
  rename(year = Year) %>%
  summarize(modeled.yield = mean(yieldMax))

## COMBINED CROP YIELD
yield <- modeled.yield %>%
  left_join(measured.yield, by = c("System", "year", "location")) %>%
  mutate(location = factor(location,
                           c("Monocrop", "North", "Middle", "South"),
                           c("Monocrop", "AF-North", "AF-Middle", "AF-South")))

sd.yield <- yield %>%
  filter(System == "Agroforestry") %>%
  group_by(System, year) %>%
  summarize(modeled.sd = sd(modeled.yield, na.rm = TRUE),
            measured.sd = sd(measured.yield, na.rm = TRUE))

AF.yield <- yield %>%
  ungroup() %>%
  filter(System == "Agroforestry") %>%
  select(-System, -crop, -measured.yield.sd) %>%
  rename(modeled.yield.AF = modeled.yield) %>%
  rename(measured.yield.AF = measured.yield)

CC.yield <- yield %>%
  ungroup() %>%
  filter(System == "Monocrop") %>%
  select(-System, -crop, -location, -measured.yield.sd) %>%
  rename(modeled.yield.CC = modeled.yield) %>%
  rename(measured.yield.CC = measured.yield)

rel.yield <- AF.yield %>%
  left_join(CC.yield, by = "year") %>%
  mutate(modeled.rel.yield = modeled.yield.AF / modeled.yield.CC) %>%
  mutate(measured.rel.yield = measured.yield.AF / measured.yield.CC) %>%
  select(-(modeled.yield.AF:measured.yield.CC))

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
#   theme_ggEHD()
#
# ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_", gsub("\\.", "_", i), ".jpg"), crop.ts.plot)

## MEASURED vs. MODELED SCATTERPLOT
crop.scatterplot <- ggplot(yield, aes(x = modeled.yield, y = measured.yield)) +
  labs(x = "Modeled wheat yield (ton ha-1)",
       y = "Measured wheat yield (ton ha-1)",
       title = paste("Hi-sAFe Calibration:", MODELED.SITE),
       shape = "Zone",
       fill = "Year") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = measured.yield - measured.yield.sd,
                    ymax = measured.yield + measured.yield.sd), na.rm = TRUE) +
  geom_point(aes(fill = year, shape = location), na.rm = TRUE) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0,10)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0,10)) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_fill_viridis(option = "magma") +
  annotate("text", x = 10, y = 0, label = mvm_annotation(yield$modeled.yield, yield$measured.yield), hjust = 1, vjust = 0) +
  theme_ggEHD() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_crop_yield.jpg"), crop.scatterplot, scale = 1.1)


## MEASURED vs. MODELED STDEV SCATTERPLOT
sd.scatterplot <- ggplot(sd.yield, aes(x = modeled.sd, y = measured.sd)) +
  labs(x = "Modeled wheat yield SD (ton ha-1)",
       y = "Measured wheat yield SD (ton ha-1)",
       title = paste("Hi-sAFe Calibration:", MODELED.SITE),
       fill = "Year") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = year), shape = 21, na.rm = TRUE) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0,1.7)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0,1.7)) +
  scale_fill_viridis(option = "magma") +
  annotate("text", x = 1.7, y = 0, label = mvm_annotation(sd.yield$modeled.sd, sd.yield$measured.sd), hjust = 1, vjust = 0) +
  theme_ggEHD() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_crop_yield_SD.jpg"), sd.scatterplot, scale = 1.1)

## MEASURED vs. MODELED STDEV SCATTERPLOT
rel.scatterplot <- ggplot(rel.yield, aes(x = modeled.rel.yield, y = measured.rel.yield)) +
  labs(x = "Modeled wheat realtive yield",
       y = "Measured wheat realtive yield",
       title = paste("Hi-sAFe Calibration:", MODELED.SITE),
       shape = "Zone",
       fill = "Year") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill = year, shape = location), na.rm = TRUE) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0.5,1)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0.5,1)) +
  scale_shape_manual(values = c(22, 23, 24)) +
  scale_fill_viridis(option = "magma") +
  annotate("text", x = 0.5, y = 1, label = mvm_annotation(rel.yield$modeled.rel.yield, rel.yield$measured.rel.yield), hjust = 0, vjust = 1) +
  theme_ggEHD() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_crop_yield_relative.jpg"), rel.scatterplot, scale = 1.1)
