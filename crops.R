### hisafe-calibration
### CROPS
### Author: Kevin J. Wolz

## REFERENCE CELLS
# 13x8
# NORTH  <- c(43,44,56,57)
# MIDDLE <- c(40,52,53,65)
# SOUTH  <- c(48,49,61,62)

# 13x9
NORTH  <- c(56,57)
MIDDLE <- c(53,65)
SOUTH  <- c(61,62)

CELL.IDS  <- c(NORTH, MIDDLE, SOUTH)
REF.CELLS <- tibble(System = c(rep("Agroforestry", length(CELL.IDS)), "Monocrop"),
                    location = c(rep(c("North", "Middle", "South"), each = 2), "Monocrop"),
                    id = c(CELL.IDS, 1))

## MODELED CROP YIELD (raw units tons ha-1)
modeled.yield <- face$annualcrop %>%
  filter(System != "Forestry") %>%
  filter(SimulationName %in% c("Monocrop", MODELED.SITE)) %>%
  select(System, Year, id, x, y, yieldMax) %>%
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
  labs(x = "Modeled crop yield (ton ha-1)", y = "Measured crop yield (ton ha-1)", shape = "Zone", fill = "Year") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = measured.yield - measured.yield.sd,
                    ymax = measured.yield + measured.yield.sd), na.rm = TRUE) +
  geom_point(aes(fill = year, shape = location), na.rm = TRUE) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0,10)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0,10)) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_fill_viridis(option = "magma") +
  theme_ggEHD()

ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_crop_yield.jpg"), crop.scatterplot)
