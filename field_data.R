### hisafe-calibration
### FIELD DATA
### Author: Kevin J. Wolz

mean_radius <- function(a, b) {
  out <- data.frame(a = a, b = b)
  out <- rowMeans(out, na.rm = TRUE)
  out[is.nan(out)] <- NA
  return(out)
}
reldiff <- function(x) diff(x) / x[-length(x)]

##### DEFINE EDGE TRIES #####
paste_h <- function(...) paste(..., sep = "-")

A2.EDGE.TREES <- c(paste_h("A", 1:100),
                   paste_h("B", c(10:12, 69)),
                   paste_h("C", c(9:10, 68)),
                   paste_h("D", c(8:10, 68:70)),
                   paste_h("E", c(6:8, 70:73)),
                   paste_h("F", c(4:6, 73:76)),
                   paste_h("G", c(3:4, 76:80)),
                   paste_h("H", c(1:3, 50:62, 80:83)),
                   paste_h("I", c(1:5, 42:50, 61:68, 82:84)),
                   paste_h("J", 1:100),
                   paste_h("K", 1:100))

A3.EDGE.TREES <- c(paste_h(1, 1:100),
                   paste_h(2, c(1:10, 44:100)),
                   paste_h(3, c(3:4, 44:100)),
                   paste_h(4, c(4:5, 44:100)),
                   paste_h(5, c(5:16, 44:100)),
                   paste_h(6, 1:100),
                   paste_h(7, 1:100),
                   paste_h(8, 1:100))

A4.GOOD.TREES <- c(paste_h(2,  c(3)),
                   paste_h(3,  c(2:5)),
                   paste_h(4,  c(4:5)),
                   paste_h(5,  c(3:5, 38)),
                   paste_h(6,  c(6:7, 32:34)),
                   paste_h(7,  c(4:6, 8:9, 14:18, 22:28, 32, 36:37)),
                   paste_h(8,  c(3:7, 9:11, 17, 21:22, 37)),
                   paste_h(9,  c(3:9)),
                   paste_h(10, c(5:6, 8, 31:33)))

CASTRIE.EDGE.TREES <- c(paste_h(1, 1:33),
                        paste_h(2, c(1:2, 33)),
                        paste_h(3, c(2, 32:33)),
                        paste_h(4, c(2, 31:32)),
                        paste_h(5, c(2, 28:31)),
                        paste_h(6, c(2, 27:28)),
                        paste_h(7, c(2, 26:27)),
                        paste_h(8, c(2:3, 24:26)),
                        paste_h(9, 3:24))

##### MEASURED TREE BIOMETRICS #####
restinclieres.trees <- read_csv(paste0(input.path, "restinclieres_tree_biometrics.csv"), col_types = cols(), guess_max = 3000) %>%
  filter(!is.na(value),
         !(variable %in% c("diametre_a_la_base_du_tronc", "circonference_tronc_a_130cm")),
         (species != "NN" | is.na(species)),
         !(plot == "Restinclieres-A2" & System == "Forestry"), # this plot is too small to use for model comparisons
         value > 0) %>%
  mutate(rowtree.id = str_replace(rowtree.id, "'", "")) %>%
  mutate(year = season.year) %>%
  mutate(age  = season.year - 1995) %>%
  mutate(variable = factor(variable, labels = c("measured.dbh", "measured.pruned.height", "measured.height"))) %>%
  mutate(value = value / 100) %>% # Convert cm to m
  spread(variable, value) %>%
  filter(!(plot == "Restinclieres-A2" & row.id %in% c("E", "F"))) %>% # Don't use rows E&F after 2013 because they were pollarded - & year > 2013
  select(System, plot, year, date, id, rowtree.id, row.id, tree.id, age, measured.dbh, measured.pruned.height, measured.height)

restinclieres.trees <- restinclieres.trees %>%
  select(plot, rowtree.id) %>%
  distinct() %>%
  mutate(i = 1) %>%
  inner_join(tibble(year = seq(min(restinclieres.trees$year), max(restinclieres.trees$year), 1), i = 1), by = "i") %>%
  select(-i) %>%
  left_join(restinclieres.trees, by = c("plot", "rowtree.id", "year")) %>%
  mutate(date = ymd(paste0(year, "-12-15"))) %>%
  mutate(row.id  = purrr::map_chr(str_split(rowtree.id, "-"), 1)) %>%
  mutate(tree.id = as.numeric(purrr::map_chr(str_split(rowtree.id, "-"), 2)))

## Remove individual tree-year points that are sharp jumps or negative growth
bad.dbh    <-  read_csv(paste0(input.path, "Bad_Tree_DBH_data.csv"),    col_types = cols()) %>%
  mutate(bad.check = paste(plot, rowtree.id, year, sep = "-"))
bad.height <-  read_csv(paste0(input.path, "Bad_Tree_Height_data.csv"), col_types = cols()) %>%
  mutate(bad.check = paste(plot, rowtree.id, year, sep = "-"))
restinclieres.trees <- restinclieres.trees %>%
  mutate(bad.check = paste(plot, rowtree.id, year, sep = "-"))

restinclieres.trees$measured.dbh[restinclieres.trees$bad.check    %in% bad.dbh$bad.check]    <- NA
restinclieres.trees$measured.height[restinclieres.trees$bad.check %in% bad.height$bad.check] <- NA

restinclieres.trees <- select(restinclieres.trees, -bad.check)

## Remove 2017 tree height data - clearly measurement error!
restinclieres.trees$measured.height[which(restinclieres.trees$year == 2017)] <- NA

A2.LIVING.TREES <- restinclieres.trees %>%
  filter(plot == "Restinclieres-A2") %>%
  filter(year == 2017) %>%
  filter(!is.na(measured.dbh)) %>%
  .$rowtree.id
A3.LIVING.TREES <- restinclieres.trees %>%
  filter(plot == "Restinclieres-A3") %>%
  filter(year == 2017) %>%
  filter(!is.na(measured.dbh)) %>%
  .$rowtree.id
A4.LIVING.TREES <- restinclieres.trees %>%
  filter(plot == "Restinclieres-A4") %>%
  filter(year == 2017) %>%
  filter(!is.na(measured.dbh)) %>%
  .$rowtree.id

castries.trees <- read_csv(paste0(input.path, "castries_tree_biometrics.csv"), col_types = cols(), guess_max = 4000) %>%
  mutate(id     = as.character(id)) %>%
  mutate(row.id = as.character(row.id)) %>%
  mutate(plot   = "Castries") %>%
  mutate(System = c("Forestry", "Agroforestry")[as.numeric(trt.new == "T") + 1]) %>%
  mutate(year = season.year) %>%
  mutate(age  = season.year - 1991) %>%
  mutate(measured.dbh             = dbh.mm           / 1000) %>% # Convert mm to m
  mutate(measured.pruned.height   = pruned.height.cm  / 100) %>% # Convert cm to m
  mutate(measured.height          = height.cm         / 100) %>% # Convert cm to m
  mutate(crown.radius.N           = crown.radius.N.cm / 100) %>% # Convert cm to m
  mutate(crown.radius.S           = crown.radius.S.cm / 100) %>% # Convert cm to m
  mutate(crown.radius.E           = crown.radius.E.cm / 100) %>% # Convert cm to m
  mutate(crown.radius.W           = crown.radius.W.cm / 100) %>% # Convert cm to m
  mutate(crown.radius.tree.line = mean_radius(crown.radius.N, crown.radius.S)) %>%
  mutate(crown.radius.inter.row = mean_radius(crown.radius.E, crown.radius.W)) %>%
  mutate(crown.radius           = mean_radius(crown.radius.tree.line, crown.radius.inter.row)) %>%
  mutate(crown.area             = pi * crown.radius^2) %>%
  select(System, plot, year, id, rowtree.id, row.id, tree.id, alley.crop, age, measured.dbh, measured.pruned.height, measured.height,
         crown.radius.tree.line, crown.radius.inter.row, crown.radius, crown.area)

castries.trees <- castries.trees %>%
  select(plot, id) %>%
  distinct() %>%
  mutate(i = 1) %>%
  inner_join(tibble(year = seq(min(castries.trees$year), max(castries.trees$year), 1), i = 1), by = "i") %>%
  select(-i) %>%
  left_join(castries.trees, by = c("plot", "id", "year")) %>%
  mutate(date = ymd(paste0(year, "-12-15")))

## Unfiltered data for use in building allometries
measured.trees.all <- restinclieres.trees %>%
  bind_rows(castries.trees) %>%
  mutate(measured.pruned.height = pmax(0, measured.pruned.height + -18.5 * measured.dbh ^ 3.03))
  # adjust pruned height from what was measured in the field as the connection point of the lowest branch
  # to what is actually the lowest point of the canopy (using relationship between the difference between
  # these two and measured dbh - this was measured in April 2018 on 20 trees in Restinclieres A2).

measured.trees.allom <- measured.trees.all %>%
  filter(!is.na(measured.dbh)) %>%
  group_by(plot, year, row.id) %>%
  arrange(plot, year, row.id, tree.id) %>%
  mutate(spacing.1 = c(NA, diff(tree.id))) %>%
  mutate(spacing.2 = c(diff(tree.id), NA)) %>%
  ungroup()

measured.trees.allom$spacing.1[measured.trees.allom$spacing.1 < 1] <- NA
measured.trees.allom$spacing.1[measured.trees.allom$spacing.1 > 6] <- NA
measured.trees.allom$spacing.2[measured.trees.allom$spacing.2 < 1] <- NA
measured.trees.allom$spacing.2[measured.trees.allom$spacing.2 > 6] <- NA

measured.trees.allom <- measured.trees.allom %>%
  mutate(mean.spacing = (spacing.1 + spacing.2) / 2) %>%
  mutate(even.spacing = as.numeric(spacing.1 == spacing.2) * spacing.1) %>%
  rename(height        = measured.height) %>%
  rename(dbh           = measured.dbh) %>%
  rename(pruned.height = measured.pruned.height) %>%
  filter(!(plot %in% c("Restinclieres-A4", VALIDATION.SIMULATIONS))) %>%                   # Remove forestry trees or data from validation sites
  filter(!(plot == "Restinclieres-A2" & row.id %in% c("C", "G") & year >= 2004)) %>%       # Don't use rows C&G after 2004 because they were pruned high
  mutate(plot = str_replace(plot, "-A2|-A3", "")) %>%                                      # Treat all restinclieres as one plot for allometry
  filter(!(plot == "Restinclieres" & spacing.1 < 2 & year >= 2004) | is.na(spacing.1)) %>% # Remove trees that are too close together
  filter(!(plot == "Restinclieres" & spacing.2 < 2 & year >= 2004) | is.na(spacing.2)) %>% # Remove trees that are too close together
  filter(!(plot == "Restinclieres" & spacing.1 < 3 & year >= 2014) | is.na(spacing.1)) %>% # Remove trees that are too close together
  filter(!(plot == "Restinclieres" & spacing.2 < 3 & year >= 2014) | is.na(spacing.2))     # Remove trees that are too close together

## Filtered data for calibration/validation
measured.trees <- measured.trees.all %>%
  mutate(measured.dbh = measured.dbh * 100) %>% # Convert m to cm for calibration/validation
  ## Filter A2 Trees
  filter(!(plot == "Restinclieres-A2" & rowtree.id   %in% A2.EDGE.TREES)) %>%      # Don't use edge trees
  filter(!(plot == "Restinclieres-A2" & row.id       %in% c("D", "G"))) %>%        # Don't use rows D&G bc after 2013 they were adjacent to pollarded E&F
  filter(!(plot == "Restinclieres-A2" & row.id       %in% c("C", "G"))) %>%        # Don't use rows C&G bc ~2004 they were pruned high
  filter(!(plot == "Restinclieres-A2" & row.id       %in% LETTERS[1:6])) %>%       # Don't use rows A-F bc they're far from the two piezos used in water table model
  filter(!(plot == "Restinclieres-A2" & !(rowtree.id %in% A2.LIVING.TREES))) %>%   # Use only trees still living today
  ## Filter A3 Trees
  filter(!(plot == "Restinclieres-A3" & rowtree.id   %in% A3.EDGE.TREES)) %>%      # Don't use edge trees
  filter(!(plot == "Restinclieres-A3" & !(rowtree.id %in% A3.LIVING.TREES))) %>%   # Use only trees still living today
  ## Filter A4 Trees
  filter(!(plot == "Restinclieres-A4" & !(rowtree.id %in% A4.GOOD.TREES))) %>%     # Use specific trees that are not on edges and were not fertilized
  filter(!(plot == "Restinclieres-A4" & !(rowtree.id %in% A4.LIVING.TREES))) %>%   # Use only trees still living today
  ## Filter Castries Trees
  filter(!(plot == "Castries"         & rowtree.id   %in% CASTRIE.EDGE.TREES)) %>% # Don't use edge trees
  filter(!(plot == "Castries"         & alley.crop   %in% c("L", "FL"))) %>%       # Only use trees that had Fescue as alley crop
  ## Calculate growth in DBH and height
  group_by(plot, rowtree.id) %>%
  arrange(plot, rowtree.id, date) %>%
  #mutate(measured.dbh.inc    = c(NA, diff(measured.dbh))) %>%           # Calculate ABSOLUE increase
  #mutate(measured.height.inc = c(NA, diff(measured.height)) * 100) %>%  # Calculate ABSOLUE increase
  mutate(measured.dbh.inc    = c(NA, reldiff(measured.dbh))) %>%    # Calculate RELATIVE increase
  mutate(measured.height.inc = c(NA, reldiff(measured.height))) %>% # Calculate RELATIVE increase
  ungroup()

## Remove trees with negative growth in height or dbh
measured.trees$measured.height[measured.trees$measured.height.inc < 0] <- NA
measured.trees$measured.height.inc[measured.trees$measured.height.inc < 0] <- NA

measured.trees$measured.dbh[measured.trees$measured.dbh.inc < 0] <- NA
measured.trees$measured.dbh.inc[measured.trees$measured.dbh.inc < 0] <- NA

##### DBH-EVEN SPACING COMPARISON PLOTS #####
# all.spacing.data <- measured.trees %>%
#   group_by(plot, year) %>%
#   summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
#             N            = n()) %>%
#   mutate(even.spacing = 0) %>%
#   ungroup()
#
# spacing.data <- measured.trees %>%
#   group_by(plot, year, even.spacing) %>%
#   summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
#             N            = n()) %>%
#   bind_rows(all.spacing.data) %>%
#   filter(even.spacing > 0) %>%
#   ungroup()
#
# even.spacing.plot <- ggplot(all.spacing.data, aes(x = year, y = measured.dbh)) +
#   labs(x       = "Year",
#        y       = "DBH (m)",
#        title   = "Growth x Even Spacing",
#        color   = "Spacing") +
#   facet_wrap(~plot) +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_line(color = "black", size = 1.5, na.rm = TRUE) +
#   geom_line(data = spacing.data, aes(color = factor(even.spacing)), na.rm = TRUE) +
#   scale_color_manual(values = cbPalette) +
#   theme_hisafe_ts() +
#   theme(plot.title = element_text(hjust = 0.5))
#
# ggsave_fitmax("./output/DBH_Even_Spacing_timeseries.jpg", even.spacing.plot)
#
# even.spacing.n.plot <- ggplot(all.spacing.data, aes(x = year, y = N)) +
#   labs(x       = "Year",
#        y       = "N",
#        title   = "Growth x Even Spacing",
#        color   = "Spacing") +
#   facet_wrap(~plot) +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_line(color = "black", size = 1.5, na.rm = TRUE) +
#   geom_line(data = spacing.data, aes(color = factor(even.spacing)), na.rm = TRUE) +
#   scale_color_manual(values = cbPalette) +
#   theme_hisafe_ts() +
#   theme(plot.title = element_text(hjust = 0.5))
#
# ggsave_fitmax("./output/DBH_Even_Spacing_N_timeseries.jpg", even.spacing.n.plot)

##### DBH-MEAN SPACING COMPARISON PLOTS #####
# all.spacing.data <- measured.trees %>%
#   group_by(plot, year) %>%
#   summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
#             N            = n()) %>%
#   mutate(mean.spacing = 0) %>%
#   ungroup()
#
# spacing.data <- measured.trees %>%
#   group_by(plot, year, mean.spacing) %>%
#   summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
#             N            = n()) %>%
#   bind_rows(all.spacing.data) %>%
#   filter(mean.spacing > 0) %>%
#   ungroup()
#
# mean.spacing.plot <- ggplot(all.spacing.data, aes(x = year, y = measured.dbh)) +
#   labs(x       = "Year",
#        y       = "DBH (m)",
#        title   = "Growth x Mean Spacing",
#        color   = "Spacing") +
#   facet_wrap(~plot) +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_line(color = "black", size = 1.5, na.rm = TRUE) +
#   geom_line(data = spacing.data, aes(color = factor(mean.spacing)), na.rm = TRUE) +
#   scale_color_manual(values = cbPalette) +
#   theme_hisafe_ts() +
#   theme(plot.title = element_text(hjust = 0.5))
#
# ggsave_fitmax("./output/DBH_Mean_Spacing_timeseries.jpg", mean.spacing.plot)
#
# mean.spacing.n.plot <- ggplot(all.spacing.data, aes(x = year, y = N)) +
#   labs(x       = "Year",
#        y       = "N",
#        title   = "Growth x Mean Spacing",
#        color   = "Spacing") +
#   facet_wrap(~plot) +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#   geom_line(color = "black", size = 1.5, na.rm = TRUE) +
#   geom_line(data = spacing.data, aes(color = factor(mean.spacing)), na.rm = TRUE) +
#   scale_color_manual(values = cbPalette) +
#   theme_hisafe_ts() +
#   theme(plot.title = element_text(hjust = 0.5))
#
# ggsave_fitmax("./output/DBH_Mean_Spacing_N_timeseries.jpg", mean.spacing.n.plot)

##### FILTER FOR TREES OF PROPER, EVEN SPACING #####
# measured.trees <- measured.trees %>%
#   filter(!(plot == "Restinclieres-A2" & even.spacing != 1 & year <  2004)) %>%
#   filter(!(plot == "Restinclieres-A2" & even.spacing != 2 & year >= 2004)) %>%
#   filter(!(plot == "Restinclieres-A3" & even.spacing != 1 & year <  2004)) %>%
#   filter(!(plot == "Restinclieres-A3" & even.spacing != 2 & year >= 2004)) %>%
#   filter(!(plot == "Restinclieres-A4" & even.spacing != 1)) %>%
#   filter(!(plot == "Castries"         & even.spacing != 1))
write_csv(measured.trees, paste0(data.path, "tree_biometrics_PROCESSED.csv"))

##### PLOT MAPS #####
nums <- as.character(1:26)
names(nums) <- LETTERS

map.data <- measured.trees.all %>%
  mutate(row.id  = as.numeric(str_replace_all(row.id, nums)))
map.data$row.id[map.data$plot  == "Restinclieres-A3"] <- -1 * map.data$row.id[map.data$plot  == "Restinclieres-A3"]

piezos <- read_csv(paste0(input.path, "restinclieres_piezometer_info.csv"), col_types = cols()) %>%
  filter(rowtree.id != "station") %>%
  mutate(plot = paste0("Restinclieres-", plot)) %>%
  mutate(row.id  = purrr::map_chr(str_split(rowtree.id, "-"), 1)) %>%
  mutate(tree.id = as.numeric(purrr::map_chr(str_split(rowtree.id, "-"), 2))) %>%
  mutate(row.id  = as.numeric(str_replace_all(row.id, nums)))
piezos$row.id[piezos$plot  == "Restinclieres-A3"] <- -1 * piezos$row.id[piezos$plot  == "Restinclieres-A3"]

for(y in c(2000, 2004)) { # 2017
  for(p in unique(map.data$plot)) {
    map.plot.data <- map.data %>%
      filter(plot == p) %>%
      filter(!is.na(measured.dbh)) %>%
      filter(year == y) %>% # min(.$year)
      mutate(unique.id = paste(plot, year, rowtree.id, sep = "-")) %>%
      mutate(used = unique.id %in% paste(measured.trees$plot, measured.trees$year, measured.trees$rowtree.id, sep = "-"))

    map.pizeo.data <- piezos %>%
      filter(plot == p)

    dbh.map <- ggplot(map.plot.data,
                      aes(x = as.numeric(row.id),
                          y = -tree.id)) +
      labs(title  = p) +
      guides(size = FALSE, color = FALSE) +
      geom_point(na.rm = TRUE, aes(size = measured.dbh, color = used)) +
      theme_void() +
      theme(aspect.ratio = 2,
            plot.title   = element_text(hjust = 0.5, size = 30))
    if(p != "Castries") dbh.map <- dbh.map + geom_point(data = map.pizeo.data, shape = 4)
    ggsave_fitmax(paste0("./output/DBH_Map_", p, "-", y, ".jpg"), dbh.map, scale = 1)
  }
}

##### PREP MEASURED DATA FOR CALIBRATION/VALIDATION #####
## For calibration, use the MEAN measured value of all trees rather than the MEDIAN.
## They are both extremely similar, except for a few times when MEDIAN has weird jumps/dips.
## To calculate INCREMENTS, take the DIFFERENCE OF THE MEANS, rather than the MEAN OF THE DIFFERENCES.
## This makes the measured vs. modeled look so much better!

cal.measured.trees <- measured.trees %>%
  filter(plot %in% CALIBRATION.SIMULATIONS) %>%
  mutate(plot = as.character(factor(plot, levels = paste0("Restinclieres-A", 2:4), labels = paste0("A", 2:4))))

cal.measured.annual <- cal.measured.trees %>%
  group_by(plot, year) %>%
  summarize(measured.dbh           = mean(measured.dbh,           na.rm = TRUE),
            measured.dbh.inc.MD    = mean(measured.dbh.inc,       na.rm = TRUE),
            #measured.dbh.inc.sd    = sd(measured.dbh.inc,           na.rm = TRUE),
            measured.height        = mean(measured.height,        na.rm = TRUE),
            #measured.height.inc.sd = sd(measured.height.inc,        na.rm = TRUE),
            measured.pruned.height = mean(measured.pruned.height, na.rm = TRUE)) %>%
  # mutate(measured.dbh.inc.DM = c(NA, diff(measured.dbh)),              #mean(measured.dbh.inc,    na.rm = TRUE), # ABSOLUTE DIFFERENCES
  #        measured.height.inc = c(NA, diff(measured.height)) * 100) %>% #mean(measured.height.inc, na.rm = TRUE), # ABSOLUTE DIFFERENCES
  mutate(measured.dbh.inc.DM = c(NA, reldiff(measured.dbh)),              # RELATIVE DIFFERENCES
         measured.height.inc = c(NA, reldiff(measured.height)) * 100) %>% # RELATIVE DIFFERENCES
  mutate(measured.dbh.inc = measured.dbh.inc.DM) %>%
  ungroup()

cal.measured.annual[is.na(cal.measured.annual)] <- NA

val.measured.trees <- measured.trees %>%
   filter(plot %in% VALIDATION.SIMULATIONS)

val.measured.annual <- val.measured.trees %>%
  group_by(plot, year) %>%
  summarize(measured.dbh           = mean(measured.dbh,           na.rm = TRUE),
            measured.dbh.inc.MD    = mean(measured.dbh.inc,       na.rm = TRUE),
            #measured.dbh.inc.sd    = sd(measured.dbh.inc,           na.rm = TRUE),
            measured.height        = mean(measured.height,        na.rm = TRUE),
            #measured.height.inc.sd = sd(measured.height.inc,        na.rm = TRUE),
            measured.pruned.height = mean(measured.pruned.height, na.rm = TRUE)) %>%
  # mutate(measured.dbh.inc.DM = c(NA, diff(measured.dbh)),              #mean(measured.dbh.inc,    na.rm = TRUE), # ABSOLUTE DIFFERENCES
  #        measured.height.inc = c(NA, diff(measured.height)) * 100) %>% #mean(measured.height.inc, na.rm = TRUE), # ABSOLUTE DIFFERENCES
  mutate(measured.dbh.inc.DM = c(NA, reldiff(measured.dbh)),              # RELATIVE DIFFERENCES
         measured.height.inc = c(NA, reldiff(measured.height)) * 100) %>% # RELATIVE DIFFERENCES
  mutate(measured.dbh.inc = measured.dbh.inc.DM) %>%
  ungroup()

val.measured.annual[is.na(val.measured.annual)] <- NA

##### MEASURED CROP YIELD #####
## raw units are hundredweight/ha
CALIBRATION.CROPS <- "durum wheat"#c("durum wheat", "protein pea")
measured.yield.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_crop_yield.csv"), stringsAsFactors = FALSE))
measured.yield <- measured.yield.raw %>%
  filter(crop %in% CALIBRATION.CROPS) %>%
  filter(year > 2000) %>% # don't have year-specific management data before this, even though have year-specific yield data
  filter(year > 2003) %>% # wheat varieties that are not Claudio and for which we don't have solid parameterization
  filter(!(year %in% c(2002, 2007))) %>% # these are the 2 wheat years directly after rape. Yields are abnormally high, likely due to some biotic interaction with rape that is not adequately represented in STICS. We don't have any data on rape biomass/yield to compare with the model output.
  mutate(measured.yield    = measured.yield    / 10,
         measured.yield.sd = measured.yield.sd / 10) %>%
  select(plot, year, crop, location, everything(), -System)
write_csv(measured.yield, paste0(data.path, "restinclieres_crop_yield_PROCESSED.csv"))

##### MVM ANNOTATION FUNCTION #####
mvm_annotation = function(m, o) {
  rmse <- round(sqrt(mean((m - o)^2, na.rm = TRUE)), 2)

  pcc     <- cor.test(m, o, method = "pearson")
  pcc.est <- round(pcc$estimate, 2)
  pcc.p   <- round(pcc$p.val, 2)

  srcc     <- cor.test(m, o, method = "spearman", exact = FALSE)
  srcc.est <- round(srcc$estimate, 2)
  srcc.p   <- round(srcc$p.val, 2)

  label <- paste0("RMSE = ",   rmse,
                  "\nPCC = ",  pcc.est,  ", p = ", pcc.p,
                  "\nSRCC = ", srcc.est, ", p = ", srcc.p)
  return(label)
}



##### CALCULATE MEASURED RY FOR GA #####
# yield <- measured.yield %>%
#   mutate(group = purrr::map_chr(strsplit(plot, "-"),2)) %>%
#   filter(group == "A2") %>%
#   select(group, location, year, measured.yield)
#
# AF.yield <- yield %>%
#   filter(location != "Monocrop") %>%
#   rename(measured.yield.AF = measured.yield)
#
# CC.yield <- yield %>%
#   ungroup() %>%
#   filter(location == "Monocrop") %>%
#   select(-location) %>%
#   rename(measured.yield.CC = measured.yield)
#
# rel.yield <- AF.yield %>%
#   left_join(CC.yield, by = c("group", "year")) %>%
#   mutate(measured.ry = measured.yield.AF / measured.yield.CC) %>%
#   rename(plot = group) %>%
#   select(-measured.yield.AF, -measured.yield.CC)
#
# ggplot(rel.yield, aes(x = year, y = measured.ry, color = location)) + geom_point()
#
# write_csv(rel.yield, "./GA1/input/MEASURED_RY.csv")
