### hisafe-calibration
### FIELD DATA
### Author: Kevin J. Wolz

mean_radius <- function(a, b) {
  out <- data.frame(a = a, b = b)
  out <- rowMeans(out, na.rm = TRUE)
  out[is.nan(out)] <- NA
  return(out)
}

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
  mutate(date = ymd(paste0(year, "-12-15")))

## Remove individual tree-year points that are sharp jumps or negative growth
bad.dbh    <-  read_csv(paste0(input.path, "Bad_Tree_DBH_data.csv"),    col_types = cols()) %>%
  mutate(bad.check = paste0(plot, rowtree.id, year, sep = "-"))
bad.height <-  read_csv(paste0(input.path, "Bad_Tree_Height_data.csv"), col_types = cols()) %>%
  mutate(bad.check = paste0(plot, rowtree.id, year, sep = "-"))
restinclieres.trees <- restinclieres.trees %>%
  mutate(bad.check = paste0(plot, rowtree.id, year, sep = "-"))

#restinclieres.trees$measured.dbh[restinclieres.trees$bad.check    %in% bad.dbh$bad.check]    <- NA
#restinclieres.trees$measured.height[restinclieres.trees$bad.check %in% bad.height$bad.check] <- NA

restinclieres.trees <- select(restinclieres.trees, -bad.check)

A2.LIVING.TREES <- restinclieres.trees %>%
  filter(plot == "Restinclieres-A2") %>%
  filter(year == 2014) %>%
  filter(!is.na(measured.dbh)) %>%
  .$rowtree.id
A3.LIVING.TREES <- restinclieres.trees %>%
  filter(plot == "Restinclieres-A3") %>%
  filter(year == 2014) %>%
  filter(!is.na(measured.dbh)) %>%
  .$rowtree.id
A4.LIVING.TREES <- restinclieres.trees %>%
  filter(plot == "Restinclieres-A4") %>%
  filter(year == 2014) %>%
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
  group_by(plot, year, row.id) %>%
  arrange(plot, year, row.id, tree.id) %>%
  mutate(spacing.1 = c(NA, diff(tree.id))) %>%
  mutate(spacing.2 = c(diff(tree.id), NA)) %>%
  ungroup()

measured.trees.all$spacing.1[measured.trees.all$spacing.1 < 1] <- NA
measured.trees.all$spacing.1[measured.trees.all$spacing.1 > 6] <- NA
measured.trees.all$spacing.2[measured.trees.all$spacing.2 < 1]<- NA
measured.trees.all$spacing.2[measured.trees.all$spacing.2 > 6] <- NA

measured.trees.all <- measured.trees.all %>%
  mutate(mean.spacing = (spacing.1 + spacing.2) / 2) %>%
  mutate(even.spacing = as.numeric(spacing.1 == spacing.2) * spacing.1)

## Filtered data for calibration/validation
measured.trees <- measured.trees.all %>%
  mutate(measured.dbh = measured.dbh * 100) %>% # Convert m to cm for calibration/validation
  ## Filter A2 Trees
  filter(!(plot == "Restinclieres-A2" & rowtree.id   %in% A2.EDGE.TREES)) %>%      # Don't use edge trees
  filter(!(plot == "Restinclieres-A2" & row.id       %in% c("D", "G"))) %>%        # Don't use rows D&G because after 2013 they were adjacent to pollarded E&F
  filter(!(plot == "Restinclieres-A2" & row.id       %in% c("C", "G"))) %>%        # Don't use rows C&G because after ~2004 they were pruned high
  filter(!(plot == "Restinclieres-A2" & !(rowtree.id %in% A2.LIVING.TREES))) %>%   # Use only trees still living today
  ## Filter A3 Trees
  filter(!(plot == "Restinclieres-A3" & rowtree.id   %in% A3.EDGE.TREES)) %>%      # Don't use edge trees
  filter(!(plot == "Restinclieres-A3" & !(rowtree.id %in% A3.LIVING.TREES))) %>%   # Use only trees still living today
  ## Filter A4 Trees
  filter(!(plot == "Restinclieres-A4" & !(rowtree.id %in% A4.GOOD.TREES))) %>%     # Use specific trees that are not on edges and were not fertilized
  filter(!(plot == "Restinclieres-A4" & !(rowtree.id %in% A4.LIVING.TREES))) %>%   # Use only trees still living today
  ## Filter Castries Trees
  filter(!(plot == "Castries"         & rowtree.id   %in% CASTRIE.EDGE.TREES)) %>% # Don't use edge trees
  filter(!(plot == "Castries"         & alley.crop   %in% c("L", "FL")))           # Only use trees that had Fescue as alley crop

##### DBH-EVEN SPACING COMPARISON PLOTS #####
all.spacing.data <- measured.trees %>%
  group_by(plot, year) %>%
  summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
            N            = n()) %>%
  mutate(even.spacing = 0) %>%
  ungroup()

spacing.data <- measured.trees %>%
  group_by(plot, year, even.spacing) %>%
  summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
            N            = n()) %>%
  bind_rows(all.spacing.data) %>%
  filter(even.spacing > 0) %>%
  ungroup()

even.spacing.plot <- ggplot(all.spacing.data, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (m)",
       title   = "Growth x Even Spacing",
       color   = "Spacing") +
  facet_wrap(~plot) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(color = "black", size = 1.5, na.rm = TRUE) +
  geom_line(data = spacing.data, aes(color = factor(even.spacing)), na.rm = TRUE) +
  scale_color_manual(values = cbPalette) +
  theme_hisafe_ts() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave_fitmax("./output/DBH_Even_Spacing_timeseries.jpg", even.spacing.plot)

even.spacing.n.plot <- ggplot(all.spacing.data, aes(x = year, y = N)) +
  labs(x       = "Year",
       y       = "N",
       title   = "Growth x Even Spacing",
       color   = "Spacing") +
  facet_wrap(~plot) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(color = "black", size = 1.5, na.rm = TRUE) +
  geom_line(data = spacing.data, aes(color = factor(even.spacing)), na.rm = TRUE) +
  scale_color_manual(values = cbPalette) +
  theme_hisafe_ts() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave_fitmax("./output/DBH_Even_Spacing_N_timeseries.jpg", even.spacing.n.plot)

##### DBH-MEAN SPACING COMPARISON PLOTS #####
all.spacing.data <- measured.trees %>%
  group_by(plot, year) %>%
  summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
            N            = n()) %>%
  mutate(mean.spacing = 0) %>%
  ungroup()

spacing.data <- measured.trees %>%
  group_by(plot, year, mean.spacing) %>%
  summarize(measured.dbh = mean(measured.dbh, na.rm = TRUE),
            N            = n()) %>%
  bind_rows(all.spacing.data) %>%
  filter(mean.spacing > 0) %>%
  ungroup()

mean.spacing.plot <- ggplot(all.spacing.data, aes(x = year, y = measured.dbh)) +
  labs(x       = "Year",
       y       = "DBH (m)",
       title   = "Growth x Mean Spacing",
       color   = "Spacing") +
  facet_wrap(~plot) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(color = "black", size = 1.5, na.rm = TRUE) +
  geom_line(data = spacing.data, aes(color = factor(mean.spacing)), na.rm = TRUE) +
  scale_color_manual(values = cbPalette) +
  theme_hisafe_ts() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave_fitmax("./output/DBH_Mean_Spacing_timeseries.jpg", mean.spacing.plot)

mean.spacing.n.plot <- ggplot(all.spacing.data, aes(x = year, y = N)) +
  labs(x       = "Year",
       y       = "N",
       title   = "Growth x Mean Spacing",
       color   = "Spacing") +
  facet_wrap(~plot) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(color = "black", size = 1.5, na.rm = TRUE) +
  geom_line(data = spacing.data, aes(color = factor(mean.spacing)), na.rm = TRUE) +
  scale_color_manual(values = cbPalette) +
  theme_hisafe_ts() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave_fitmax("./output/DBH_Mean_Spacing_N_timeseries.jpg", mean.spacing.n.plot)

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
map.data <- measured.trees.all
nums <- as.character(1:26)
names(nums) <- LETTERS
map.data$row.id <- str_replace_all(map.data$row.id, nums)
map.data$row.id <- as.numeric(map.data$row.id)
map.data$row.id[map.data$plot  == "Restinclieres-A3"] <- -1 * map.data$row.id[map.data$plot  == "Restinclieres-A3"]

piezos <- read_csv(paste0(input.path, "restinclieres_piezometer_locations.csv"), col_types = cols())
piezos$row.id <- str_replace_all(piezos$row.id, nums)
piezos$row.id <- as.numeric(piezos$row.id)
piezos$row.id[piezos$plot  == "Restinclieres-A3"] <- -1 * piezos$row.id[piezos$plot  == "Restinclieres-A3"]

for(y in c(2000)) { # 2017
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
      geom_point(data = map.pizeo.data, shape = 4) +
      theme_void() +
      theme(aspect.ratio = 2,
            plot.title   = element_text(hjust = 0.5, size = 30))
    ggsave_fitmax(paste0("./output/DBH_Map_", p, "-", y, ".jpg"), dbh.map, scale = 1)
  }
}

##### MEASURED CROP YIELD #####
## raw units are hundredweight/ha
measured.yield.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_crop_yield.csv"), stringsAsFactors = FALSE))
measured.yield <- measured.yield.raw %>%
  filter(crop == "durum wheat") %>%
  mutate(year = as.numeric(year)) %>%
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

##### PREP MEASURED DATA FOR CALIBRATION #####
cal.measured.trees <- measured.trees %>%
  filter(plot %in% CALIBRATION.SIMUATIONS) %>%
  mutate(plot = as.character(factor(plot, levels = paste0("Restinclieres-A", 2:4), labels = paste0("A", 2:4)))) %>%
  group_by(plot, rowtree.id) %>%
  arrange(plot, rowtree.id, date) %>%
  mutate(measured.dbh.inc    = c(NA, diff(measured.dbh))) %>%
  mutate(measured.height.inc = c(NA, diff(measured.height)) * 100) %>%
  ungroup()

## Remove trees with negative growth in height or dbh
cal.measured.trees$measured.height[cal.measured.trees$measured.height.inc < 0] <- NA
cal.measured.trees$measured.height.inc[cal.measured.trees$measured.height.inc < 0] <- NA

cal.measured.trees$measured.dbh[cal.measured.trees$measured.dbh.inc < 0] <- NA
cal.measured.trees$measured.dbh.inc[cal.measured.trees$measured.dbh.inc < 0] <- NA

cal.measured.annual <- cal.measured.trees %>%
  group_by(plot, year) %>%
  summarize(measured.dbh           = mean(measured.dbh,           na.rm = TRUE),
            measured.dbh.inc.sd    = sd(measured.dbh.inc,         na.rm = TRUE),
            measured.dbh.inc       = mean(measured.dbh.inc,       na.rm = TRUE),
            measured.height        = mean(measured.height,        na.rm = TRUE),
            measured.height.inc.sd = sd(measured.height.inc,      na.rm = TRUE),
            measured.height.inc    = mean(measured.height.inc,    na.rm = TRUE),
            measured.pruned.height = mean(measured.pruned.height, na.rm = TRUE)) %>%
  ungroup()
