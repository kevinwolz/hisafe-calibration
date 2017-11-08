### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

## MODELED DATA
hisafe.trees <- hop$annual

## FIELD DATA
trees.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_tree_biometrics.csv"), stringsAsFactors = FALSE))
trees <- trees.raw %>%
  filter(!is.na(value),
         !(variable %in% c("diametre_a_la_base_du_tronc", "circonference_tronc_a_130cm")),
         value > 0) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  mutate(tree = factor(tree)) %>%
  mutate(variable = factor(variable, labels = c("DBH", "Pruned height", "Tree height"))) %>%
  select(year, date, everything(), -unit)

write_csv(trees, paste0(data.path, "restinclieres_tree_biometrics_PROCESSED.csv"))

## FIELD DATA PLOTS
for(i in unique(trees$variable)){
  tree.plot <- ggplot(subset(trees, variable == i), aes(x = year, y = value)) +
    labs(x = "Years after establishment", y = paste(i, "(cm)")) +
    geom_boxplot(aes(group = year), color = "black") +
    scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(1994,2017), breaks = seq(1995, 2015, 5)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    theme_ggEHD()

  ggsave_fitmax(paste0(plot.path, NAME, "_", i, ".jpg"), tree.plot)
}

## CALIBRATION PLOTS
