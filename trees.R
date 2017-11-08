### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

trees.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_tree_biometrics.csv"), stringsAsFactors = FALSE))

trees <- trees.raw %>%
  filter(!is.na(value), variable != "diametre_a_la_base_du_tronc", value > 0) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  mutate(tree = factor(tree)) %>%
  mutate(variable = factor(variable, labels = c("cbh", "dbh", "pruned_height", "tree_height"))) %>%
  select(year, date, everything(), -unit)

#table(trees$year, trees$variable)

ylab <- function(x) {
  x <- gsub("_", " ", tolower(x))
  if(grepl(" ", x)){
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  } else(
    x <- toupper(x)
  )
  x
}

for(i in unique(trees$variable)){
  tree.plot <- ggplot(subset(trees, variable == i), aes(x = year, y = value)) +
    labs(x = "Years after establishment", y = paste(ylab(i), "(cm)")) +
    geom_boxplot(aes(group = year), color = "black") +
    scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(1994,2017), breaks = seq(1995, 2015, 5)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    theme_ggEHD()

  ggsave_fitmax(paste0(plot.path, "restinclieres_A2_", i, ".jpg"), tree.plot)
}

write_csv(trees, paste0(data.path, "restinclieres_tree_biometrics_PROCESSED.csv"))
