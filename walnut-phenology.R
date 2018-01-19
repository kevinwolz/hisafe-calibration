### hisafe-calibration
### WALNUT PHENOLOGY PARAMETERIZATION
### Author: Kevin J. Wolz

pheno.data <- as.tibble(read.csv(paste0(input.path, "restinclieres_tree_phenology.csv"), stringsAsFactors = FALSE)) %>%
  group_by(year, month, day, plot) %>%
  summarize(score = mean(score, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day, sep = "-")))

ggplot(pheno.data, aes(x = date, y = score, color = plot)) +
  facet_wrap(~year, scales = "free_x") +
  geom_line() +
  theme_hisafe_ts()
