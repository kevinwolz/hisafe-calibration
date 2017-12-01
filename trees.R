### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

## MODELED TREE BIOMETRICS
modeled.trees <- face$trees %>%
  filter(System == "Agroforestry") %>%
  filter(SimulationName == MODELED.SITE) %>%
  rename(date = Date) %>%
  rename(modeled.dbh = dbh) %>%
  mutate(modeled.pruned.height = crownBaseHeight * 100) %>%
  mutate(modeled.height = height * 100) %>%
  select(date, Year, Month, Day, modeled.dbh, modeled.height, modeled.pruned.height)

measured.single <- measured.trees %>%
  group_by(year) %>%
  summarize(measured.dbh = round(mean(measured.dbh, na.rm = TRUE)/100, 1),
            measured.pruned.height = round(mean(measured.pruned.height, na.rm = TRUE)/100, 1),
            measured.height = round(mean(measured.height, na.rm = TRUE)/100, 1))

modeled.single <- modeled.trees %>%
  rename(year = Year) %>%
  filter(Month == 12, Day == 15) %>%
  select(-date, -Month, -Day)

single <- modeled.single %>%
  left_join(measured.single, by = "year")


## MEASURED vs. MODELED TIMESERIES
vars <- c("dbh", "pruned.height", "height")
labs <- c("DBH", "Pruned height", "Tree height")

for(i in vars){
  mvm <- mvm_annotation(single[[paste0("modeled.", i)]], single[[paste0("measured.", i)]])
  grob <- grobTree(textGrob(mvm, x = 0.05, y = 0.95, hjust = 0, vjust = 1))

  tree.plot <- ggplot(measured.trees, aes_string(x = "date", y = paste0("measured.", i))) +
    labs(x = "Year",
         y = paste(labs[match(i, vars)], "(cm)"),
         title = paste("Hi-sAFe Calibration:", MODELED.SITE),
         caption = "Measured: Boxplots\nModeled: Line") +
    geom_boxplot(aes(group = year), color = "grey50", na.rm = TRUE, width = 200) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(data = modeled.trees, aes_string(y = paste0("modeled.", i)), color = "black", size = 1) +
    annotation_custom(grob) +
    theme_ggEHD() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_", gsub("\\.", "_", i), ".jpg"), tree.plot)
}
