### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

## MODELED TREE BIOMETRICS
modeled.trees <- face$trees %>%
  filter(System == "Agroforestry") %>%
  filter(SimulationName == MODELED.SITE) %>%
  #filter(Month == 12) %>%
  #filter(Day == 15) %>%
  #mutate(Year = Year - 1) %>% # the Year in Hi-sAFe annual export data is the YEAR AFTER MEASUREMENT
  #rename(year = Year) %>%
  rename(date = Date) %>%
  rename(modeled.dbh = dbh) %>%
  mutate(modeled.pruned.height = crownBaseHeight * 100) %>%
  mutate(modeled.height = height * 100) %>%
  select(date, modeled.dbh, modeled.height, modeled.pruned.height)

## MEASURED vs. MODELED TIMESERIES
vars <- c("dbh", "pruned.height", "height")
labs <- c("DBH", "Pruned height", "Tree height")

for(i in vars){
  tree.plot <- ggplot(measured.trees, aes_string(x = "date", y = paste0("measured.", i))) +
    labs(x = "Year", y = paste(labs[match(i, vars)], "(cm)")) +
    geom_boxplot(aes(group = year), color = "black", na.rm = TRUE, width = 200) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    #scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(1994,2017), breaks = seq(1995, 2015, 5)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(data = modeled.trees, aes_string(y = paste0("modeled.", i)), color = "red") +
    theme_ggEHD()

  ggsave_fitmax(paste0(PATH, "analysis/", FIELD.SITE, "_", gsub("\\.", "_", i), ".jpg"), tree.plot)
}

## MEASURED VS. MODELED SCATTERPLOTS
# tree.comp <- measured.trees %>%
#   group_by(year) %>%
#   summarize(measured.DBH.sd = sd(measured.DBH, na.rm = TRUE),
#             measured.DBH = mean(measured.DBH, na.rm = TRUE),
#             measured.pruned.height.sd = sd(measured.pruned.height, na.rm = TRUE),
#             measured.pruned.height = mean(measured.pruned.height, na.rm = TRUE),
#             measured.tree.height.sd = sd(measured.tree.height, na.rm = TRUE),
#             measured.tree.height = mean(measured.tree.height, na.rm = TRUE)) %>%
#   left_join(modeled.trees, by = "year")
