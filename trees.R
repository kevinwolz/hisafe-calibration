### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

## MODELED TREE BIOMETRICS
modeled.trees <- hop$trees %>%
  filter(SimulationName %in% c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4")) %>%
  rename(plot = SimulationName) %>%
  rename(date = Date) %>%
  rename(modeled.dbh = dbh) %>%
  mutate(modeled.pruned.height = crownBaseHeight * 100) %>%
  mutate(modeled.height = height * 100) %>%
  select(plot, date, Year, Month, Day, modeled.dbh, modeled.height, modeled.pruned.height)

measured.single <- measured.trees %>%
  group_by(plot, year) %>%
  summarize(measured.dbh           = mean(measured.dbh,           na.rm = TRUE),
            measured.pruned.height = mean(measured.pruned.height, na.rm = TRUE),
            measured.height        = mean(measured.height,        na.rm = TRUE))

modeled.single <- modeled.trees %>%
  rename(year = Year) %>%
  filter(Month == 12, Day == 15) %>%
  select(-date, -Month, -Day)

single <- modeled.single %>%
  left_join(measured.single, by = c("plot", "year"))


## MEASURED vs. MODELED TIMESERIES
vars <- c("dbh", "pruned.height", "height")
labs <- c("DBH", "Pruned height", "Tree height")

for(i in vars){
  #mvm <- mvm_annotation(single[[paste0("modeled.", i)]], single[[paste0("measured.", i)]])
  #grob <- grobTree(textGrob(mvm, x = 0.05, y = 0.95, hjust = 0, vjust = 1))

  ts.plot <- ggplot(measured.trees, aes_string(x = "date", y = paste0("measured.", i))) +
    labs(x = "Year",
         y = paste(labs[match(i, vars)], "(cm)"),
         title = "Hi-sAFe Calibration",
         caption = "Measured: Boxplots\nModeled: Line") +
    facet_wrap(~plot, nrow = 1) +
    geom_boxplot(aes(group = year), color = "grey50", na.rm = TRUE, outlier.shape = NA) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(data = modeled.trees, aes_string(y = paste0("modeled.", i)), color = "black", size = 1) +
    #annotation_custom(grob) +
    theme_hisafe_ts() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_", gsub("\\.", "_", i), "_timeseries.jpg"), ts.plot, scale = 1.5)
}

## MEASURED vs. MODELED INCREMENT SCATTERPLOT
vars <- c("dbh", "height")
labs <- c("DBH increment", "tree height increment")

get_increment <- function(x, df) {
  df <- subset(df, plot == x)
  increment <- tibble(year            = df$year[2:nrow(df)],
                      plot            = x,
                      modeled.dbh     = diff(df$modeled.dbh),
                      modeled.height  = diff(df$modeled.height),
                      measured.dbh    = diff(df$measured.dbh),
                      measured.height = diff(df$measured.height))
  return(increment)
}
increment.trees <- purrr::map_dfr(unique(single$plot), get_increment, df = single)

for(i in vars){
  if(i == "dbh") { LIMITS <- c(0,2.5) } else { LIMITS <- c(0,75) }
  #mvm <- mvm_annotation(increment.trees[[paste0("modeled.", i)]], increment.trees[[paste0("measured.", i)]])
  #grob <- grobTree(textGrob(mvm, x = 0.05, y = 0.95, hjust = 0, vjust = 1))

  mvm.inc.plot <- ggplot(increment.trees, aes_string(x = paste0("modeled.", i), y = paste0("measured.", i))) +
    labs(x = paste("Modeled", labs[match(i, vars)], "(cm)"),
         y = paste("Measured", labs[match(i, vars)], "(cm)"),
         title = "Hi-sAFe Calibration") +
    facet_wrap(~plot, nrow = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_errorbar(aes(ymin = measured.yield - measured.yield.sd,
    #                  ymax = measured.yield + measured.yield.sd), na.rm = TRUE) +
    geom_point(aes(fill = year), shape = 21, na.rm = TRUE) +
    scale_fill_viridis(option = "magma") +
    scale_x_continuous(limits = LIMITS) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
    #annotation_custom(grob) +
    coord_equal() +
    theme_hisafe_ts() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_", gsub("\\.", "_", i), "_increment_scatterplot.jpg"), mvm.inc.plot, scale = 1.7)
}

## MEASURED vs. MODELED INCREMENT SCATTERPLOT
vars <- c("dbh", "height")
labs <- c("DBH increment", "tree height increment")

for(i in vars){
  ts.inc.plot <- ggplot(increment.trees, aes(x = year)) +
    labs(x = "Year",
         y = paste(labs[match(i, vars)], "(cm)"),
         title = "Hi-sAFe Calibration",
         caption = "Measured: Points\nModeled: Line") +
    facet_wrap(~plot, nrow = 1) +
    geom_line(aes_string(y = paste0("modeled.", i)), na.rm = TRUE) +
    geom_point(aes_string(y = paste0("measured.", i)), na.rm = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    coord_equal() +
    theme_hisafe_ts() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave_fitmax(paste0(PATH, "analysis/calibration/hisafe_calibration_", gsub("\\.", "_", i), "_increment_timeseries.jpg"), ts.inc.plot, scale = 1.7)
}