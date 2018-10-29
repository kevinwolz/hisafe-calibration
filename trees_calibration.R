### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

for(STEP in STEPS) {

  if(STEP == "calibration") {
    SIMS <- CALIBRATION.SIMULATIONS
    NEW.SIM.NAMES <- purrr::map_chr(str_split(CALIBRATION.SIMULATIONS, "-"), 2)
    plot.measured.annual <- cal.measured.annual
    plot.measured.trees  <- cal.measured.trees
    DATE.MIN <- "1995-01-01"
  } else {
    SIMS <- VALIDATION.SIMULATIONS
    NEW.SIM.NAMES <- VALIDATION.SIMULATIONS
    plot.measured.annual <- val.measured.annual
    plot.measured.trees  <- val.measured.trees
    DATE.MIN <- "1991-01-01"
  }

  ##### PREP MODELED TREE BIOMETRICS #####
  modeled.trees <- hop$trees %>%
    filter(SimulationName %in% SIMS) %>%
    mutate(SimulationName        = as.character(factor(SimulationName, levels = SIMS, labels = NEW.SIM.NAMES))) %>%
    filter(idTree == 1) %>%
    filter(Date >= lubridate::ymd(DATE.MIN)) %>%
    rename(plot                  = SimulationName) %>%
    rename(date                  = Date) %>%
    rename(modeled.dbh           = dbh) %>%
    mutate(modeled.pruned.height = crownBaseHeight) %>%
    mutate(modeled.height        = height) %>%
    mutate(modeled.crown.radius.interrow = crownRadiusInterRow) %>%
    mutate(modeled.crown.radius.treeline = crownRadiusTreeLine) %>%
    select(plot, date, Year, Month, Day, modeled.dbh, modeled.height, modeled.pruned.height, modeled.crown.radius.interrow, modeled.crown.radius.treeline)

  modeled.annual <- modeled.trees %>%
    rename(year = Year) %>%
    filter(Month == 12, Day == 15) %>%
    select(-date, -Month, -Day) %>%
    arrange(year) %>%
    group_by(plot) %>%
    # mutate(modeled.dbh.inc    = c(NA, diff(modeled.dbh))) %>%          # ABSOLUTE INCREMENT
    # mutate(modeled.height.inc = c(NA, diff(modeled.height)) * 100) %>% # ABSOLUTE INCREMENT
    mutate(modeled.dbh.inc    = c(NA, reldiff(modeled.dbh))) %>%          # RELATIVE INCREMENT
    mutate(modeled.height.inc = c(NA, reldiff(modeled.height)) * 100) %>% # RELATIVE INCREMENT
    ungroup() %>%
    arrange(plot, year)

  annual <- modeled.annual %>%
    left_join(plot.measured.annual, by = c("plot", "year"))

  ##### MEASURED vs. MODELED TIMESERIES #####
  vars <- c("dbh", "pruned.height", "height")
  labs <- c("DBH (cm)", "Pruned height (m)", "Tree height (m)")

  for(i in vars){
    plot.annotation <- data.frame(plot = NEW.SIM.NAMES)
    plot.annotation$date <- min(plot.measured.trees$date, na.rm = TRUE)
    plot.annotation[[paste0("measured.", i)]] <- max(max(plot.measured.trees[[paste0("measured.", i)]], na.rm = TRUE),
                                                     max(modeled.trees[[paste0("modeled.", i)]], na.rm = TRUE))

    ts.plot <- ggplot(plot.measured.trees, aes_string(x = "date", y = paste0("measured.", i))) +
      labs(x       = "Year",
           caption = "Measured: Boxplots\nModeled: Line",
           y       = labs[match(i, vars)]) +
      facet_wrap(~plot) +
      geom_boxplot(aes(group = date), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
      stat_summary(fun.y = mean, color = "grey30", geom = "point", size = 1, na.rm = TRUE) +
      scale_x_date(date_breaks = "5 years", date_labels = "%Y", limits = c(lubridate::ymd("1995-01-01"), lubridate::ymd("2018-06-01"))) + # seq(lubridate::ymd("1995-01-01"), lubridate::ymd("2015-01-01"), "5 years")
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
      geom_line(data = modeled.trees, aes_string(y = paste0("modeled.", i)), color = "black", size = 0.75) +
      geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
      theme_hisafe_ts(strip.background = element_blank(),
                      strip.text       = element_blank(),
                      panel.grid       = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

    ggsave_fitmax(paste0(PATH, "analysis/", STEP, "/hisafe_", STEP, "_ts_", gsub("\\.", "_", i), ".png"), ts.plot, scale = 1.5)
    #ggsave_fitmax(paste0(PATH, "analysis/measured_trees_pruned_height.png"), ts.plot, scale = 1.5)
  }

  ##### MEASURED vs. MODELED INCREMENT SCATTERPLOT #####
  vars <- c("dbh.inc", "height.inc")
  labs <- c("DBH relative increment", "Height relative increment")

  for(i in vars){
    if(i == "dbh.inc") LIMITS <- c(0, 3) else LIMITS <- c(0, 150)
    mvm <- mvm_annotation(annual[[paste0("modeled.", i)]], annual[[paste0("measured.", i)]])
    #grob <- grobTree(textGrob(mvm, x = 1.05, y = 0.95, hjust = 0, vjust = 1))

    # plot.annotation <- data.frame(plot = paste0("A", 2:4))
    # plot.annotation[[paste0("modeled.",  i)]] <- LIMITS[1]
    # plot.annotation[[paste0("measured.", i)]] <- LIMITS[2]

    #annual$ub <- annual[[paste0("measured.", i)]] + annual[[paste0("measured.", i, ".sd")]]
    #annual$lb <- annual[[paste0("measured.", i)]] - annual[[paste0("measured.", i, ".sd")]]

    mvm.inc.plot <- ggplot(annual, aes_string(x = paste0("modeled.", i), y = paste0("measured.", i))) +
      labs(x    = paste("Modeled", labs[match(i, vars)]),
           caption = mvm,
           fill = NULL,
           y    = paste("Measured", labs[match(i, vars)])) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      # geom_errorbar(aes(ymin = lb,
      #                   ymax = ub),
      #               alpha = 0.5, na.rm = TRUE) +
      geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
      scale_fill_manual(values = c("white", "grey70", "black")) +
      scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) + #, limits = LIMITS
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) + #, limits = LIMITS
      #geom_text(data = plot.annotation, aes(label = label), hjust = 0, vjust = 1, size = 5) +
      #annotation_custom(grob) +
      coord_equal() +
      theme_hisafe_ts(strip.background = element_blank(),
                      strip.text       = element_blank(),
                      panel.grid       = element_blank())

    ggsave_fitmax(paste0(PATH, "analysis/", STEP, "/hisafe_", STEP, "_sp_", gsub("\\.", "_", i), ".png"), mvm.inc.plot, scale = 1.1)
  }

  ##### MEASURED vs. MODELED INCREMENT TIMESERIES #####
  vars <- c("dbh.inc", "height.inc")
  labs <- c("DBH relative increment", "tree height relative increment")

  for(i in vars){
    plot.annotation <- data.frame(plot = NEW.SIM.NAMES)
    plot.annotation$year <- min(annual$year, na.rm = TRUE)
    plot.annotation[[paste0("measured.", i)]] <- max(max(annual[[paste0("measured.", i)]], na.rm = TRUE),
                                                     max(annual[[paste0("modeled.", i)]],   na.rm = TRUE))

    #annual$ub <- annual[[paste0("measured.", i)]] + annual[[paste0("measured.", i, ".sd")]]
    #annual$lb <- annual[[paste0("measured.", i)]] - annual[[paste0("measured.", i, ".sd")]]

    ts.inc.plot <- ggplot(annual, aes(x = year)) +
      labs(x = "Year",
           #title = "Hi-sAFe Calibration",
           caption = "Measured: Points\nModeled: Line",
           y = labs[match(i, vars)]) +
      facet_wrap(~plot) +
      geom_point(aes_string(y = paste0("measured.", i)), na.rm = TRUE, color = "grey50") +
      geom_line(aes_string(y  = paste0("modeled.", i)),  na.rm = TRUE) +
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
      # geom_errorbar(aes(ymin = lb,
      #                   ymax = ub),
      #               color = "grey50", na.rm = TRUE) +
      geom_text(data = plot.annotation, aes_string(label = "plot", y = paste0("measured.", i)), hjust = 0, vjust = 1, size = 5) +
      coord_equal() +
      theme_hisafe_ts(strip.background = element_blank(),
                      strip.text       = element_blank(),
                      panel.grid       = element_blank())

    ggsave_fitmax(paste0(PATH, "analysis/", STEP, "/hisafe_", STEP, "_ts_", gsub("\\.", "_", i), ".png"), ts.inc.plot, scale = 1.7)
  }
}
