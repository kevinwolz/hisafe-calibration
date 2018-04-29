### hisafe-calibration
### TREES
### Author: Kevin J. Wolz

for(STEP in c("calibration", "validation")) {

  if(STEP == "calibration") {
    SIMS <- CALIBRATION.SIMUATIONS
    NEW.SIM.NAMES <- purrr::map_chr(str_split(CALIBRATION.SIMUATIONS, "-"), 2)
    plot.measured.annual <- cal.measured.annual
    plot.measured.trees  <- cal.measured.trees
    DATE.MIN <- "1995-01-01"
  } else {
    SIMS <- VALIDATION.SIMUATIONS
    NEW.SIM.NAMES <- VALIDATION.SIMUATIONS
    plot.measured.annual <- val.measured.annual
    plot.measured.trees  <- val.measured.trees
    DATE.MIN <- "1991-01-01"
  }

  ##### PREP MODELED TREE BIOMETRICS #####
  modeled.trees <- hop$trees %>%
    filter(SimulationName %in% SIMS) %>%
    mutate(SimulationName        = as.character(factor(SimulationName, levels = SIMS, labels = NEW.SIM.NAMES))) %>%
    filter(id == 1) %>%
    filter(Date >= lubridate::ymd(DATE.MIN)) %>%
    rename(plot                  = SimulationName) %>%
    rename(date                  = Date) %>%
    rename(modeled.dbh           = dbh) %>%
    mutate(modeled.pruned.height = crownBaseHeight) %>%
    mutate(modeled.height        = height) %>%
    select(plot, date, Year, Month, Day, modeled.dbh, modeled.height, modeled.pruned.height)

  modeled.annual <- modeled.trees %>%
    rename(year = Year) %>%
    filter(Month == 12, Day == 15) %>%
    select(-date, -Month, -Day) %>%
    arrange(year) %>%
    group_by(plot) %>%
    mutate(modeled.dbh.inc    = c(NA, diff(modeled.dbh))) %>%
    mutate(modeled.height.inc = c(NA, diff(modeled.height)) * 100) %>%
    ungroup() %>%
    arrange(plot, year)

  annual <- modeled.annual %>%
    left_join(plot.measured.annual, by = c("plot", "year"))

  ##### MEASURED vs. MODELED TIMESERIES #####
  vars <- c("dbh", "pruned.height", "height")
  labs <- c("DBH", "Pruned height", "Tree height")

  for(i in vars){
    plot.annotation <- data.frame(plot = NEW.SIM.NAMES)
    plot.annotation$date <- min(plot.measured.trees$date, na.rm = TRUE)
    plot.annotation[[paste0("measured.", i)]] <- max(plot.measured.trees[[paste0("measured.", i)]], na.rm = TRUE)

    ts.plot <- ggplot(plot.measured.trees, aes_string(x = "date", y = paste0("measured.", i))) +
      labs(x       = "Year",
           caption = "Measured: Boxplots\nModeled: Line",
           y       = paste(labs[match(i, vars)], "(cm)")) +
      facet_wrap(~plot) +
      geom_boxplot(aes(group = date), color = "grey30", na.rm = TRUE, outlier.shape = NA) +
      scale_x_date(date_breaks = "5 years", date_labels = "%Y") + # seq(lubridate::ymd("1995-01-01"), lubridate::ymd("2015-01-01"), "5 years")
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
      geom_line(data = modeled.trees, aes_string(y = paste0("modeled.", i)), color = "black", size = 0.75) +
      geom_text(data = plot.annotation, aes(label = plot), hjust = 0, vjust = 1, size = 5) +
      theme_hisafe_ts(strip.background = element_blank(),
                      strip.text       = element_blank(),
                      panel.grid       = element_blank()) #+
    #theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

    ggsave_fitmax(paste0(PATH, "analysis/", STEP, "/hisafe_", STEP, "_ts_", gsub("\\.", "_", i), ".png"), ts.plot, scale = 1.5)
  }

  ##### MEASURED vs. MODELED INCREMENT SCATTERPLOT #####
  vars <- c("dbh.inc", "height.inc")
  labs <- c("DBH increment", "Height increment")

  for(i in vars){
    if(i == "dbh.inc") LIMITS <- c(0, 3) else LIMITS <- c(0, 150)
    mvm <- mvm_annotation(annual[[paste0("modeled.", i)]], annual[[paste0("measured.", i)]])
    #grob <- grobTree(textGrob(mvm, x = 1.05, y = 0.95, hjust = 0, vjust = 1))

    # plot.annotation <- data.frame(plot = paste0("A", 2:4))
    # plot.annotation[[paste0("modeled.",  i)]] <- LIMITS[1]
    # plot.annotation[[paste0("measured.", i)]] <- LIMITS[2]

    annual$ub <- annual[[paste0("measured.", i)]] + annual[[paste0("measured.", i, ".sd")]]
    annual$lb <- annual[[paste0("measured.", i)]] - annual[[paste0("measured.", i, ".sd")]]

    mvm.inc.plot <- ggplot(annual, aes_string(x = paste0("modeled.", i), y = paste0("measured.", i))) +
      labs(x    = paste("Modeled", labs[match(i, vars)], "(cm)"),
           caption = mvm,
           fill = NULL,
           y    = paste("Measured", labs[match(i, vars)], "(cm)")) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      # geom_errorbar(aes(ymin = lb,
      #                   ymax = ub),
      #               alpha = 0.5, na.rm = TRUE) +
      geom_point(aes(fill = plot), shape = 21, na.rm = TRUE) +
      scale_fill_manual(values = c("white", "grey70", "black")) +
      scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = LIMITS) +
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
  labs <- c("DBH increment", "tree height increment")

  for(i in vars){
    plot.annotation <- data.frame(plot = NEW.SIM.NAMES)
    plot.annotation$year <- min(annual$year, na.rm = TRUE)
    plot.annotation[[paste0("measured.", i)]] <- max(annual[[paste0("measured.", i)]], na.rm = TRUE)

    annual$ub <- annual[[paste0("measured.", i)]] + annual[[paste0("measured.", i, ".sd")]]
    annual$lb <- annual[[paste0("measured.", i)]] - annual[[paste0("measured.", i, ".sd")]]

    ts.inc.plot <- ggplot(annual, aes(x = year)) +
      labs(x = "Year",
           #title = "Hi-sAFe Calibration",
           caption = "Measured: Points\nModeled: Line",
           y = paste(labs[match(i, vars)], "(cm)")) +
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