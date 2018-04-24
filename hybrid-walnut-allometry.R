### hisafe-calibration
### WALNUT ALLOMETRY PARAMETERIZATION
### Author: Kevin J. Wolz

## Filter data to what is appropriate for allometries
allom.data <- measured.trees.all %>%
  mutate(height           = measured.height       ) %>%
  mutate(dbh              = measured.dbh          ) %>%
  mutate(pruned.height    = measured.pruned.height) %>%
  select(-measured.dbh, -measured.height, -measured.pruned.height) %>%
  filter(!(plot %in% c("Restinclieres-A4", "Castries", "NDL"))) %>%                        # Remove forestry trees or data from validation sites
  filter(!(plot == "Restinclieres-A2" & row.id %in% c("C", "G") & year >= 2004))           # Don't use rows C&G after 2004 because they were pruned high
  mutate(plot = str_replace(plot, "-A2|-A3", "")) %>%                                      # Treat all restinclieres as one plot for allometry
  filter(!(plot == "Restinclieres" & spacing.1 < 2 & year >= 2004) | is.na(spacing.1)) %>% # Remove trees that are too close together
  filter(!(plot == "Restinclieres" & spacing.2 < 2 & year >= 2004) | is.na(spacing.2)) %>% # Remove trees that are too close together
  filter(!(plot == "Restinclieres" & spacing.1 < 3 & year >= 2014) | is.na(spacing.1)) %>% # Remove trees that are too close together
  filter(!(plot == "Restinclieres" & spacing.2 < 3 & year >= 2014) | is.na(spacing.2)) %>% # Remove trees that are too close together


##### HEIGHT vs. DBH #####
h_dbh <- function(data, site = NULL){
  if(!is.null(site)) data <- filter(data, plot == site)
  h.dbh.model <- nls(height ~ a * dbh ^ b,
                     data  = data,
                     start = list(a = 31, b = 0.72))
  DBH.RANGE <- range(data$dbh, na.rm = TRUE)
  h.dbh.pred <- tibble(dbh = seq(DBH.RANGE[1], DBH.RANGE[2], length.out = 1000))
  h.dbh.pred$height <- predict(h.dbh.model, h.dbh.pred)
  h.dbh.coefs <- round(as.numeric(coef(h.dbh.model)), c(1,2))
  model.formula <- paste0("y = ", h.dbh.coefs[1], "x ^ ", h.dbh.coefs[2])
  return(list(pred = h.dbh.pred, coefs = coef(h.dbh.model), model.formula = model.formula))
}

# all.h.dbh           <- h_dbh(allom.data)
# restinclieres.h.dbh <- h_dbh(allom.data, "Restinclieres")
# castries.h.dbh      <- h_dbh(allom.data, "Castries")
#
# h.dbh.plot <- ggplot(allom.data, aes(x = dbh, y = height)) +
#   labs(x     = "DBH (m)",
#        y     = "Height (m)",
#        color = NULL) +
#   geom_point(shape = 21, size = 1, na.rm = TRUE, aes(color = plot), alpha = 0.4) + #, fill = age
#   scale_color_manual(values = c("blue", "green")) + #c("grey30", "grey70")) +
#   #scale_fill_viridis(option = "magma") +
#   geom_line(data = all.h.dbh$pred,           color = "black",      size = 1, linetype = "solid") +
#   geom_line(data = restinclieres.h.dbh$pred, color = "dark green", size = 1, linetype = "dashed") +
#   geom_line(data = castries.h.dbh$pred,      color = "dark blue",  size = 1, linetype = "dashed") +
#   scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(NA, 0.43)) +
#   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(NA, 18)) +
#   ggalt::annotate_textp(label = "All sites",                       x = 0.05, y = 0.98, hjust = 1, vjust = 0, size = 15) +
#   ggalt::annotate_textp(label = all.h.dbh$model.formula,           x = 0.05, y = 0.92, hjust = 1, vjust = 0, size = 15) +
#   ggalt::annotate_textp(label = "Restinclieres",                   x = 0.05, y = 0.84, hjust = 1, vjust = 0, size = 15) +
#   ggalt::annotate_textp(label = restinclieres.h.dbh$model.formula, x = 0.05, y = 0.78, hjust = 1, vjust = 0, size = 15) +
#   ggalt::annotate_textp(label = "Castries",                        x = 0.05, y = 0.70, hjust = 1, vjust = 0, size = 15) +
#   ggalt::annotate_textp(label = castries.h.dbh$model.formula,      x = 0.05, y = 0.64, hjust = 1, vjust = 0, size = 15) +
#   guides(color = guide_legend(override.aes = list(size = 2, stroke = 1.5)),
#          fill  = FALSE) +
#   theme_ggEHD() +
#   theme(legend.position = c(0.8, 0.2))
# ggsave_fitmax(paste0(allom.path, "H_vs_DBH.jpg"), h.dbh.plot, scale = 1)

all.h.dbh <- h_dbh(test.data)

h.dbh.plot <- ggplot(test.data, aes(x = dbh, y = height)) +
  labs(x     = "DBH (m)",
       y     = "Height (m)",
       color = NULL) +
  geom_point(shape = 21, size = 1, na.rm = TRUE, color = "grey50") +
  scale_fill_viridis(option = "magma") +
  geom_line(data = all.h.dbh$pred, color = "black", size = 1, linetype = "solid") +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(NA, 0.43)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(NA, 18)) +
  ggalt::annotate_textp(label = all.h.dbh$model.formula, x = 0.05, y = 0.95, hjust = 1, vjust = 0, size = 15) +
  guides(color = guide_legend(override.aes = list(size = 2, stroke = 1.5)), fill  = FALSE) +
  theme_ggEHD() +
  theme(legend.position = c(0.8, 0.2))
ggsave_fitmax(paste0(allom.path, "H_vs_DBH.jpg"), h.dbh.plot, scale = 1)

##### CROWN AREA vs. DCB #####
talbot.crown.area <- read_csv(paste0(input.path, "talbot_crown_area_data.csv"), col_types = cols())
talbot.crown.area$age <- NA
talbot.crown.area$age[talbot.crown.area$plot == "Castries"]      <- talbot.crown.area$year[talbot.crown.area$plot == "Castries"]      - 1991
talbot.crown.area$age[talbot.crown.area$plot == "Restinclieres"] <- talbot.crown.area$year[talbot.crown.area$plot == "Restinclieres"] - 1995
talbot.crown.area$age[talbot.crown.area$plot == "NDL"]           <- NA

# allom.data provies mostly Castries data in 2006 + 20 trees we measured in 2018,
# whereas Talbot data provides mostly Castries data from 2004
crown.area.data <- allom.data %>%
  filter(!is.na(crown.area)) %>%
  mutate(dcb = dbh * min(1, (1 + (1.3 - pruned.height) / height) ^ (1 / 0.709708))) %>%
  bind_rows(talbot.crown.area)

ca.model <- nls(crown.area ~ a * dcb ^ b,
                data  = crown.area.data,
                start = list(a = 401, b = 1.38))

DCB.RANGE <- range(crown.area.data$dcb, na.rm = TRUE)
ca.pred <- tibble(dcb = seq(DCB.RANGE[1], DCB.RANGE[2], length.out = 1000))
ca.pred$crown.area <- predict(ca.model, ca.pred)
ca.coefs <- round(as.numeric(coef(ca.model)), c(1,2))
model.formula <- paste0("y = ", ca.coefs[1], "x ^ ", ca.coefs[2])

ca.dcb.plot <- ggplot(crown.area.data, aes(x = dcb, y = crown.area)) +
  labs(x     = "DCB (m)",
       y     = "Crown area (m2)",
       fill = NULL) +
  geom_point(shape = 21, size = 1.2, na.rm = TRUE, aes(fill = plot), color = "black") +
  scale_fill_manual(values = c("white", "grey50", "black")) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  #scale_fill_viridis(option = "magma") +
  geom_line(data = ca.pred, color = "black", size = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  ggalt::annotate_textp(label = model.formula,      x = 0.05, y = 0.98, hjust = 1, vjust = 0, size = 15, color = "black") +
  ggalt::annotate_textp(label = "p < 0.01",         x = 0.05, y = 0.92, hjust = 1, vjust = 0, size = 15, color = "black") +
  theme_ggEHD() +
  theme(legend.position = c(0.8, 0.2))
ggsave_fitmax(paste0(allom.path, "Crown_Area_vs_DCB.jpg"), ca.dcb.plot, scale = 1)

##### LEAF AREA vs. CROWN VOLUME #####
talbot.leaf.area <- read_csv(paste0(input.path, "talbot_leaf_area_data.csv"), col_types = cols())
talbot.leaf.area$age <- NA
talbot.leaf.area$age[talbot.leaf.area$plot == "Castries"]      <- talbot.leaf.area$year[talbot.leaf.area$plot == "Castries"]      - 1991
talbot.leaf.area$age[talbot.leaf.area$plot == "Restinclieres"] <- talbot.leaf.area$year[talbot.leaf.area$plot == "Restinclieres"] - 1995
talbot.leaf.area$age[talbot.leaf.area$plot == "NDL"]           <- NA

la.cv.model <- nls(leaf.area ~ a * crown.volume ^ b,
                    data  = talbot.leaf.area,
                    start = list(a = 5.34, b = 0.61))

VOL.RANGE <- range(talbot.leaf.area$crown.volume, na.rm = TRUE)
la.cv.pred <- tibble(crown.volume = seq(VOL.RANGE[1], VOL.RANGE[2], length.out = 1000))
la.cv.pred$leaf.area <- predict(la.cv.model, la.cv.pred)
la.cv.coefs <- round(as.numeric(coef(la.cv.model)), c(2,2))
model.formula <- paste0("y = ", la.cv.coefs[1], "x ^ ", la.cv.coefs[2])

ca.dcb.plot <- ggplot(talbot.leaf.area, aes(x = crown.volume, y = leaf.area)) +
  labs(x    = "Crown volume (m3)",
       y    = "Leaf area (m2)",
       fill = NULL) +
  geom_point(shape = 21, size = 1.2, na.rm = TRUE, aes(fill = plot), color = "black") +
  scale_fill_manual(values = c("white", "grey50", "black")) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  geom_line(data = la.cv.pred, color = "black", size = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  ggalt::annotate_textp(label = "Talbot re-fit", x = 0.05, y = 0.98, hjust = 1, vjust = 0, size = 15, color = "black") +
  ggalt::annotate_textp(label = model.formula,   x = 0.05, y = 0.92, hjust = 1, vjust = 0, size = 15, color = "black") +
  ggalt::annotate_textp(label = "p < 0.01",      x = 0.05, y = 0.86, hjust = 1, vjust = 0, size = 15, color = "black") +
  theme_ggEHD() +
  theme(legend.position = c(0.8, 0.2))

ggsave_fitmax(paste0(allom.path, "Leaf_Area_vs_Crown_Volume.jpg"), ca.dcb.plot, scale = 1)
