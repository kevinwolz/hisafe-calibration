### hisafe-calibration
### WALNUT ALLOMETRY PARAMETERIZATION
### Author: Kevin J. Wolz

## Restinclieres Data
allom.data <- measured.trees %>%
  mutate(height = measured.height / 100) %>%
  mutate(dbh    = measured.dbh    / 100) %>%
  select(System:species, dbh, height)

## McPherson Data
mcp <- read_csv(paste0(input.path, "McPherson_Data_PROCESSED.csv"), col_types = cols()) %>%
  filter(scientific.name == "Juglans nigra") %>%
  filter(!(dbh.cm > 50  & height.m < 12)) %>%
  filter(!(dbh.cm > 100 & height.m < 15)) %>%
  mutate(dbh        = dbh.cm / 100) %>%
  mutate(height     = height.m) %>%
  mutate(dcb        = dbh * min(1, (1 + (1.3 - bole.m) / height.m) ^ (1 / 0.709708))) %>%
  mutate(crown.area = pi * crown.diam.m^2)

##### HEIGHT vs. DBH #####
mcp.dbh.model <- nls(height ~ a*dbh^b,
                     data = mcp,
                     start = list(a = 31, b = 0.72))
summary(mcp.dbh.model)
coef(mcp.dbh.model)

mcp.X.RANGE <- range(mcp$dbh, na.rm = TRUE)
mcp.h.dbh.pred <- tibble(dbh = seq(mcp.X.RANGE[1], mcp.X.RANGE[2], length.out = 1000))
mcp.h.dbh.pred$height <- predict(mcp.dbh.model, mcp.h.dbh.pred)


h.dbh.model <- nls(height ~ a*dbh^b,
                   data = allom.data,
                   start = list(a = 31, b = 0.72))
summary(h.dbh.model)
coef(h.dbh.model)

X.RANGE <- range(allom.data$dbh, na.rm = TRUE)
h.dbh.pred <- tibble(dbh = seq(X.RANGE[1], X.RANGE[2], length.out = 1000))
h.dbh.pred$height <- predict(h.dbh.model, h.dbh.pred)


h.dbh.coefs <- round(as.numeric(coef(h.dbh.model)), c(1,3))
mcp.h.dbh.coefs <- round(as.numeric(coef(mcp.dbh.model)), c(1,3))

h.dbh.plot <- ggplot(allom.data, aes(x = dbh, y = height)) +
  labs(x = "DBH (m)",
       y = "Height (m)") +
  geom_point(shape = 21, size = 1, color = "grey30", na.rm = TRUE) +
  geom_point(data = mcp, size = 1, na.rm = TRUE, color = "red") +
  geom_line(data = h.dbh.pred, color = "black", size = 1.5) +
  geom_line(data = mcp.h.dbh.pred, color = "red", size = 1.5) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  theme_ggEHD() +
  ggalt::annotate_textp(label = paste0("y = ", h.dbh.coefs[1], "x ^ ", h.dbh.coefs[2]), x = 0.05, y = 0.9, hjust = 1, vjust = 0, size = 15) +
  ggalt::annotate_textp(label = "p < 0.01", x = 0.05, y = 0.85, hjust = 1, vjust = 0, size = 15) +
  ggalt::annotate_textp(label = "Restinclieres", x = 0.05, y = 0.95, hjust = 0, vjust = 1, size = 15) +
  ggalt::annotate_textp(label = paste0("y = ", mcp.h.dbh.coefs[1], "x ^ ", mcp.h.dbh.coefs[2]), x = 0.95, y = 0.1, hjust = 0, vjust = 1, size = 15, color = "red") +
  ggalt::annotate_textp(label = "p < 0.01", x = 0.95, y = 0.05, hjust = 0, vjust = 1, size = 15, color = "red") +
  ggalt::annotate_textp(label = "McPherson et al.", x = 0.95, y = 0.15, hjust = 0, vjust = 1, size = 15, color = "red")

ggsave_fitmax(paste0(allom.path, "H_vs_DBH.jpg"), h.dbh.plot, scale = 1)

##### CROWN VOL vs. DBC #####
mcp.ca.model <- nls(crown.area ~ a*dcb^b,
                    data  = mcp,
                    start = list(a = 401, b = 1.38))
summary(mcp.ca.model)
coef(mcp.ca.model)

mcp.X.RANGE <- range(mcp$dcb, na.rm = TRUE)
mcp.ca.dcb.pred <- tibble(dcb = seq(mcp.X.RANGE[1], mcp.X.RANGE[2], length.out = 1000))
mcp.ca.dcb.pred$crown.area <- predict(mcp.ca.model, mcp.ca.dcb.pred)

mcp.ca.dcb.coefs <- round(as.numeric(coef(mcp.ca.model)), c(0,2))

ca.dcb.plot <- ggplot(mcp, aes(x = dcb, y = crown.area)) +
  labs(x = "DCB (m)",
       y = "Crown Area (m2)") +
  geom_point(data = mcp, size = 1, na.rm = TRUE, color = "red") +
  geom_line(data = mcp.ca.dcb.pred, color = "red", size = 1.5) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  theme_ggEHD() +
  ggalt::annotate_textp(label = paste0("y = ", mcp.ca.dcb.coefs[1], "x ^ ", mcp.ca.dcb.coefs[2]), x = 0.95, y = 0.1, hjust = 0, vjust = 1, size = 15, color = "red") +
  ggalt::annotate_textp(label = "p < 0.01", x = 0.95, y = 0.05, hjust = 0, vjust = 1, size = 15, color = "red") +
  ggalt::annotate_textp(label = "McPherson et al.", x = 0.95, y = 0.15, hjust = 0, vjust = 1, size = 15, color = "red")

ggsave_fitmax(paste0(allom.path, "Crown_Area_vs_DCB.jpg"), ca.dcb.plot, scale = 1)

## PHENOLOGY
pheno_summary <- function(data, thresh) {
  N    <- sum(!is.na(data))
  cnt  <- sum(!is.na(data) & data >= thresh)
  perc <- cnt / N
  return(perc)
}

pheno.raw <- read_csv(paste0(input.path, "restinclieres_tree_phenology.csv"), col_types = cols())
pheno <- pheno.raw %>%
  #mutate(filter.col = paste(plot, rowtree.id, sep = "-")) %>%
  #filter(filter.col %in% unqiue(filter(.$filter.col, year == 2003)) %>%
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  filter(!(plot == "Restinclieres-A3" & rowtree.id %in% c("2-34", "4-17", "4-30"))) %>%            # Remove trees by pit
  filter(!(plot == "Restinclieres-A4" & rowtree.id %in% c("5-7", "5-8", "6-6", "6-7", "6-8"))) %>% # Remove trees by pit
  select(plot, date, year, score) %>%
  group_by(plot, date, year)

phenol  <- summarize(pheno, '1' = pheno_summary(score, 1)) %>%
  left_join(summarize(pheno, '2' = pheno_summary(score, 2)), by = c("date", "year", "plot")) %>%
  left_join(summarize(pheno, '3' = pheno_summary(score, 3)), by = c("date", "year", "plot")) %>%
  gather("thresh", "perc", '1':'3') %>%
  mutate(thresh = as.numeric(thresh))

ggplot(filter(phenol, thresh == 1), aes(x = date, y = perc)) +
  facet_wrap(~year, scales = "free_x") +
  geom_point()


##### LEAF AREA vs. CROWN VOLUME #####
## Data from Talbot plot via WebPlotDigitizer
la.cv <- as.data.frame(matrix(c(9.21184432984775, 17.573841563273447,
                                9.198859450607117, 20.52326413364355,
                                10.499202357418085, 25.15966101515255,
                                21.71999300406509, 26.437214133908554,
                                15.145934142812493, 19.687726904139794,
                                66.5326662462039, 47.55857770522735,
                                58.559950392461346, 58.50403591246598,
                                67.78106963605241, 63.992664868216735,
                                90.20224611911111, 71.1825780020246,
                                66.40467243654633, 76.63145732744687,
                                92.1110233674827, 87.61746015762051,
                                131.0044466586461, 103.25416973622146,
                                146.15038080145854, 112.9634144402457,
                                148.10182265304934, 119.70733672176847,
                                103.76773496006489, 139.87868412823758,
                                151.3628823251944, 128.98092547739304,
                                182.35036914156694, 140.39463432990073,
                                302.4530821863357, 159.92124273244255,
                                244.1862190681627, 194.82300814602422,
                                350.4247910494433, 213.49022954086524), ncol = 2, byrow = TRUE))
names(la.cv) <- c("crown.volume", "leaf.area")

la.cv.model <- nls(leaf.area ~ a*crown.volume^b,
                    data  = la.cv,
                    start = list(a = 5.34, b = 0.61))
summary(la.cv.model)
coef(la.cv.model)
plot(la.cv.model)

la.cv.X.RANGE <- range(la.cv$crown.volume, na.rm = TRUE)
la.cv.pred <- tibble(crown.volume = seq(la.cv.X.RANGE[1], la.cv.X.RANGE[2], length.out = 1000))
la.cv.pred$leaf.area <- predict(la.cv.model, la.cv.pred)

la.cv.coefs <- round(as.numeric(coef(la.cv.model)), c(2,2))

ca.dcb.plot <- ggplot(la.cv, aes(x = crown.volume, y = leaf.area)) +
  labs(x = "Crown volume (m3)",
       y = "Leaf area (m2)") +
  geom_point(data = la.cv, size = 1, na.rm = TRUE, color = "red") +
  geom_line(data = la.cv.pred, color = "red", size = 1.5) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  theme_ggEHD() +
  ggalt::annotate_textp(label = paste0("y = ", la.cv.coefs[1], "x ^ ", la.cv.coefs[2]), x = 0.95, y = 0.1, hjust = 0, vjust = 1, size = 15, color = "red") +
  ggalt::annotate_textp(label = "p < 0.01", x = 0.95, y = 0.05, hjust = 0, vjust = 1, size = 15, color = "red") +
  ggalt::annotate_textp(label = "Talbot re-fit", x = 0.95, y = 0.15, hjust = 0, vjust = 1, size = 15, color = "red")

ggsave_fitmax(paste0(allom.path, "Leaf_Area_vs_Crown_Volume.jpg"), ca.dcb.plot, scale = 1)
