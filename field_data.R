### hisafe-calibration
### FIELD DATA
### Author: Kevin J. Wolz

## MEASURED TREE BIOMETRICS
A2.EDGE.TREES <- c(paste("B", c(10:12, 69), sep = "-"),
                   paste("C", c(9:10, 68), sep = "-"),
                   paste("D", c(8:10, 68:70), sep = "-"),
                   paste("E", c(6:8, 70:73), sep = "-"),
                   paste("F", c(4:6, 73:76), sep = "-"),
                   paste("G", c(3:4, 76:80), sep = "-"),
                   paste("H", c(1:3, 50:62, 80:83), sep = "-"),
                   paste("I", c(1:5, 42:50, 61:68, 82:84), sep = "-"))

A3.EDGE.TREES <- c(paste(2, c(1:10, 53:55), sep = "-"),
                   paste(3, c(3:4, 55:57), sep = "-"),
                   paste(4, c(4:5, 56:59), sep = "-"),
                   paste(5, c(5:16, 50:62), sep = "-"))

A4.GOOD.TREES <- c(paste(3, c(3:4), sep = "-"),
                   paste(4, c(3:4), sep = "-"),
                   paste(5, c(3:4), sep = "-"),
                   paste(6, c(3:4, 31:34), sep = "-"),
                   paste(7, c(3:4, 10:18, 30:34), sep = "-"),
                   paste(8, c(3:4, 10:17, 29:34), sep = "-"),
                   paste(9, c(30:34), sep = "-"),
                   paste(10, c(32:34), sep = "-"))

measured.trees.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_tree_biometrics.csv"), stringsAsFactors = FALSE))
measured.trees <- measured.trees.raw %>%
  filter(!is.na(value),
         !(variable %in% c("diametre_a_la_base_du_tronc", "circonference_tronc_a_130cm")),
         (species != "NN" | is.na(species)),
         !(plot == "Restinclieres-A2" & System == "Forestry"), # this plot is too small to use for model comparisons
         value > 0) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  mutate(date = ymd(paste0(year, "-12-15"))) %>%
  mutate(variable = factor(variable, labels = c("measured.dbh", "measured.pruned.height", "measured.height"))) %>%
  spread(variable, value) %>%
  ## Filter A2AF Trees
  filter(!(plot == "Restinclieres-A2" & row.id     %in% c("A", "J", "K")),              # Don't use edge rows
         !(plot == "Restinclieres-A2" & rowtree.id %in% A2.EDGE.TREES),                 # Don't use other trees that are exposed
         !(plot == "Restinclieres-A2" & row.id     %in% c("E", "F") & year > 2013)) %>% # Don't use rows E&F after 2013 because they were pollarded
  ## Filter A3 Trees
  filter(!(plot == "Restinclieres-A3" & row.id     %in% c("1", "6", "7", "8")),  # Don't use edge rows
         !(plot == "Restinclieres-A3" & rowtree.id %in% A3.EDGE.TREES)) %>%      # Don't use other trees that are exposed
  ## Filter A4 Trees
  filter(!(plot == "Restinclieres-A4" & !(rowtree.id %in% A4.GOOD.TREES))) %>% # Use specific trees that are not on edges and were not fertilized
  select(System, plot, year, date, id, rowtree.id, row.id, tree.id, everything(), -unit)
write_csv(measured.trees, paste0(data.path, "restinclieres_tree_biometrics_PROCESSED.csv"))

#table(measured.trees$plot, measured.trees$System)
#table(measured.trees$year, measured.trees$plot)

# need to calculate increment at tree level to get STDEV
# calc.increment <- function(id, x) {
#   y <- filter(x, id == id) %>%
#     select(-measured.pruned.height) %>%
#     arrange(year) %>%
#     mutate(measured.dbh = c(NA, diff(measured.dbh)),
#            measured.height = c(NA, diff(measured.height)))
#   return(y)
# }
#
# measured.trees.increment <- purrr::map_dfr(unique(measured.trees$id), calc.increment, x = measured.trees)

## MEASURED CROP YIELD (raw units are hundredweight/ha)
measured.yield.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_crop_yield.csv"), stringsAsFactors = FALSE))
measured.yield <- measured.yield.raw %>%
  filter(crop == "durum wheat") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(measured.yield    = measured.yield / 10,
         measured.yield.sd = measured.yield.sd / 10) %>%
  select(plot, year, crop, location, everything(), -System)
write_csv(measured.yield, paste0(data.path, "restinclieres_crop_yield_PROCESSED.csv"))

# measured.AF.yield <- measured.yield %>%
#   ungroup() %>%
#   filter(System == "Agroforestry") %>%
#   select(-System, -crop, -measured.yield.sd) %>%
#   rename(measured.yield.AF = measured.yield)
#
# measured.CC.yield <- measured.yield %>%
#   ungroup() %>%
#   filter(System == "Monocrop") %>%
#   select(-System, -crop, -location, -measured.yield.sd) %>%
#   rename(measured.yield.CC = measured.yield)
#
# measured.rel.yield <- measured.AF.yield %>%
#   left_join(measured.CC.yield, by = "year") %>%
#   mutate(measured.rel.yield = measured.yield.AF / measured.yield.CC) %>%
#   select(-(measured.yield.AF:measured.yield.CC))

## MVM ANNOTATION FUNCTION
mvm_annotation = function(m, o) {
  rmse <- round(sqrt(mean((m - o)^2, na.rm = TRUE)), 2)

  pcc     <- cor.test(m, o, method = "pearson")
  pcc.est <- round(pcc$estimate, 2)
  pcc.p   <- round(pcc$p.val, 2)

  srcc <- cor.test(m, o, method = "spearman", exact = FALSE)
  srcc.est <- round(srcc$estimate, 2)
  srcc.p   <- round(srcc$p.val, 2)

  label <- paste0("RMSE = ", rmse,
                  "\nPCC = ", pcc.est, ", p = ", pcc.p,
                  "\nSRCC = ", srcc.est, ", p = ", srcc.p)
  return(label)
}