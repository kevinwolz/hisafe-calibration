### hisafe-calibration
### FIELD DATA
### Author: Kevin J. Wolz

## MEASURED TREE BIOMETRICS
ROW.END.TREES <- c("A-13", "B-10", "C-9", "D-10", "E-6", "F-6", "G-3", "H-3", "I-1", "J-5", "K-10",
                   "A-67", "B-67", "C-68", "D-70", "E-73", "F-76", "G-78", "H-83", "I-83", "J-82", "K-80",
                   "I-50", "J-52", "K-17", "I-63", "J-68", "K-79")
OTHER.EDGE.TREES <- c("B-11", "C-10", "I-2", "I-3", "D-69", "F-74", "G-76", "G-77", "H-78", "H-80", "H-81", "I-81",
                      "I-42", "I-48", "I-65", "I-68", "H-50", "H-51", "H-52", "H-55", "H-57", "H-58", "H-61", "H-62")

measured.trees.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_tree_biometrics.csv"), stringsAsFactors = FALSE))
measured.trees <- measured.trees.raw %>%
  filter(!is.na(value),
         !(variable %in% c("diametre_a_la_base_du_tronc", "circonference_tronc_a_130cm")),
         value > 0) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  mutate(date = ymd(paste0(year, "-12-15"))) %>%
  mutate(variable = factor(variable, labels = c("measured.dbh", "measured.pruned.height", "measured.height"))) %>%
  spread(variable, value) %>%
  filter(!(row.id %in% c("A", "J", "K"))) %>% # Don't use edge rows
  filter(!(rowtree.id %in% ROW.END.TREES)) %>% # Don't use trees at row ends
  filter(!(rowtree.id %in% OTHER.EDGE.TREES)) %>% # Don't use other trees that are exposed
  filter(!(row.id %in% c("E", "F") & year > 2013)) %>% # Don't use rows E&F after 2013 because they were pollarded
  select(year, date, id, AFTF, rowtree.id, row.id, tree.id, everything(), -unit)
write_csv(measured.trees, paste0(data.path, "restinclieres_tree_biometrics_PROCESSED.csv"))

## MEASURED CROP YIELD (raw units are hundredweight/ha)
measured.yield.raw <- as.tibble(read.csv(paste0(input.path, "restinclieres_crop_yield.csv"), stringsAsFactors = FALSE))
measured.yield <- measured.yield.raw %>%
  filter(crop == "durum wheat") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(measured.yield    = measured.yield / 10,
         measured.yield.sd = measured.yield.sd / 10)
write_csv(measured.yield, paste0(data.path, "restinclieres_crop_yield_PROCESSED.csv"))

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