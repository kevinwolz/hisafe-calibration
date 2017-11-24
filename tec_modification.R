library(hisafer)

management    <- readr::read_csv("./raw_data/restinclieres_crop_management.csv",    col_types = readr::cols())
fertilization <- readr::read_csv("./raw_data/restinclieres_crop_fertilization.csv", col_types = readr::cols())
NEW.TECS <- data.frame(year = 1995:2016, file.name = paste0("R", 1995:2016, "-", 1996:2017, ".tec"))

get_original_tec <- function(x, num.disturb, num.fert) {
  if(x == "wheat-durum") {
    original.tec.file <- "durum-wheat-restinclieres.tec"
  } else if(x == "rape") {
    original.tec.file <- "rape.tec"
  } else if(x == "protein pea") {
    original.tec.file <- "pea.tec"
  } else if(x == "barley") {
    original.tec.file <- "barley.tec"
  } else {
    stop(paste0("original tec file for ", x, " not found"))
  }

  tec <- read_param_file(paste0(path, original.tec.file))

  ## INITIALIZE TABLES
  tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value <- data.frame(julres     = rep("NA", num.disturb),
                                                                                  coderes    = rep("NA", num.disturb),
                                                                                  P_qres     = rep("NA", num.disturb),
                                                                                  P_Crespc   = rep("NA", num.disturb),
                                                                                  P_CsurNres = rep("NA", num.disturb),
                                                                                  P_Nminres  = rep("NA", num.disturb),
                                                                                  P_eaures   = rep("NA", num.disturb))
  tec$TILLAGE_TABLE$tillage_table$value <- data.frame(jultrav  = rep("NA", num.disturb),
                                                      profres  = rep("NA", num.disturb),
                                                      proftrav = rep("NA", num.disturb))
  tec$FERTILIZATION_TABLE$fertilization_table$value <- data.frame(julapN  = rep("NA", num.fert),
                                                                  qte  = rep("NA", num.fert))
  tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$commented <- FALSE
  tec$TILLAGE_TABLE$tillage_table$commented <- FALSE
  tec$FERTILIZATION_TABLE$fertilization_table$commented <- FALSE

  return(tec)
}

paths <- paste0("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/inst/extdata/", c("restinclieres_monocrop", "restinclieres_agroforestry"), "/itk/")
for(path in paths) {
  for(i in 1:nrow(NEW.TECS)) {
    YEAR <- NEW.TECS$year[i]

    manage <- dplyr::filter(management,   year == YEAR)
    fert   <- dplyr::filter(fertilization, year == YEAR)

    crop <- manage$crop
    disturb.doys <- c(manage$discing.doy, manage$tillage.doy)
    num.disturb  <- length(disturb.doys[!is.na(disturb.doys)])
    num.fert  <- length(fert$crop.doy)

    tec <- get_original_tec(crop, num.disturb, num.fert)

    ## DISCING & TILLAGE
    tec$SOIL_MANAGEMENT$P_nbjres$value  <- num.disturb # number of residue incorporation events
    tec$SOIL_MANAGEMENT$P_nbjtrav$value <- num.disturb # number of tillage events

    tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$julres     <- c(manage$discing.doy, manage$tillage.doy)[!is.na(disturb.doys)]
    tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$coderes    <- rep(1, num.disturb)
    tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_Nminres  <- rep(0, num.disturb)
    tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_eaures   <- rep(0, num.disturb)

    if(crop == "wheat-durum") {
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_qres     <- rep(1, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_Crespc   <- rep(42, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_CsurNres <- rep(15, num.disturb)
    } else if(crop == "rape") {
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_qres     <- rep(2, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_Crespc   <- rep(42, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_CsurNres <- rep(60, num.disturb)
    } else if(crop == "protein pea") {
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_qres     <- rep(2, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_Crespc   <- rep(20, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_CsurNres <- rep(70, num.disturb)
    } else if(crop == "barley") {
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_qres     <- rep(5, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_Crespc   <- rep(50, num.disturb)
      tec$RESIDUE_INCORPORATION_TABLE$residue_incoproration_table$value$P_CsurNres <- rep(15, num.disturb)
    } else {
      stop(paste0("crop ", crop, " not supported"))
    }

    tec$TILLAGE_TABLE$tillage_table$value$jultrav  <- disturb.doys[!is.na(disturb.doys)]
    tec$TILLAGE_TABLE$tillage_table$value$profres  <- c(manage$discing.depth, manage$tillage.depth)[!is.na(disturb.doys)]
    tec$TILLAGE_TABLE$tillage_table$value$proftrav <- c(manage$discing.depth, manage$tillage.depth)[!is.na(disturb.doys)]

    ## SOWING
    tec$SOWING_OPTIONS$iplt0$value <- manage$sow.doy

    ## FERTILIZATION
    tec$FERTILIZATION_TABLE$fertilization_table$value$julapN <- fert$crop.doy
    tec$FERTILIZATION_TABLE$fertilization_table$value$qte    <- fert$kg.N

    ## WRITE
    write_param_file(tec, paste0(path, as.character(NEW.TECS$file.name[i])))
  }
}