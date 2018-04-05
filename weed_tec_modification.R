library(hisafer)
library(tidyverse)
source("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/R/utils.R")
source("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/R/params.R")

management    <- readr::read_csv("./raw_data/restinclieres_weed_management.csv",    col_types = readr::cols())
fertilization <- readr::read_csv("./raw_data/restinclieres_crop_fertilization.csv", col_types = readr::cols()) # USE SAME FERTILIZATION AS MAIN CROP
NEW.TECS <- data.frame(year = 1994:2016, file.name = paste0("W", 1994:2016, "-", 1995:2017, ".tec"))

get_original_tec <- function(x, num.disturb, num.fert) {
  num.disturb <- max(num.disturb, 1)
  num.fert <- max(num.fert, 1)

  if(x == "weed") {
    original.tec.file <- "weed-restinclieres.tec"
  } else if(x == "baresoil") {
    original.tec.file <- "baresoil.tec"
  } else {
    stop(paste0("original tec file for ", x, " not found"))
  }

  tec <- read_param_file(paste0(path, original.tec.file))

  ## INITIALIZE TABLES
  tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value <- list(data.frame(julres     = rep("NA", num.disturb),
                                                                                  coderes    = rep("NA", num.disturb),
                                                                                  P_qres     = rep("NA", num.disturb),
                                                                                  P_Crespc   = rep("NA", num.disturb),
                                                                                  P_CsurNres = rep("NA", num.disturb),
                                                                                  P_Nminres  = rep("NA", num.disturb),
                                                                                  P_eaures   = rep("NA", num.disturb)))
  tec$TILLAGE_TABLE$tillage.table$value <- list(data.frame(jultrav  = rep("NA", num.disturb),
                                                      profres  = rep("NA", num.disturb),
                                                      proftrav = rep("NA", num.disturb)))
  tec$FERTILIZATION_TABLE$fertilization.table$value <- list(data.frame(julapN  = rep("NA", num.fert),
                                                                  qte  = rep("NA", num.fert)))
  tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$commented <- FALSE
  tec$TILLAGE_TABLE$tillage.table$commented <- FALSE
  tec$FERTILIZATION_TABLE$fertilization.table$commented <- FALSE

  return(tec)
}

paths <- paste0("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/inst/extdata/template_common/cropInterventions/")
for(path in paths) {
  for(i in 1:nrow(NEW.TECS)) {
    YEAR <- NEW.TECS$year[i]

    manage <- dplyr::filter(management,    year == YEAR)
    fert   <- dplyr::filter(fertilization, year == YEAR)
    if(sum(fert$kg.N) == 0) fert <- NULL

    crop <- manage$crop
    disturb.doys  <- manage$tillage.doy
    disturb.depth <- manage$tillage.depth
    real.disturb  <- !is.na(disturb.doys)
    num.disturb   <- sum(real.disturb)
    num.fert      <- length(fert$crop.doy)

    tec <- get_original_tec(crop, num.disturb, num.fert)

    ## DISCING & TILLAGE
    tec$SOIL_MANAGEMENT$P_nbjres$value  <- num.disturb # number of residue incorporation events
    tec$SOIL_MANAGEMENT$P_nbjtrav$value <- num.disturb # number of tillage events

    if(num.disturb != 0) {
      if(crop == "weed") {
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$julres     <- disturb.doys[real.disturb]
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$coderes    <- rep(1, num.disturb)
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$P_qres     <- rep(1,  num.disturb)
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$P_Crespc   <- rep(42, num.disturb)
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$P_CsurNres <- rep(15, num.disturb)
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$P_Nminres  <- rep(0, num.disturb)
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$P_eaures   <- rep(0, num.disturb)
      } else if(crop == "baresoil") {
        tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table <- NULL
      } else {
        stop(paste0("crop ", crop, " not supported"))
      }

      tec$TILLAGE_TABLE$tillage.table$value[[1]]$jultrav  <- disturb.doys[real.disturb]
      tec$TILLAGE_TABLE$tillage.table$value[[1]]$profres  <- disturb.depth[real.disturb]
      tec$TILLAGE_TABLE$tillage.table$value[[1]]$proftrav <- disturb.depth[real.disturb]
    } else {
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table <- NULL
      tec$TILLAGE_TABLE$tillage.table                             <- NULL
    }

    ## SOWING
    tec$SOWING_OPTIONS$iplt0$value <- manage$sow.doy

    ## FERTILIZATION
    tec$FERTILIZATIONS$Qtot_N$value <- num.fert
    tec$FERTILIZATIONS$napN$value   <- num.fert
    if(num.fert > 0) {
      tec$FERTILIZATION_TABLE$fertilization.table$value[[1]]$julapN <- fert$crop.doy
      tec$FERTILIZATION_TABLE$fertilization.table$value[[1]]$qte    <- fert$kg.N
    } else {
      tec$FERTILIZATION_TABLE$fertilization.table <- NULL
    }

    ## IRRIGATION
    tec$IRRIGATIONS$nap$value <- 0
    tec$IRRIGATION_TABLE$irrigation.table <- NULL

    ## CUTTING
    tec$CUTTING_INTERVENTIONS$codefauche$value <- 2 #1 = yes; 2 = no
    tec$CUTTING_INTERVENTIONS$nbcoupe$value <- 0
    tec$CUTTING_TABLE$cutting.table <- NULL

    ## WRITE
    write_param_file(tec, paste0(path, as.character(NEW.TECS$file.name[i])))
  }
}
