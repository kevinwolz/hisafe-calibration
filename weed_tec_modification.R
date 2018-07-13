library(hisafer)
library(tidyverse)
source("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/R/utils.R")
source("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/R/params.R")

management    <- readr::read_csv("./raw_data/restinclieres_weed_management.csv",    col_types = readr::cols())
fertilization <- readr::read_csv("./raw_data/restinclieres_crop_fertilization.csv", col_types = readr::cols()) # USE SAME FERTILIZATION AS MAIN CROP
NEW.TECS <- data.frame(year = 1994:2017, file.name = paste0("W", 1994:2017, "-", 1995:2018, ".tec"))

get_original_tec <- function(x, num.disturb, num.weeds, num.fert) {
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
  tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value <- list(data.frame(julres   = rep("NA", num.weeds),
                                                                                       coderes  = rep("NA", num.weeds),
                                                                                       qres     = rep("NA", num.weeds),
                                                                                       Crespc   = rep("NA", num.weeds),
                                                                                       CsurNres = rep("NA", num.weeds),
                                                                                       Nminres  = rep("NA", num.weeds),
                                                                                       eaures   = rep("NA", num.weeds)))
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
    num.weeds     <- 0

    tec <- get_original_tec(crop, num.disturb, num.weeds, num.fert)

    ## DISCING & TILLAGE
    tec$SOIL_MANAGEMENT$nbjtrav$value <- num.disturb # number of tillage events
    if(num.disturb != 0) {
      tec$TILLAGE_TABLE$tillage.table$value[[1]]$jultrav  <- disturb.doys[real.disturb]
      tec$TILLAGE_TABLE$tillage.table$value[[1]]$profres  <- disturb.depth[real.disturb]
      tec$TILLAGE_TABLE$tillage.table$value[[1]]$proftrav <- disturb.depth[real.disturb]
    } else {
      tec$TILLAGE_TABLE$tillage.table <- NULL
    }

    ## WEED RESIDUE
    tec$SOIL_MANAGEMENT$nbjres$value  <- num.weeds # number of residue incorporation events
    if(num.weeds != 0) {
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$julres   <- manage$tillage.doy
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$coderes  <- 1
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$qres     <- manage$tillage.weeds
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$Crespc   <- 50
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$CsurNres <- 25
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$Nminres  <- 0
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table$value[[1]]$eaures   <- 0
    } else {
      tec$RESIDUE_INCORPORATION_TABLE$residue.incorporation.table <- NULL
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
