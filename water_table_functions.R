##### FUNCTIONS #####
water_table_comp <- function(params, wth, latitude, piezo.data) {
  modeled.data <- compute_nappe(wth = wth, params, latitude = latitude) %>%
    rename(modeled = watertable) %>%
    select(year, doy, modeled)

  if(any(is.infinite(modeled.data$modeled) | is.nan(modeled.data$modeled))) return(Inf)

  comp.data <- piezo.data %>%
    rename(measured = watertable) %>%
    left_join(modeled.data, by = c("year", "doy"))

  # ggplot(comp.data, aes(x = measured, y = modeled)) + geom_point() + geom_abline()
  rmse <- sqrt(mean((comp.data$measured - comp.data$modeled) ^ 2))
  return(rmse)
}

compute_nappe <- function(wth, params, latitude){
  etpGen <- getETP(tavg     = wth$Tavg,
                   rg       = wth$Rg,
                   wind     = wth$wind,
                   julian   = wth$doy,
                   hravg    = wth$RHavg,
                   latitude = latitude)

  nappe <- predNappe(init = params[1], #-300,
                     rain = wth$precip,
                     etp  = etpGen,
                     a    = params[2], #1.4e-05,
                     b    = params[3], #2.25,
                     l    = params[4], #120,
                     m    = params[5], #6,
                     n    = params[6], #0.6,
                     ru   = params[7], #300,
                     prof_nappe_max = params[8]) #540)
  wth$watertable <- nappe / 100
  return(wth)
}

predNappe <- function(init,
                      rain,
                      etp,
                      a  = 1.4e-05,
                      b  = 2.25,
                      l  = 120,
                      m  = 6,
                      n  = 0.6,
                      ru = 300,
                      prof_nappe_max = 540){
  # prof_nappe_max = 540 in AF et prof_nappe_max=  400 in TF
  sol <- vector()
  sol[1] <- 100
  for(i in 2:length(rain)){
    sol[i] <- sol[i - 1] - etp[i] + min(500, rain[i])
    sol[i] <- max(min(sol[i], ru), 0)
  }

  glo <- vector()
  glo[1] <- init
  glo[2] <- init

  for(i in 3:length(rain)){
    glo[i] <- glo[i - 1] - a * (prof_nappe_max + glo[i - 1]) ^ b
    glo[i] <- glo[i] + ((sol[i - 2] + rain[i - 1]) > l) * m * rain[i - 1] ^ n
  }
  glo <- pmin(0, glo)
  return(glo)
}

getETP <- function(tavg, rg, wind, julian, hravg, latitude) {
	gama <- 0.65

	delta <- getDelta(tavg)
	airVapourP <- getVpSat(tavg) * hravg / 100
	dsat <- getVpSat(tavg) - airVapourP
	L <- (2500840 - 2358.6 * tavg) / 1000000

	fracinsol <- ((rg / getExtraRad(julian, latitude)) - 0.18) / 0.62
	fracinsol <- sapply(fracinsol, max, 0)
	var1 <- (tavg + 273.16) ^ 4 / 1000000000
	var2 <- (0.1 + 0.9 * fracinsol)
	var3 <- 0.56 - 0.08 * sqrt(airVapourP)
	rglo <- 4.9 * var1 * var2 * var3
	rnetp <- (1 - 0.2) * rg - rglo
	etp <- (rnetp / L * delta / (delta + gama) + (gama / (delta + gama)) * (0.26 * (1 + 0.54 * wind)) * dsat)
	etp <- sapply(etp, max, 0)
	return(etp)
}

getVpSat <- function(temp) 6.107 * (1 + sqrt(2) * sin(pi * temp / 3 / 180)) ^ 8.827

getDelta <- function(temp) getVpSat(temp + 0.5) - getVpSat(temp - 0.5)

getExtraRad <- function(julian, latitude){
	om <- 0.017202 * (julian - 3.244)
	teta <- om + 0.03344 * sin(om) * (1 + 0.021 * cos(om)) - 1.3526
	sidec <- 0.3978 * sin(teta)
	sunDeclination <- asin(sidec)
	codec <- cos(sunDeclination)
	silat <- sin(latitude / 180*pi)
	colat <- cos(latitude / 180 * pi)
	sinR <- 0.01064
	AA <- (-sinR - sidec * silat) / (codec * colat);
	dayLength <-24 / pi* acos(AA)
	CC <- 1370 * 3600 * 1.e-6
	CC <- CC * (1 + 0.033 * cos(2 * pi * (julian-4)/366))
	G0 <- silat * sidec * dayLength
	G0 <- G0 + colat * codec * (24 / pi) * sin((pi / 12) * (dayLength / 2))
	return(G0 * CC)
}

# nappe_isomorphism <- function(dati, parcel_name){
#   # compute isomorphism from parcel A2 to one among parcels A1, A3, A4 in Restinclieres (copied from an Excel file)
#   fluctuations <- data.frame(name = c("A1","A2","A3","A4"),
#                              min  = c(-1,-3,-1,-0.5),
#                              max  = c(-4,-6,-4,-3),
#                              a    = numeric(4),
#                              b    = numeric(4))
#   fluctuations$a <- (fluctuations$max - fluctuations$min) / (fluctuations$max[fluctuations$name == "A3"] - fluctuations$min[fluctuations$name == "A3"])
#   fluctuations$b <- (fluctuations$min - (fluctuations$a * fluctuations$min[fluctuations$name == "A3"]) )
#   dati$water <- dati$water * fluctuations$a[fluctuations$name == parcel_name] + fluctuations$b[fluctuations$name == parcel_name]
#   return (dati)
# }
