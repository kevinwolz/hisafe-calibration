### hisafe-calibration
### WALNUT PHENOLOGY PARAMETERIZATION
### Author: Kevin J. Wolz

# Scores  :
# 0 = 0%
# 1 = moins de 50%
# 2 = plus de 50 %
# 3 = 100%

## WEATHER
wth.raw <- as_tibble(read.csv(paste0(input.path, "restinclieres_weather.csv"), stringsAsFactors = FALSE)) %>%
  group_by(year) %>%
  mutate(tmean = (tmin + tmax)/2)

wth.raw$tmean[wth.raw$tmean > 30] <- 30
wth.raw$tmean[wth.raw$tmean < 0] <- 0

wth <- wth.raw %>%
  filter(doy >= 61) %>%
  group_by(year) %>%
  mutate(cgdd = cumsum(tmean)) %>%
  select(year, month, day, cgdd)


perc_gr <- function(x, thresh) sum(x >= thresh, na.rm = TRUE) / length(x[!is.na(x)])

pheno.data.raw <- as_tibble(read.csv(paste0(input.path, "restinclieres_tree_phenology.csv"), stringsAsFactors = FALSE)) %>%
  filter(plot %in% c("Restinclieres-A2", "Restinclieres-A3", "Restinclieres-A4")) %>%
  filter(!(plot == "Restinclieres-A2" & str_detect(rowtree.id, "E|F"))) %>%
  filter(!is.na(score))

bb.data <- pheno.data.raw %>%
  filter(metric == "bud.burst") %>%
  group_by(year, month, day, plot, metric) %>%
  summarize(score3 = perc_gr(score, 3),
            score2 = perc_gr(score, 2),
            score1 = perc_gr(score, 1)) %>%
  ungroup()

lf.data <- pheno.data.raw %>%
  filter(metric == "leaf.fall") %>%
  group_by(year, month, day, plot, metric) %>%
  summarize(score3 = perc_gr(score, 3),
            score2 = perc_gr(score, 2),
            score1 = perc_gr(score, 1)) %>%
  ungroup()

pheno.data <- bb.data %>%
  bind_rows(lf.data) %>%
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  left_join(wth, by = c("year", "month", "day"))

ggplot(filter(pheno.data, metric == "bud.burst"), aes(x = cgdd, y = score1, color = plot)) +
  #facet_wrap(~year, scales = "free_x") +
  geom_point() +
  theme_hisafe_ts() +
  geom_vline(xintercept = c(600,700))

#nlmod <- nls(score ~  1 / (1 + exp(b * cgdd)), data = pheno.data)

## SEASON DURATION
find_pheno_p <- function(y, p, df) {
  ## BUD BURST
  dat <- filter(df, year == y & plot == p & metric == "bud.burst" & !is.na(score2))
  if(nrow(dat) > 2 & any(dat$score2 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score2, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      bud.burst.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      bud.burst.date <- NA
    }
  } else {
    bud.burst.date <- NA
  }

  ## LEAF FALL
  dat <- filter(df, year == y & plot == p & metric == "leaf.fall" & !is.na(score2))
  if(nrow(dat) > 2 & any(dat$score2 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score2, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      leaf.fall.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      leaf.fall.date <- NA
    }
  } else {
    leaf.fall.date <- NA
  }
  out <- tibble(year      = y,
                plot      = p,
                bud.burst = bud.burst.date,
                leaf.fall = leaf.fall.date)
  return(out)
}

find_pheno <- function(y, df) {
  ## BUD BURST
  dat <- filter(df, year == y & metric == "bud.burst" & !is.na(score2))
  if(nrow(dat) > 2 & any(dat$score2 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score2, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      bud.burst.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      bud.burst.date <- NA
    }
  } else {
    bud.burst.date <- NA
  }

  ## LEAF FALL
  dat <- filter(df, year == y & metric == "leaf.fall" & !is.na(score2))
  if(nrow(dat) > 2 & any(dat$score2 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score2, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      leaf.fall.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      leaf.fall.date <- NA
    }
  } else {
    leaf.fall.date <- NA
  }
  out <- tibble(year      = y,
                bud.burst = bud.burst.date,
                leaf.fall = leaf.fall.date)
  return(out)
}

search.df <- expand.grid(year = unique(pheno.data$year), plot = unique(pheno.data$plot))
pheno <- purrr::map2_df(search.df$year, search.df$plot, find_pheno_p, df = pheno.data) %>%
  mutate(season.duration = as.numeric(leaf.fall - bud.burst))

pheno <- purrr::map_df(unique(pheno.data$year), find_pheno, df = pheno.data) %>%
  mutate(season.duration = as.numeric(leaf.fall - bud.burst))

SEASON.DURATION <- pheno %>%
  #filter(year > 2012) %>%
  #group_by(plot) %>%
  summarize(season.dur.mean   = round(mean(season.duration,   na.rm = TRUE)),
            season.dur.median = round(median(season.duration, na.rm = TRUE)))
round(mean(pheno$season.duration, na.rm = TRUE))
round(median(pheno$season.duration, na.rm = TRUE))

ggplot(pheno, aes(x = plot, y = season.duration)) + geom_boxplot()
ggplot(pheno, aes(x = year, y = season.duration, color = plot)) +
  geom_line() +
  theme_hisafe_ts()

##### LEAF FALL DURATION #####
find_lfd_p <- function(y, p, df) {
  ## LEAF FALL START
  dat <- filter(df, year == y & plot == p & metric == "leaf.fall" & !is.na(score1))
  if(nrow(dat) > 2 & any(dat$score1 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score1, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      lf.start.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      lf.start.date <- NA
    }
  } else {
    lf.start.date <- NA
  }

  ## LEAF FALL END
  dat <- filter(df, year == y & plot == p & metric == "leaf.fall" & !is.na(score3))
  if(nrow(dat) > 2 & any(dat$score3 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score3, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      lf.end.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      lf.end.date <- NA
    }
  } else {
    lf.end.date <- NA
  }
  out <- tibble(year      = y,
                plot      = p,
                lf.start = lf.start.date,
                lf.end   = lf.end.date)
  return(out)
}

find_lfd_p_ALT <- function(y, p, df) {
  ## LEAF FALL START
  dat <- filter(df, year == y & plot == p & metric == "leaf.fall" & !is.na(score1))
  if(nrow(dat) > 2) {
    lf.start.date <- dat$date[1]
    lf.thresh     <- dat$score1[1]
  } else {
    lf.start.date <- NA
    lf.thresh <- NA
  }

  ## LEAF FALL END
  dat <- filter(df, year == y & plot == p & metric == "leaf.fall" & !is.na(score3))
  if(nrow(dat) > 2) {
    dat.out <- as.data.frame(approx(dat$date, dat$score3, xout = seq(min(dat$date), max(dat$date), 1)))
    lf.end.date <- filter(dat.out, y > lf.thresh)$x[1]
  } else {
    lf.end.date <- NA
  }
  out <- tibble(year     = y,
                plot     = p,
                lf.start = lf.start.date,
                lf.end   = lf.end.date)
  return(out)
}

find_lfd <- function(y, df) {
  ## LEAF FALL START
  dat <- filter(df, year == y & metric == "leaf.fall" & !is.na(score1))
  if(nrow(dat) > 2 & any(dat$score1 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score1, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      lf.start.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      lf.start.date <- NA
    }
  } else {
    lf.start.date <- NA
  }

  ## LEAF FALL END
  dat <- filter(df, year == y & metric == "leaf.fall" & !is.na(score3))
  if(nrow(dat) > 2 & any(dat$score3 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score3, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      lf.end.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      lf.end.date <- NA
    }
  } else {
    lf.end.date <- NA
  }
  out <- tibble(year      = y,
                lf.start = lf.start.date,
                lf.end   = lf.end.date)
  return(out)
}

search.df <- expand.grid(year = unique(pheno.data$year), plot = unique(pheno.data$plot))
# lf <- purrr::map2_df(search.df$year, search.df$plot, find_lfd_p_ALT, df = pheno.data) %>%
#   mutate(lf.duration = as.numeric(lf.end - lf.start))

lf <- purrr::map_df(unique(pheno.data$year), find_lfd, df = pheno.data) %>%
  mutate(lf.duration = as.numeric(lf.end - lf.start))

# LF.DURATION <- lf %>%
#   group_by(plot) %>%
#   summarize(lf.dur.mean   = round(mean(lf.duration, na.rm = TRUE)),
#             lf.dur.median = round(median(lf.duration, na.rm = TRUE)))
round(mean(lf$lf.duration,   na.rm = TRUE))
# round(median(lf$lf.duration, na.rm = TRUE))
# boxplot(lf$lf.duration)
#
#  ggplot(lf, aes(x = year, y = lf.duration, color = plot)) +
#    geom_line() +
#    theme_hisafe_ts()

##### BUD BURST DURATION #####
find_bbd_p <- function(y, p, df) {
  ## BUD BURST START
  dat <- filter(df, year == y & plot == p & metric == "bud.burst" & !is.na(score))
  if(nrow(dat) > 2 & any(dat$score < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      bb.start.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      bb.start.date <- NA
    }
  } else {
    bb.start.date <- NA
  }

  ## BUD BURST END
  dat <- filter(df, year == y & plot == p & metric == "bud.burst" & !is.na(score3))
  if(nrow(dat) > 2 & any(dat$score3 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score3, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      bb.end.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      bb.end.date <- NA
    }
  } else {
    bb.end.date <- NA
  }
  out <- tibble(year      = y,
                plot      = p,
                bb.start = bb.start.date,
                bb.end   = bb.end.date)
  return(out)
}

find_bbd <- function(y, df) {
  ## BUD BURST START
  dat <- filter(df, year == y & metric == "bud.burst" & !is.na(score))
  if(nrow(dat) > 2 & any(dat$score < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      bb.start.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      bb.start.date <- NA
    }
  } else {
    bb.start.date <- NA
  }

  ## BUD BURST END
  dat <- filter(df, year == y & metric == "bud.burst" & !is.na(score3))
  if(nrow(dat) > 2 & any(dat$score3 < 0.5)) {
    dat.out <- as.data.frame(approx(dat$date, dat$score3, xout = seq(min(dat$date), max(dat$date), 1)))
    if(any(dat.out$y > 0.5)) {
      bb.end.date <- filter(dat.out, y > 0.5)$x[1]
    } else {
      bb.end.date <- NA
    }
  } else {
    bb.end.date <- NA
  }
  out <- tibble(year      = y,
                bb.start = bb.start.date,
                bb.end   = bb.end.date)
  return(out)
}

# search.df <- expand.grid(year = unique(pheno.data$year), plot = unique(pheno.data$plot))
# lf <- purrr::map2_df(search.df$year, search.df$plot, find_lfd, df = pheno.data) %>%
#   mutate(lf.duration = as.numeric(lf.end - lf.start))

bb <- purrr::map_df(unique(pheno.data$year), find_bbd, df = pheno.data) %>%
  mutate(bb.duration = as.numeric(bb.end - bb.start))

BB.DURATION <- bb %>%
  #group_by(plot) %>%
  summarize(bb.duration = round(mean(bb.duration, na.rm = TRUE)))
round(mean(bb$bb.duration, na.rm = TRUE))
round(median(bb$bb.duration, na.rm = TRUE))
boxplot(bb$bb.duration)

# ggplot(lf, aes(x = year, y = lf.duration, color = plot)) +
#   geom_line() +
#   theme_hisafe_ts()




##### GROWING SEASON PRECEIP #####
ggplot(pheno, aes(x = format(bud.burst, "%j"), y = season.duration, color = plot)) + geom_point()

gs.precip <- wth.raw %>%
  filter(month %in% 5:10) %>%
  group_by(year) %>%
  summarize(rain = sum(rain))

ws <- hop.p$trees %>%
  filter(phenologicalStage %in% 2) %>%
  #filter(Month %in% 10) %>%
  rename(year = Year) %>%
  group_by(year) %>%
  summarize(waterStress = sum(waterStress))

comp <- pheno %>%
  filter(plot == "Restinclieres-A2") %>%
  left_join(gs.precip, by = "year") %>%
  left_join(ws, by = "year")

ggplot(comp, aes(x = waterStress, y = season.duration)) +
  geom_point()
