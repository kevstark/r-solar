# analysis.R

if(!"tidyverse" %in% library()$result[, 1]) install.packages("tidyverse")
if(!"processx"  %in% library()$result[, 1]) install.packages("processx")

library(dplyr)
library(magrittr)
library(ggplot2)
library(processx)

if(!"suncalc"   %in% library()$result[, 1]) install.packages("suncalc")

source(file.path(here::here(), "r", "bom.R"))
source(file.path(here::here(), "r", "ecas.R"))

config <- yaml::yaml.load_file("config.yml")

for (site in config$sites) {
  message(site$label)

  # Load ecas dataset
  ecas <- read.ecas(site$csv)
  message(paste0(capture.output(summary(ecas)), collapse = "\n"))

  start <- lubridate::date(min(ecas$timestamp))
  end <- lubridate::date(max(ecas$timestamp))
  # Array of all dates within logged dataset
  dates <- start + seq(0, as.numeric(end - start))
  
  times <- suncalc::getSunlightTimes(dates, lat = site$lat, lon = site$lon, keep = c("sunrise", "sunset"))
  times %<>% mutate(daylight = as.numeric(sunset - sunrise)) 
  
  # Calculate total solar exposure for days within range
  se <- solarexposure.bom(dates, lat = site$lat, lon = site$lon, label = site$label, verbose = TRUE)
  
}


