# analysis.R

# Install missing packages
if(!"tidyverse" %in% library()$result[, 1]) install.packages("tidyverse")
if(!"processx"  %in% library()$result[, 1]) install.packages("processx")
if(!"suncalc"   %in% library()$result[, 1]) install.packages("suncalc")

# Load common packages
library(dplyr, warn.conflicts = FALSE)
library(magrittr)
library(ggplot2)
library(processx)
library(lubridate)

# Load component scripts
source(file.path(here::here(), "r", "bom.R"))
source(file.path(here::here(), "r", "ecas.R"))

# Load config file
config <- yaml::yaml.load_file("config.yml")
message("config:\n  ", paste0(names(config), collapse = "\n  "))
message("config$sites[", length(config$sites), "]")

# Process each site contained within
for (site in config$sites) {
  message("  site: ", site$label)

  if(!site$tz %in% OlsonNames()) stop("Unrecognised timezone: '", site$tz, "'. See `OlsenNames() for valid entries with format 'Country/City'")
  
  # Load ecas dataset
  ecas <- read.ecas(site$csv, tz = site$tz)
  message(paste0(capture.output(summary(ecas)), collapse = "\n"))
  # ggplot() + geom_line(aes(x = ecas$timestamp, y = ecas$value))
  
  # Array of all dates within logged dataset
  dates <- date.fill(ecas$timestamp)
  
  ####
  # Calculate/estimate/normalise ecas readings to once per day at midnight
  ecas_interp <- data.frame(
    timestamp = lubridate::force_tz(lubridate::as_datetime(dates), site$tz),
    value = purrr::map_dbl(
      lubridate::force_tz(lubridate::as_datetime(dates), site$tz) + lubridate::seconds(86399), 
      interpolate_solar.ecas,
      x = ecas, 
      lat = site$lat,
      lon = site$lon),
    stringsAsFactors = FALSE)
  
  
  # ggplot() + 
  #   geom_line(aes(x = ecas$timestamp, y = ecas$value), col = "black") + 
  #   geom_line(aes(x = ecas_readings$timestamp, y = ecas_readings$value), col = "red") + 
  #   xlab("timestamp") + ylab("value") 
  
  solar <- data.frame(date = dates, stringsAsFactors = FALSE)
  solar$ecas <- ecas_interp$value - dplyr::lag(ecas_interp$value)
  
  bom <- solarexposure.bom(
    x = dates, 
    lat = site$lat, lon = site$lon, 
    label = site$label)

  solar$bom <- bom$KWh_m2 * site$panel_m2 
  
  # Calculate daily production from ecas
  # Calculate daily potential from bom
  ####
  
  solar %>% 
    mutate(efficiency = (ecas / bom) * 100) %>% 
    ggplot(aes(x = date, y = efficiency)) + 
    geom_line(col = "grey") + 
    geom_smooth()
  
}


