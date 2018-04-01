# ecas.R
if(!"suncalc"   %in% library()$result[, 1]) install.packages("suncalc")

read.ecas <- function(x, tz = NULL) {
  if(!require('readr')) stop("'readr' package is not available, use install.packages('tidyverse')", call. = FALSE)
  if(!require('dplyr')) stop("'dplyr' package is not available, use install.packages('tidyverse')", call. = FALSE)
  if(!require('lubridate')) stop("'lubridate' package is not available, use install.packages('tidyverse')", call. = FALSE)
  
  data <- readr::read_csv(
    x, 
    col_names = c("day", "time", "value", "label"), 
    col_types = readr::cols(
      day = readr::col_character(),
      time = readr::col_character(), 
      value = readr::col_number(),
      label = readr::col_character()
    ))
  data <- dplyr::mutate(data, timestamp = lubridate::ymd_hms(paste0(day, " ", time)))
  if(is.null(tz)) {
    warning("'tz' not specified, assuming Sys.timesone() '", Sys.timezone(TRUE), "'")
    data <- dplyr::mutate(data, timestamp = lubridate::force_tz(timestamp, Sys.timezone(TRUE)))
    lubridate::tz(data$timestamp) <- Sys.timezone(TRUE)
  } else {
    data <- dplyr::mutate(data, timestamp = lubridate::force_tz(timestamp, tz))
    lubridate::tz(data$timestamp) <- tz
  }
  return(data)
}

#' Takes two or more date/time objects and returns a vector of all dates within the range of values.
date.fill <- function(x) {
  if(!require('lubridate')) stop("'lubridate' package is not available, use install.packages('tidyverse')", call. = FALSE)
  
  x_dates <- as.Date(x, tz = lubridate::tz(x))  
  return(range(x_dates)[1] + seq(from = 0, to = range(x_dates)[2] - range(x_dates)[1]))
}

#' Esimate the value of a solar power meter at a given date time, based on historical readings.
#' Uses the `suncalc` library to limit production times, and assumes `sin` wave production during daylight hours.
interpolate_solar.ecas <- function(x, at, lat, lon, verbose = FALSE) {
  if(!require('suncalc')) stop("'suncalc' package is not available, use install.packages('suncalc')", call. = FALSE)
  if(!require('purrr')) stop("'purrr' package is not available, use install.packages('tidyverse')", call. = FALSE)
  if(!require('dplyr')) stop("'dplyr' package is not available, use install.packages('tidyverse')", call. = FALSE)
  if(!require('magrittr')) stop("'magrittr' package is not available, use install.packages('tidyverse')", call. = FALSE)
  if(!require('lubridate')) stop("'lubridate' package is not available, use install.packages('tidyverse')", call. = FALSE)
  if(!require('stats')) stop("'magrittr' package is not available, use install.packages('tidyverse')", call. = FALSE)
  
  if(verbose) message("interpolate_solar.ecas() x[", paste0(dim(x), collapse = " x "), "] at=", at)
  
  if(at < min(x$timestamp)) return(x$value[1])
  if(at > max(x$timestamp)) return(x$value[nrow(x)])
  
  # Capture readings before and after 'at' time
  x_at <- x[c(max(which(x$timestamp <= at)), min(which(x$timestamp >= at))), ]
  if(verbose) message("  x_at[", paste0(dim(x_at), collapse = " x "), "]")
  if(verbose) message(paste0(capture.output(print(as.data.frame(x_at))), collapse = "\n"))
  
  # Extract sun times for days spanning readings
  suntimes_at <- suncalc::getSunlightTimes(date.fill(x_at$timestamp), lat, lon, tz = lubridate::tz(at), keep = c("sunrise", "sunset"))
  suntimes_at %<>% dplyr::mutate(daylight_hrs = as.numeric(difftime(sunset, sunrise, units = "hours"))) %>% select(-lat, -lon)
  # Number of daylight hours before and after ecas readings
  suntimes_at %<>% 
    dplyr::mutate(x_at_1_h = ifelse(x_at[1, ]$timestamp > sunrise, difftime(x_at[1, ]$timestamp, sunrise, units = "hours"), 0)) %>%
    dplyr::mutate(x_at_2_h = ifelse(x_at[nrow(x_at), ]$timestamp < sunset, daylight_hrs - as.numeric(difftime(sunset, x_at[nrow(x_at), ]$timestamp, units = "hours")), daylight_hrs))

  # Define a solar intensity curve where the total area under daylight hours == 1.0
  solar_f <- function(x) { (sin((x*(2*pi)) - pi / 2) + 1) }
  
  if(verbose) message("  suntimes_at[", paste0(dim(suntimes_at), collapse = " x "), "]")
  if(verbose) message(paste0(capture.output(print(as.data.frame(suntimes_at))), collapse = "\n"))
  
  # Total number of effective solar hours between readings (weighted by solar function)
  suntimes_at %<>% dplyr::mutate(
    solar_hrs = daylight_hrs * purrr::map2_dbl(
      (suntimes_at$x_at_1_h / suntimes_at$daylight_hrs), 
      (suntimes_at$x_at_2_h / suntimes_at$daylight_hrs), 
      ~ stats::integrate(solar_f, lower = .x, upper = .y)$value)
  )
  
  # Average rate between ecas readings bounding the requested time
  ecas_avg = (x_at[2, ]$value - x_at[1, ]$value) / sum(suntimes_at$solar_hrs)

  # Number of daylight hours between ecas readings and requested time
  suntimes_at %<>% 
    dplyr::mutate(x_at_h = ifelse(at >= lubridate::floor_date(sunrise, "day") & at < lubridate::ceiling_date(sunset, "day"), 
                           pmin(pmax(0, difftime(at, sunrise, units = "hours")), daylight_hrs), 
                           NA))
  
  # Solar hours at the time of 'at'
  suntimes_at %<>%
    dplyr::mutate(x_at_sh = daylight_hrs * purrr::map2_dbl(
      0, ifelse(is.na(suntimes_at$x_at_h), 0, (suntimes_at$x_at_h / suntimes_at$daylight_hrs)), 
      ~ stats::integrate(solar_f, lower = .x, upper = .y)$value)
    )
                  
  # Total solar hours up to 'at'
  suntimes_at %<>%
    dplyr::mutate(cuml_solar_hrs = cumsum(solar_hrs)) %>%
    dplyr::mutate(cuml_solar_hrs = ifelse(is.na(x_at_h), 0, x_at_sh + ifelse(is.na(dplyr::lag(cuml_solar_hrs)), 0, dplyr::lag(cuml_solar_hrs))))
  
  # Project ecas reading forward to 'at' time
  return(x_at$value[1] + ecas_avg * max(suntimes_at$cuml_solar_hrs))
}
