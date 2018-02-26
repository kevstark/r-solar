# ecas.R

read.ecas <- function(x) {
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
  return(data)
}

