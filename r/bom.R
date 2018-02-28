
#' Downloads a grid from BOM and returns a \code{sp::SpatialGridDataFrame} object
.downloadgrid.bom <- function(url, name, path_cache, path_7za, verbose = FALSE) {
  if(verbose) message(".downloadgrid.bom() ", name)
  # Create cache directory if missing
  if(!dir.exists(path_cache)) dir.create(path_cache, recursive = TRUE)
  # Define files paths for manipulation
  grid_path <- file.path(path_cache, paste0(name, ".grid"))
  grid_z_path <- file.path(path_cache, paste0(name, ".grid.z"))
  
  if(!file.exists(grid_path)) {
    # Download the file
    download_log <- purrr::safely(download.file)(url, destfile = grid_z_path, mode = "wb")
    # Check that the download completed successfully
    if(!is.null(download_log$error)) stop(download_log$error$message, call. = FALSE)
    # Uncompress the downloaded grid
    unzip_log <- processx::run(path_7za, c("e", "-y", paste0("-o", path_cache), grid_z_path), echo = verbose)
    if(unzip_log$status != 0) stop("Unzip command failed: ", unzip_log$stderr, "\n", unzip_log$stdout, call. = FALSE)
    # Cleanup
    file.remove(grid_z_path)
  }
  return(grid_path)
}

#' Query a grid for values as specific lat/lon coordinates
.querygrid.bom <- function(x, lat, lon, verbose = FALSE) {
  if(length(lat) != length(lon)) stop("Mismatched number of lat-lon values", call. = FALSE)
  if(length(lat) == 0) stop("Missing lat-lon values", call. = FALSE)
  
  if(is.na(x)) { return(rep(NA, length(lat))) }
  grid <- rgdal::readGDAL(x, silent = !verbose)
  values <- raster::extract(raster::raster(grid), data.frame(lon, lat))
  return(values)
}

#' Calculate the location-specific solar exposure for specific days as published
#' by bom.gov.au.
#' 
#' @param x A vector of \code{POSIXct} dates to calculate solar exposure.
#' @param lat A numeric vector of latitude values in the range +-90.0
#' @param lon A numeric vector of longitude values in the range +-180.0
#' @param label A character vector of names for each lat-lon pair
#' @param path_cache Relative path to directory to store/read cached grid files
#' @param path_7za Relative path to 7za binary 
#' @param url Template URL path for download
#' @param verbose Flag for additional debugging/progress info
#' 
#' @return A data.frame of calculated solar exposure for each location and date 
#'   in both MJ and KWh units.
#'   date | location | MJ | KWh
#'   
#' @export

solarexposure.bom <- function(
  x, 
  lat, lon, label = NULL,
  path_cache = file.path(".cache-solar-bom.gov.au"), 
  path_7za = file.path("7zip", "7za"),
  url = "http://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/{yyyymmdd}{yyyymmdd}.grid.Z",
  verbose = FALSE) 
{
  if(verbose) message("solarexposure.bom()")
  # Validate location
  if(length(lat) != length(lon)) stop("Mismatched number of lat-lon values", call. = FALSE)
  if(length(lat) == 0) stop("Missing lat-lon values", call. = FALSE)
  
  # Validate labels
  if(!is.null(label)) { 
    if(length(label) != length(lat)) stop("Mismatched number of labels for locations", call. = FALSE) 
  } else { 
    # Generate labels as sequential number
    label <- paste0("location-", 1:length(lat))
  }
  
  # Convert date values to formatted strings
  dates <- format(x, "%Y%m%d")
  
  # Replace url placholders with date strings
  urls <- stringr::str_replace_all(url, stringr::fixed("{yyyymmdd}"), dates)
  
  # Download urls and return local paths
  grid_paths <- purrr::map2(
    urls, 
    dates, 
    function(url, date, path_cache, path_7za, verbose) {
      download <- purrr::safely(.downloadgrid.bom)(url, date, path_cache, path_7za, verbose)
      if(!is.null(download$error)) {
        warning(".downloadgrid.bom() Failed to download grid at date: ", date, "\n", download$error, noBreaks. = FALSE, call. = FALSE)
        return(as.character(NA))
      }
      return(download$result)
    }, 
    path_cache = path_cache, 
    path_7za = path_7za, 
    verbose = verbose
  )

  # Load grids and extract values at coordinates
  values <- purrr::map(
    grid_paths,
    .querygrid.bom, 
    lat = lat,
    lon = lon,
    verbose = verbose
  )
  # Add location names and convert units
  values <- purrr::map2(dates, values, function(date, vals, names, lat, lon) { data.frame(date = date, label = names, lat = lat, lon = lon, MJ_m2 = vals, KWh_m2 = vals * 0.2777778, stringsAsFactors = FALSE) }, names = label, lat = lat, lon = lon)
  names(values) <- dates
  # Merge results and return with date columns
  result <- dplyr::bind_rows(values)
  return(result)
}


.download.bom_solarstn <- function(stn, dst = "data/bom", path_7za = "7zip/7za") {
  if(!dir.exists(dst)) dir.create(dst, recursive = TRUE)
  # Append the station number to the URL
  url <- paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=193&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", stn)
  # Load the URL and parse html response for download links
  html <- xml2::read_html(url)
  href_downloads <- rvest::html_nodes(html, xpath = "//ul[@class='downloads']//a")
  if(length(href_downloads) != 3) stop("Unhandled html response, expecting 3 download links")
  # Extract the URL link and write to disk
  url_download <- paste0("http://www.bom.gov.au", rvest::html_attr(href_downloads[[2]], "href"))
  httr::GET(url_download, httr::write_disk(file.path(dst, "bom-solarexposure.zip"), overwrite = TRUE))
  # Read data from zipped CSV
  data <- readr::read_csv(
    file.path(dst, "bom-solarexposure.zip"), 
    col_types = readr::cols_only(
      Year = readr::col_integer(),
      Month = readr::col_integer(),
      Day = readr::col_integer(), 
      'Daily global solar exposure (MJ/m*m)' = readr::col_double())
    )
  # Cleanup downloaded file(s)
  file.remove(file.path(dst, "bom-solarexposure.zip"))
  # Reformat loaded data
  data %>% dplyr::mutate(date = lubridate::ymd(paste0(Year, "-", Month, "-", Day))) %>% dplyr::select(date, se_mj = 'Daily global solar exposure (MJ/m*m)')
}

radar.bom <- function(id, when) {
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802261050.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802261050.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802261040.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802261040.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802261030.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802261030.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802261020.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802261020.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802261010.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802261010.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802261000.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802261000.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260950.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260950.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260940.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260940.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260930.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260930.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260920.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260920.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260910.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260910.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260900.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260900.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260850.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260850.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260840.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260840.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260830.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260830.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260820.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260820.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260810.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260810.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260800.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260800.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260750.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260750.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260740.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260740.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260730.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260730.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260720.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260720.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260710.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260710.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260700.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260700.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260650.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260650.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260640.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260640.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260630.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260630.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260620.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260620.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260610.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260610.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260600.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260600.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260550.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260550.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260540.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260540.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260530.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260530.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260520.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260520.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260510.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260510.png", mode = "wb")
  download.file("http://www.bom.gov.au/radar/IDR503.T.201802260500.png", destfile = ".cache-radar-bom.gov.au/IDR503.T.201802260500.png", mode = "wb")
}
  