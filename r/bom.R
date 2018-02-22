
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
.querygrid.bom <- function(x, loc, verbose = FALSE) {
  if(is.na(x)) { return(rep(NA, nrow(loc))) }
  grid <- rgdal::readGDAL(x, silent = !verbose)
  values <- raster::extract(raster::raster(grid), data.frame(loc$lon, loc$lat))
  return(values)
}

#' Calculate the location-specific solar exposure for specific days as published
#' by bom.gov.au.
#' 
#' @param x A vector of \code{POSIXct} dates to calculate solar exposure.
#' @param loc A single numeric pair of lat-lon values, or a \code{data.frame} 
#'   with columns \code{lat} and \code{lon} for multiple locations.
#' 
#' @return A data.frame of calculated solar exposure for each location and date 
#'   in both MJ and KWh units.
#'   date | location | MJ | KWh
#'   
#' @export
solarexposure.bom <- function(
  x, 
  loc,
  path_cache = file.path(".cache-solar-bom.gov.au"), 
  path_7za = file.path("7zip", "7za"),
  url = "http://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/{yyyymmdd}{yyyymmdd}.grid.Z",
  verbose = FALSE) 
{
  if(verbose) message("solarexposure.bom()")
  # Validate loc values
  if(inherits(loc, "data.frame")) {
    if(!all(c("lat", "lon") %in% colnames(loc))) stop("Missing columns: expected numeric columns named 'lat' and 'lon'.")
  } else if(inherits(loc, "numeric")) {
    if(!all(c("lat", "lon") %in% names(loc))) stop("Missing names: expected numeric vector with named values 'lat' and 'lon'.")
    # Convert to data.frame
    loc <- data.frame(lat = loc["lat"], lon = loc["lon"])
  } else { stop("Unhandled 'loc' of type '", class(loc), "'. Expected data.frame or numeric vector.", call. = FALSE) }
  # Add row-names if not defined
  if(is.null(rownames(loc))) { rownames(loc) <- paste0("location-", 1:nrow(loc)) }
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
    loc = loc,
    verbose = verbose
  )
  # Add location names and convert units
  values <- purrr::map2(dates, values, function(date, vals, names) { data.frame(date = date, loc = names, MJ_m2 = vals, KWh_m2 = vals * 0.2777778, stringsAsFactors = FALSE) }, names = rownames(loc))
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