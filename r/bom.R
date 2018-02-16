x <- lubridate::ymd("20160223")
download.bom_solargrid <- function(x = Sys.Date(), dst = file.path("data", "bom"), path_7za = file.path("7zip", "7za"), verbose = FALSE) {
  if(verbose) message("download.bom_solargrid() ", format(x, "%Y%m%d"))
  if(!dir.exists(dst)) dir.create(dst, recursive = TRUE)
  grid_path <- file.path(dst, paste0(format(x, "%Y%m%d"), ".grid"))
  if(verbose) message("  grid_path: ", grid_path)
  if(!file.exists(grid_path)) {
    url <- paste0("http://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/", format(x, "%Y%m%d%Y%m%d"), ".grid.Z")
    download_path <- file.path(dst, paste0(format(x, "%Y%m%d"), ".grid.Z"))
    if(verbose) message("  download_path: ", download_path)
    
    # Download the file
    download_log <- purrr::safely(download.file)(url = url, destfile = download_path, mode = "wb")
    # Check that the download completed successfully
    if(!is.null(download_log$error)) stop(download_log$error$message, call. = FALSE)
    if(verbose) message("  downloaded: ", gdata::humanReadable(file.size(download_path)))
    
    # Uncompress the downloaded grid
    unzip_log <- processx::run(path_7za, c("e", "-y", paste0("-o", dst), download_path), echo = verbose)
    if(unzip_log$status != 0) stop("Unzip command failed: ", unzip_log$stderr, call. = FALSE)
    if(verbose) message("  unzipped: ", gdata::humanReadable(file.size(grid_path)))
    
    # Cleanup
    file.remove(download_path)
  }
  if(verbose) message("  readGDAL()")
  # Return unzipped files
  rgdal::readGDAL(fname = grid_path)
}

download.bom_solarstn <- function(stn, dst = "data/bom", path_7za = "7zip/7za") {
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