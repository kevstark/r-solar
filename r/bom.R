config <- yaml::yaml.load_file("config.yml")

download.bom_solar <- function(x = Sys.Date(), dst = "data", path_7za = "7zip/x64/7za") {
  if(!dir.exists(dst)) dir.create(dst, recursive = TRUE)
  grid_path <- dir(path = dst, pattern = paste0(format(x, "%Y%m%d"), ".grid"), full.names = TRUE)
  if(!file.exists(grid_path)) {
    url <- paste0("http://www.bom.gov.au/web03/ncc/www/awap/solar/solarave/daily/grid/0.05/history/nat/", format(x, "%Y%m%d%Y%m%d"), ".grid.Z")
    download_path <- file.path(dst, paste0(format(x, "%Y%m%d"), ".grid.Z"))
    
    # Download the file
    download_log <- purrr::safely(download.file)(url = url, destfile = download_path, mode = "wb")
    # Check that the download completed successfully
    if(!is.null(download_log$error)) stop(download_log$error$message, call. = FALSE)
    
    # Uncompress the downloaded grid
    unzip_cmd <- paste0(path_7za, " e -y -o\"", dst, "\" \"", download_path, "\"")
    unzip_log <- system(command = unzip_cmd, intern = TRUE)
    if(!is.null(attributes(unzip_log))) stop("Unzip command failed: ", unzip_cmd, call. = FALSE)
    
    # Cleanup
    file.remove(download_path)
    grid_path <- dir(path = dst, pattern = paste0(format(x, "%Y%m%d"), ".grid"), full.names = TRUE)
  }
  # Return unzipped files
  rgdal::readGDAL(fname = grid_path)
}
