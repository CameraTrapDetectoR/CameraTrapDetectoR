#' This function will download files necessary to run the model and store them in the package space
#' 
#' @param url location of file to download
#' 
#' @import rappdirs
#' @import fs
#' 
#' @export
#' 
download_cache <- function(url = "https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/fasterrcnnArch_33classes.pt")
  {

  # set destination
  cache_path <- rappdirs::user_cache_dir("CameraTrapDetector")
  fs::dir_create(cache_path)
  
  # create a file path
  path <- file.path(cache_path, fs::path_file(url))
  
  # download file
  cat(paste0("downloading ", fs::path_file(url)), " file\n")
  utils::download.file(url, path, mode = "wb")

  return(path)
}