#' This function will download files necessary to run the model and store them in the package space
#' 
#' @param url location of file to download
#' 
#' @import rappdirs
#' @import fs
#' 
#' @export
#' 
download_cache <- function(url = "https://www.dropbox.com/s/ap2pbut2eycibw0/weights_family_cpu.pth?dl=0")
  {

  # set destination
  cache_path <- rappdirs::user_cache_dir("CameraTrapDetector")
  fs::dir_create(cache_path)
  
  # create a file path
  path <- file.path(cache_path, gsub("\\?dl=0", "", fs::path_file(url)))
  
  # download file if it doesn't already exist:
  if (!file.exists(path)) {
    cat(paste0("downloading ", fs::path_file(url)), " file\n")
    utils::download.file(url, path, mode = "wb")
  }


  return(path)
}