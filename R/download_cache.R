#' This function will download files necessary to run the model and store them in the package space
#' 
#' @param name location of file to download
#' @param redownload bool - whether or not to redownload model weights from online storage
#' 
#' @import rappdirs
#' @import fs
#' @import googledrive
#' 
#' @export
#' 
download_cache <- function(name = "weights_family_cpu.pth", redownload = TRUE)
  {

  # set cache destination
  cache_path <- rappdirs::user_cache_dir("CameraTrapDetector")
  fs::dir_create(cache_path)
  
  # create a file path
  path <- file.path(cache_path, fs::path_file(name))
  
  if (!file.exists(path) | redownload==TRUE){
    
    # access the model weights/architecture files
    s_path <- file.path(.libPaths()[1], "CameraTrapDetectoR/wts/model-wts-svc-acct.json")   # access service acct credentials
    googledrive::drive_auth(path = s_path)
    
    # download selected file; overwrite any existing version in case weights have been updated
    cat(paste0("downloading ", fs::path_file(name)), " file\n")
    googledrive::drive_download(name, path=path, overwrite = TRUE)
    
  }


  return(path)
}