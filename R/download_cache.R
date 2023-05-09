#' Download model files
#' 
#' This function will download files necessary to run the model and store them in the package space
#' 
#' @param model_type
#' 
#' @import rappdirs
#' @import fs
#' 
#' @export
#' 
download_cache <- function(model_type = "species")
  {

  # set cache destination
  cache_path <- rappdirs::user_cache_dir("CameraTrapDetector")
  fs::dir_create(cache_path)
  
  # create a file path depending on model type
  if(model_type == 'general' | 'general_v1') {
    path <- file.path(cache_path, fs::path_file("general_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("general_v1"))
    agdata <- "link_to_zip_folder_on_AG_Data_Commons"
  }
  if(model_type == 'family' | 'family_v1') {
    path <- file.path(cache_path, fs::path_file("family_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("family_v1"))
    agdata <- "link_to_zip_folder_on_AG_Data_Commons"
  }
  if(model_type == 'pig_only' | 'pig_only_v1') {
    path <- file.path(cache_path, fs::path_file("pig_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("pig_v1"))
    agdata <- "link_to_zip_folder_on_AG_Data_Commons"
  }
  if(model_type == 'species' | 'species_v2') {
    path <- file.path(cache_path, fs::path_file("species_v2.zip"))
    folder <- file.path(cache_path, fs::path_file("species_v2"))
    agdata <- "link_to_zip_folder_on_AG_Data_Commons"
  }
  if(model_type == 'species_v1') {
    path <- file.path(cache_path, fs::path_file("species_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("species_v1"))
    agdata <- "link_to_zip_folder_on_AG_Data_Commons"
  }
  
  
  if (!dir.exists(folder) | redownload==TRUE){
    
    # access the model weights/architecture files
    # s_path <- file.path(.libPaths()[1], "CameraTrapDetectoR/wts/model-wts-svc-acct.json")   # access service acct credentials
    # googledrive::drive_auth(path = s_path)
    
    # download zip file from AG Data
    cat(paste0("downloading model weights, arch, and labels.\n"))
    # googledrive::drive_download(name, path=path, overwrite = TRUE)
    utils::download.file(url=agdata, destfile=path, quiet=TRUE)
    
    # unzip the folder
    utils::unzip(zipfile=path, exdir=cache_path)
  }


  return(folder)
}