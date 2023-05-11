#' Download model files
#'
#' @description Download files necessary to run the model and store them in
#' the package cache. Helper function for `deploy_model`
#' 
#' @param model_type user-specified model type and version. Defaults to 
#' latest version of model type if no version given
#' 
#' @returns path to folder containing model files
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
  if(model_type %in% c('general', 'general_v1')) {
    path <- file.path(cache_path, fs::path_file("general_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("general_v1"))
    agdata <- "https://data.nal.usda.gov/system/files/general_v1_1.zip"
  }
  if(model_type %in% c('family', 'family_v1')) {
    path <- file.path(cache_path, fs::path_file("family_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("family_v1"))
    agdata <- "https://data.nal.usda.gov/system/files/family_v1_1.zip"
  }
  if(model_type %in% c('pig_only', 'pig_only_v1')) {
    path <- file.path(cache_path, fs::path_file("pig_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("pig_v1"))
    agdata <- "https://data.nal.usda.gov/system/files/pig_v1_1.zip"
  }
  if(model_type %in% c('species', 'species_v2')) {
    path <- file.path(cache_path, fs::path_file("species_v2.zip"))
    folder <- file.path(cache_path, fs::path_file("species_v2"))
    agdata <- "https://data.nal.usda.gov/system/files/species_v2_3.zip"
  }
  if(model_type == 'species_v1') {
    path <- file.path(cache_path, fs::path_file("species_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("species_v1"))
    agdata <- "https://data.nal.usda.gov/system/files/species_v1_1.zip"
  }
  
  
  if (!dir.exists(folder)){
    
    # download zip file from AG Data
    cat(paste0("Downloading model weights, architecture, and labels.\n
               If your download times out before completion, try disconnecting from VPN and 
               running the function again.\n"))
    utils::download.file(url=agdata, destfile=path, quiet=TRUE)
    
    # unzip the folder
    utils::unzip(zipfile=path, exdir=cache_path)
    
    # delete zipped folder
    file.remove(path)
  }
  
  cat(paste0("Model files are downloaded and stored.\n"))


  return(folder)
}
