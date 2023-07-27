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
  if(grepl("general", model_type, ignore.case = TRUE)) {
    if(grepl("general_v1", model_type, ignore.case = TRUE)){
      path <- file.path(cache_path, fs::path_file("general_v1.zip"))
      folder <- file.path(cache_path, fs::path_file("general_v1"))
      agdata <- "https://data.nal.usda.gov/system/files/general_v1_1.zip"
    } else {
      path <- file.path(cache_path, fs::path_file("general_v2.zip"))
      folder <- file.path(cache_path, fs::path_file("general_v2"))
      agdata <- "https://data.nal.usda.gov/system/files/general_v2.zip"
    }

  }
  if(grepl("family", model_type, ignore.case = TRUE)) {
    path <- file.path(cache_path, fs::path_file("family_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("family_v1"))
    agdata <- "https://data.nal.usda.gov/system/files/family_v1_1.zip"
  }
  if(grepl("pig", model_type, ignore.case = TRUE)) {
    path <- file.path(cache_path, fs::path_file("pig_v1.zip"))
    folder <- file.path(cache_path, fs::path_file("pig_v1"))
    agdata <- "https://data.nal.usda.gov/system/files/pig_v1_0.zip"
  }
  if(grepl("species", model_type, ignore.case = TRUE)) {
    if(grepl("species_v1", model_type, ignore.case = TRUE)) {
      path <- file.path(cache_path, fs::path_file("species_v1.zip"))
      folder <- file.path(cache_path, fs::path_file("species_v1"))
      agdata <- "https://data.nal.usda.gov/system/files/species_v1_1.zip"
    }
    else {
      path <- file.path(cache_path, fs::path_file("species_v2.zip"))
      folder <- file.path(cache_path, fs::path_file("species_v2"))
      agdata <- "https://data.nal.usda.gov/system/files/species_v2_3.zip"
    }

  }
  
  # unzip the folder if it's in the cache
  if(file.exists(path)){
    # unzip the folder
    utils::unzip(zipfile=path, exdir=cache_path)
    
    # remove zip folder once it's been extracted
    file.remove(path)
  }
  
  if (!dir.exists(folder)){
    
    # download zip file from AG Data
    cat(paste0("Downloading model weights, architecture, and labels.\n
               If your download times out before completion, try disconnecting from VPN and 
               running the function again.\n"))
    utils::download.file(url=agdata, destfile=path, quiet=TRUE)
    
    if(file.exists(path)){
      # unzip the folder
      utils::unzip(zipfile=path, exdir=cache_path)
      
      # remove zip folder once it's been extracted
      file.remove(path)
    }
    
  }

  
  cat(paste0("Model files are downloaded and stored.\n"))
  
  
  return(folder)
}
