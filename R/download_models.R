#' Download model files. 
#'
#' @description Download files necessary to run the model and store them in
#' the package cache. May be run by the user to download models prior to running
#' `deploy_model`. Accepts a character string or vector of character strings 
#' associated with model types.
#' 
#' @param models user-specified model or list of models. Defaults to 
#' latest version of model type if no version given
#' 
#' @returns path to folder containing model files
#' 
#' @import rappdirs
#' @import fs
#' @import usethis
#' 
#' @export
#' 
download_models <- function(models = "species_v2")
  {

  # set cache destination
  cache_path <- rappdirs::user_cache_dir("CameraTrapDetector")
  fs::dir_create(cache_path)
  
  ## Loop through models if multiple versions are fed into the script:
  
  for(i in 1:length(models)) {
    
    # define model files being accessed
    model_i <- models[i]
    
    # define latest model generation - manually update with each deployment
    latest <- "v2"
    if(model_i %in% c('general', 'family', 'species', 'pig_only')){
      model_i <- paste(model_i, latest, sep="_")}
    
    # define file path depending on model version
    if(grepl("general", model_i, ignore.case = TRUE)) {
      if(grepl("general_v1", model_i, ignore.case = TRUE)){
        path <- file.path(cache_path, fs::path_file("general_v1_1.zip"))
        folder <- file.path(cache_path, fs::path_file("general_v1"))
        #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576746"
        agdata <- "https://bit.ly/4c2Ykz2"
      } else {
        path <- file.path(cache_path, fs::path_file("general_v2.zip"))
        folder <- file.path(cache_path, fs::path_file("general_v2"))
        #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576752"
        agdata <- "https://bit.ly/4c42iHt"
      }
      
    }
    if(grepl("family", model_i, ignore.case = TRUE)) {
      if(grepl("family_v1", model_i, ignore.case = TRUE)){
        path <- file.path(cache_path, fs::path_file("family_v1_1.zip"))
        folder <- file.path(cache_path, fs::path_file("family_v1"))
        #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576266"
        agdata <- "https://bit.ly/438ZCUU"
      } else {
        path <- file.path(cache_path, fs::path_file("family_v2.zip"))
        folder <- file.path(cache_path, fs::path_file("family_v2"))
        #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576296"
        agdata <- "https://bit.ly/3T3Yhu1"
      }
      
    }
    if(grepl("pig", model_i, ignore.case = TRUE)) {
      path <- file.path(cache_path, fs::path_file("pig_v1_0.zip"))
      folder <- file.path(cache_path, fs::path_file("pig_v1"))
      #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576767"
      agdata <- "https://bit.ly/438zJ7J"
    }
    if(grepl("species", model_i, ignore.case = TRUE)) {
      if(grepl("species_v1", model_i, ignore.case = TRUE)) {
        path <- file.path(cache_path, fs::path_file("species_v1_1.zip"))
        folder <- file.path(cache_path, fs::path_file("species_v1"))
        #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576215"
        agdata <- "https://bit.ly/3T4srgM"
      }
      else {
        path <- file.path(cache_path, fs::path_file("species_v2_3.zip"))
        folder <- file.path(cache_path, fs::path_file("species_v2"))
        #agdata <- "https://agdatacommons.nal.usda.gov/ndownloader/files/44576230"
        agdata <- "https://bit.ly/ctdv2sp"
      }
      
    }
    
    # unzip the folder if it's in the cache
    if(file.exists(path)){
      # unzip the folder
      utils::unzip(zipfile=path, exdir=cache_path)
    }
    
    if (!dir.exists(folder)){
      
      #print timeout message only once
      if(i == 1) {
        cat(paste0("Downloading model weights, architecture, and labels for model version ",
                   model_i,
                   " .\nIf your download times out before completion, try disconnecting from VPN 
               and/or increasing your timeout option and running the function again.\n\n"))
      }
      
      # download and unzip model files
      #utils::download.file(url=agdata, destfile=path, quiet=TRUE)
      tryCatch(usethis::use_zip(agdata, destdir = cache_path), 
               error = function(e) {utils::unzip(path, exdir = cache_path)})
      
      # once we confirm download, remove zip file
        file.remove(path)
    }
    
    cat(paste0("Model files for model version ", model_i, " are downloaded and stored.\n\n"))
  }
  
  return(folder)
}
