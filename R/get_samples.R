#' Extract example image set
#' 
#' @description Load sample image set to use in examples. Folder also contains 
#' ground truth labels and predictions from the species_v2 model. 
#' 
#' @import rappdirs
#' @import fs
#' 
#' @export
#' 
get_samples <- function() {
  
  # get dir to sample images
  img_dir <- system.file("extdata", package = "CameraTrapDetectoR")
  
  # define absolute path to example folder
  path2images <- paste(img_dir, dir(img_dir), sep = "/")
  
  # set cache destination
  dest_dir <- rappdirs::user_cache_dir("CameraTrapDetector")

  # create dir to hold examples
  fs::dir_create(dest_dir)
    
  # extract folder
  utils::unzip(zipfile=path2images, exdir = dest_dir, overwrite = TRUE)
  
  # set path to sample dir
  sample_dir <- paste0(dest_dir, "/example_set")
  
  return(sample_dir)
}
