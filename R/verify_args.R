#' verify arg list
#' 
#' @description
#' verify `deploy_model` arguments
#' 
#'
#' @param arg_list 
#'
#' @return verified list
#' @export
#'
#' @examples
verify_args <- function(arg_list) {
  
  #-- Check arguments provided 
  
  # check model_type
  models_available <- c('general', 'general_v1', 'general_v2',
                        'species', 'species_v1', 'species_v2',
                        'family', 'family_v1', 'family_v2',
                        'pig_only', 'pig_only_v1')
  if(!arg_list$model_type %in% models_available) {
    stop(paste0("model_type must be one of the available options: ",
                list(models_available)))
  }
  
  # define model version
  arg_list$model_version <- arg_list$model_type
  latest <- "v2"   # latest model generation
  if(arg_list$model_version %in% c('general', 'family', 'species', 'pig_only')){
    arg_list$model_version <- paste(arg_list$model_version, latest, sep="_")
  }
  
  # check ext types
  acceptable_exts <- c(".jpg", ".png", ".tif", ".pdf",
                       ".JPG", ".PNG", ".TIF", ".PDF")
  extension_test <- arg_list$file_extensions %in% acceptable_exts
  if(!all(extension_test)){
    stop(paste0(c("One or more of the `file_extensions` specified is not an accepted format. Please choose one of the accepted formats: \n",
                  acceptable_exts), collapse = " "))
  }
  
  # test overlap_threshold
  if (arg_list$overlap_threshold < 0 | arg_list$overlap_threshold > 1){
    stop("overlap_threshold must be between [0, 1]")
  }
  
  # test score_threshold
  if (arg_list$score_threshold < 0 | arg_list$score_threshold > 1){
    stop("score_threshold must be between [0, 1]")
  }
  
  # test checkpoint frequency
  if (arg_list$checkpoint_frequency <= 0) {
    stop("checkpoint frequency must be a positive integer.")
  }
  if (arg_list$checkpoint_frequency %% 1 != 0) {
    stop("checkpoint frequency must be a positive integer.")
  }
  
  # test review_threshold
  if (arg_list$review_threshold < 0 | arg_list$review_threshold > 1){
    stop("review_threshold must be between [0, 1]")
  }
  
  # check location arguments
  if (!is.na(arg_list$latitude)) {
    if (arg_list$latitude < -90 | arg_list$latitude > 90){
      stop("latitude must be between -90 and 90")
    } 
  }
  if (!is.na(arg_list$longitude)) {
    if (arg_list$longitude < -180 | arg_list$latitude > 180) {
      stop("longitude must be between -180 and 180")
    }
  }
  if (is.na(arg_list$latitude) & !is.na(arg_list$longitude)){
    stop("invalid location; please include both latitude and longitude or leave both blank")
  }
  
  if (!is.na(arg_list$latitude) & is.na(arg_list$longitude)){
    stop("invalid location; please include both latitude and longitude or leave both blank")
  }
  
  # test lty 
  lty_options <- 1:6
  if(!arg_list$lty %in% lty_options){
    stop("invalid lty option selected. Please select an integer from 1-6")
  }
  
  # test color
  tryCatch({grDevices::col2rgb(arg_list$col)}, error=function(e) {
    print('col value entered is not a valid value')})
  
  # test lwd
  if (arg_list$lwd <= 0){
    stop("lwd value must be greater than 0")
  }
  
  return(arg_list)
}