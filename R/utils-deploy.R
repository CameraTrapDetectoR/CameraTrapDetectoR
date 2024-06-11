# Verify Args -------------------------------------------------
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
  
}

# Load checkpoint --------------------------
load_checkpoint <- function(output_dir, model_version, file_list, type){
  
  # check for saved predictions/boxes
  results_path <- list.files(output_dir, pattern = paste(model_version, "model_predictions", sep = "_"), 
                             full.names = TRUE, ignore.case = TRUE)
  bbox_path <- list.files(output_dir, pattern = paste(model_version, "predicted_boxes", sep = "_"), 
                             full.names = TRUE, ignore.case = TRUE)
  
  # load saved predictions
  if(type == "preds"){
    if(length(results_path)>0){
      # read in csv files
      results <- do.call(rbind, lapply(results_path, utils::read.csv))
      
      # extract filenames 
      results_files <- unique(normalizePath(results$filename, winslash = "/"))
      
      # filter predictions out of file list
      chkpt <- file_list[!file_list %in% results_files]
      
      # exit function if all images have already been run
      if(length(chkpt) == 0){
        stop(print(paste0("All images in your chosen directory have already been run on the ",
                          model_version, " model. \nResults can be found at: ", output_dir, "/", model_version, "_model_predictions.csv",
                          "\nTo run the same model on the same images with different hyperparameters, please reset those parameters and leave the <output_dir> argument blank. 
          \nOtherwise, please choose another model or image directory.")))
      } else{
        # print message
        cat(paste0("\nLoading saved model results from ", output_dir, 
                 "\nModel will run only on images in not already in saved results. \n"))
      }
    }
  }
  
  # load saved bounding boxes
  if(type == "boxes"){
    if(length(bbox_path)>0){
      bboxes <- do.call(rbind, lapply(bbox_path, utils::read.csv))
      chkpt <- unique(bboxes)
      cat(paste0("\nLoading saved bbox results from ", output_dir, "\n"))
    }
  }
  
  return(chkpt)
}


# Set output directory ------------------------------
set_output_dir <- function(data_dir, model_version, recursive, make_plots){
  
  # make new output dir
  datenow <- format(Sys.Date(), "%Y%m%d")
  now <- unclass(as.POSIXlt(Sys.time()))
  current_time <- paste0("_", datenow, "_", sprintf("%02d", now$hour), 
                         sprintf("%02d", now$min), 
                         sprintf("%02d", round(now$sec)))
  output_dir <- file.path(data_dir, paste0("predictions_", model_version, current_time))
  dir.create(output_dir)
  
  # make recursive directories if needed
  if(recursive && make_plots) {
    rec_dirs <- list.dirs(data_dir, full.names = FALSE)
    for(i in 1:length(rec_dirs)){
      suppressWarnings(
        dir.create(paste(output_dir, rec_dirs[i], sep="/"))
      )
    }
  }
  
  return(output_dir)
}


# Encode labels -----------------------------
encode_labels <- function(folder) {
  # load label encoder
  label_encoder <- utils::read.table(file.path(folder, "label_encoder.txt"), 
                                     sep = ":", col.names = c("label", "encoder"))
  
  # standardize label format
  label_encoder <- dplyr::mutate(label_encoder,
                                 label = gsub("'", "", label),
                                 label = gsub(" ", "_", label),
                                 label = gsub("-", "_", label),
                                 label = tools::toTitleCase(label))
  
  return(label_encoder)
}

# Make possible labels ----------------------
#' @export
encode_locations <- function(location, model_type, label_encoder) {
  #Load species extent data
  extent.data <- species.extent.data
  
  #Get possible labels based on model class
  possible.labels <- get_possible_species(location, extent.data, model_type)
  
  # filter out possible labels not in labels
  possible.labels <- possible.labels[possible.labels$label %in% label_encoder$label,]
  
  # print message
  cat(paste0("\nIdentified ", nrow(possible.labels), " taxa out of ", nrow(label_encoder), " possible taxa.\n"))
  
  return(possible.labels)
}

