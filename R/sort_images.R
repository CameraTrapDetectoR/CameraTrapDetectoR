#' Organize image files into new directory structure sortd by class
#' 
#' @description make copies of image files into a new directory with a folder for each output class
#' 
#' @details this function takes your output results from deploy_model and sorts images into a new 
#' directory based on output class, with a separate folder for each class. If multiple classes are 
#' detected in a single image, copies of that image will be placed in every folder corresponding 
#' to a predicted detection. Also returns a df of the deploy_model output with a new column for 
#' updated filepath. Optionally removes original images. Optionally puts all images with detections 
#' below a certain threshold into a separate folder for manual review.
#' 
#' @param results df of output from deploy_model. Currently only supports 'long' prediction format
#' @param output_dir absolute path to directory in which to put sorted image folders
#' @param review_threshold images with detections below this threshold will be moved into a folder
#' titled "Manual_Review". Accepts values 0.01 - 1; default = NA.
#' @param remove_originals boolean. Delete original image files. Default = FALSE
#' 
#' 
#' @export
sort_images <- function(results = NULL, output_dir = NULL,
                        review_threshold = NA, remove_originals = FALSE){
  
  # define output_dir; create it if it doesn't exist
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  # create folders within output dir based on results classes
  classes <- unique(results$prediction)
  for(i in 1:length(classes)){
    dir.create(output_dir, classes[i], sep="/")
  }
  
  # make a folder for manual review if requested
  if(!is.na(review_threshold)){
    dir.create(output_dir, "manual_review", sep="/")
  }
  
  # create placeholder vector for new filepaths
  new_filename <- rep(NA, nrow(results))
  
  # loop through results and transfer files 
  for(i in 1:nrow(results)){
    
    # define prediction info
    pred <- results[i, ]
    old_loc <- pred$filename
    class <- pred$prediction
    conf <- pred$confidence_score
    
    # create new image name 
    img_id <- tail(strsplit(old_loc, "\\\\")[[1]], n = 2)
    img_name <- paste(img_id[1], img_id[2], sep="_")
    
    # create full path for new image loc/name
    new_loc <- paste(output_dir, class, img_name, sep="/")
    
    # update destination based on user input for review_threshold
    if(!is.na(review_threshold)){
      if(conf < review_threshold){
        new_loc <- paste(output_dir, "manual_review", img_name, sep="/")
      }
    }
    
    # copy image to new directory using new image name
    if(is.na(review_threshold)){
      file.copy(from=old_loc, to=new_loc, overwrite=F, recursive=F)
    }
    
    # update new filename column
    new_filename[i] <- new_loc
    
    # perform action on original image based on user input
    if(remove_originals){
      file.remove(old_loc)
    }
  }
  
  # add new_filename to results
  results_df <- cbind(results, new_filename)
  
  print("All files transferred")
  
  return(results_df)
}
