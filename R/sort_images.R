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
#' @param count_subdir boolean. Create subfolders inside each class directory for individual counts.
#' @param remove_originals boolean. Delete original image files. Default = FALSE
#' 
#' 
#' @export
sort_images <- function(results = NULL, output_dir = NULL,
                        review_threshold = 0, count_subdir = FALSE, 
                        remove_originals = FALSE){
  
  # define output_dir; create it if it doesn't exist
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  # create folders within output dir based on results classes
  classes <- unique(results$prediction)
  for(i in 1:length(classes)){
    # create class dir
    dir.create(paste(output_dir, classes[i], sep="/"))
    
    # make manual review folders
    if(review_threshold > 0){
      dir.create(paste(output_dir, classes[i], "manual_review", sep="/"))
    }
    
    # make count folders
    if(count_subdir){
      # filter results to a given class
      res_class <- results[results$class == class[i]]
      # get unique counts for that class
      counts <- sort(unique(res_class$count))
      for(ct in 1:length(counts)){
        dir.create(paste(output_dir, classes[i], counts[ct], sep="/"))
      }
    }
  }

  # create placeholder vector for new file paths
  new_filename <- rep(NA, nrow(results))
  
  # loop through results and transfer files 
  for(i in 1:nrow(results)){
    
    # define prediction info
    pred <- results[i, ]
    old_loc <- pred$filename
    class <- pred$prediction
    conf <- pred$confidence_score
    count <- pred$count
    
    # create new image name 
    img_id <- utils::tail(strsplit(old_loc, "\\\\")[[1]], n = 2)
    img_name <- paste(img_id[1], img_id[2], sep="_")
    
    
    # create full path for new image loc/name
    if(conf < review_threshold){
      new_loc <- paste(output_dir, class, "manual_review", img_name, sep="/")
    } else if(count_subdir){
      new_loc <- paste(output_dir, class, count, img_name, sep="/")
    }
    else{
      new_loc <- paste(output_dir, class, img_name, sep="/")
    }
    
    # copy image to new location
    file.copy(old_loc, new_loc)
    
    # update new filename column
    new_filename[i] <- new_loc
    
    # perform action on original image based on user input
    if(remove_originals){
      file.remove(old_loc)
    }
  }
  
  # add new_filename to results
  results_df <- cbind(results, new_filename)
  
  print("All images transferred")
  
  return(results_df)
}
