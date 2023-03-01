#' Evaluate results for images subject to manual review
#' 
#' @description join model_predictions output to a user-generated df of ground 
#' truth identifications and assess overall and/or classwise performance
#' 
#' @details this function takes your output results from deploy_model and joins them
#' to a dataframe with ground truth identifications. The object returned is a list. Item 
#' 1 is the joined df with predictions and ground truths; item 2 
#' 
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
                        review_threshold = NA, count_subdir = FALSE, 
                        remove_originals = FALSE){