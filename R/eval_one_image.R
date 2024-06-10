#' eval_one_image
#' 
#' @description Evaluate a single image in the model. Helper function for `deploy_model`
#'
#' @param input
#' @param filename
#' @param label_encoder 
#' @param overlap_correction
#' @param overlap_threshold
#' @param location
#'
#' @return pred_df data frame of formatted predictions for the image
#' @export
#'
#' @examples
eval_one_image <- function(input, filename, label_encoder, 
                           overlap_correction, overlap_threshold,
                           location) {
  
  # deploy the model. suppressing warnings here, because it is not important
  defaultW <- getOption("warn")
  output <- suppressMessages({model(input)})
  options(warn = defaultW)
  
  # format output
  pred_df <- decode_output(output, label_encoder)
  
  # add column for number of predictions
  pred_df$number_predictions <- 1
  
  # address overlapping predictions
  if(nrow(pred_df) > 1) {
    pred_df$number_predictions <- 0
    
    # address overlapping bboxes
    if(overlap_correction){
      pred_df <- reduce_overlapping_bboxes(pred_df, overlap_threshold)
    }
  }
  
  # evaluate predictions using possible species
  if(is.null(location)==FALSE){
    pred_df<-smart_relabel(pred_df, possible.labels, label_encoder)
    pred_df<-pred_df[pred_df$prediction %in% possible.labels$label,]
  }

  # when there is no predicted bounding box, create a relevant pred_df
  if(nrow(pred_df) < 1) {
    pred_df <- data.frame(XMin = 0, YMin = 0, XMax = 0, YMax = 0,
                          confidence_score = 1, prediction = "Empty", number_predictions = 0)
  }
  
  # add full filepath to prediction
  pred_df$filename <- rep(filename, nrow(pred_df))
  
  return(pred_df)
}