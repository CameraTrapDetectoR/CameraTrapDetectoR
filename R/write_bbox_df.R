#' separate function to write bboxes to df
#' 
#' @details create bbox df for saving in checkpoint, csv
#' 
#' @param predictions_list prediction output
#' @param w image width
#' @param h image height
#'
#' 
#' @export
write_bbox_df <- function(predictions_list, w, h) {
  
  # convert list into dataframe
  predictions_df <- do.call(rbind, predictions_list)
  
  bbox_df <- data.frame("filename" = predictions_df$filename,
                        "prediction" = predictions_df$label.y,
                        "confidence" = predictions_df$scores,
                        "number_predictions" = predictions_df$number_bboxes,
                        "XMin" = as.numeric(predictions_df$XMin)/w,
                        "XMax" = as.numeric(predictions_df$XMax)/w,
                        "YMin" = as.numeric(predictions_df$YMin)/h,
                        "YMax" = as.numeric(predictions_df$YMax)/h)
  
  #return bbox df
  return(bbox_df)
  
}#END