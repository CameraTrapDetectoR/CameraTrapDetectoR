#' separate function to write bboxes to df
#' 
#' @details create bbox df for saving in checkpoint, csv
#' 
#' @param predictions_list prediction output
#' @param w image width
#' @param h image height
#' @param bboxes existing predicted boxes df
#' @param score_threshold score threshold to filter bboxes
#'
#' @import dplyr
#'
#' @export
write_bbox_df <- function(predictions_list, w, h, bboxes, score_threshold) {
  
  # convert list into dataframe
  predictions_df <- do.call(dplyr::bind_rows, predictions_list)
  
  bbox_df <- data.frame("filename" = predictions_df$filename,
                        "prediction" = predictions_df$prediction,
                        "confidence" = predictions_df$scores,
                        "XMin" = as.numeric(predictions_df$XMin)/w,
                        "XMax" = as.numeric(predictions_df$XMax)/w,
                        "YMin" = as.numeric(predictions_df$YMin)/h,
                        "YMax" = as.numeric(predictions_df$YMax)/h)
 
   # filter out predicted bboxes below score_threshold
  bbox_df <- bbox_df[bbox_df$confidence >= score_threshold, ]
  
  # combine new bboxes with any existing results
  bbox_df <- unique(rbind(bbox_df, bboxes))
  
  #return bbox df
  return(bbox_df)
  
}#END