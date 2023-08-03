#' separate function to write bboxes to df
#' 
#' @description helper function for `deploy_model` to create bbox df for saving in checkpoint, csv
#' 
#' @param full_df prediction output from apply_score_threshold
#' @param w image width
#' @param h image height
#' @param bboxes existing predicted boxes df
#' @param score_threshold confidence score to filter bounding boxes
#'
#' @import dplyr
#'
#' @export
write_bbox_df <- function(full_df, w, h, bboxes, score_threshold) {
  
  # convert list into dataframe
  # predictions_df <- do.call(dplyr::bind_rows, predictions_list)
  
  bbox_df <- data.frame("filename" = full_df$filename,
                        "prediction" = full_df$prediction,
                        "confidence_score" = full_df$confidence_score,
                        "XMin" = as.numeric(full_df$XMin)/w,
                        "XMax" = as.numeric(full_df$XMax)/w,
                        "YMin" = as.numeric(full_df$YMin)/h,
                        "YMax" = as.numeric(full_df$YMax)/h)
 
  # filter out predicted bboxes below score_threshold
  bbox_df <- bbox_df[bbox_df$confidence_score >= score_threshold, ]
  
  # combine new bboxes with any existing results
  bbox_df <- unique(rbind(bbox_df, bboxes))
  
  #return bbox df
  return(bbox_df)
  
}#END