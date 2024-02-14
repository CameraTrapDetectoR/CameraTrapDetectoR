#' separate function to write bboxes to df
#' 
#' @description helper function for `deploy_model` to create bbox df for saving in checkpoint, csv
#' 
#' @param full_df prediction output from apply_score_threshold
#' @param bboxes existing predicted boxes df
#' @param score_threshold confidence score to filter bounding boxes
#'
#' @import dplyr
#'
#' @export
write_bbox_df <- function(full_df, bboxes, score_threshold) {
  
  # convert list into dataframe
  # predictions_df <- do.call(dplyr::bind_rows, predictions_list)
  
  bbox_df <- data.frame("filename" = full_df$filename,
                        "prediction" = full_df$prediction,
                        "confidence_score" = full_df$confidence_score,
                        "XMin" = round(full_df$XMin, 3),
                        "XMax" = round(full_df$XMax, 3),
                        "YMin" = round(full_df$YMin, 3),
                        "YMax" = round(full_df$YMax, 3))
 
  # filter out predicted bboxes below score_threshold
  bbox_df <- bbox_df[bbox_df$confidence_score >= score_threshold, ]
  
  # combine new bboxes with any existing results
  bbox_df <- unique(rbind(bbox_df, bboxes))
  
  #return bbox df
  return(bbox_df)
  
}#END