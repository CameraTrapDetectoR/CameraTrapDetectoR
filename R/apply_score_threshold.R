#' Apply score_threshold to model predictions
#' 
#' @description Remove detections that fall below the score_threshold and
#' calculate confidence that an image is empty. Helper function for `deploy_model`
#' 
#' @details This function removes detections that fall below the score_threshold.  When an image
#' has all detections removed it is assigned a value of 'empty'. The confidence in the
#' prediction that the image is empty is calculated as 1 - max(x) where x is a vector of
#' confidence values for the detections that fall below score threshold. So the confidence
#' that the image is empty is assumed to be directly related to the largest confidence in
#' the detections that will be removed.
#' 
#' @import dplyr
#'
#' @param predictions_list list of predictions from model
#' @param score_threshold Threshold score for keeping bounding boxes
#' 
#' @returns df with score threshold applied
#' 
#' @export
#'
apply_score_threshold <- function(predictions_list, score_threshold){
  
  # convert list into dataframe
  df <- do.call(dplyr::bind_rows, predictions_list)

  # get list of file names
  # file_list <- unique(df$filename)
  
  # get prediction counts for each image, join to df
  pred_counts <- dplyr::count(df, filename)
  df <- dplyr::left_join(df, pred_counts, by = "filename")
  
  # separate images with single predictions and images with multiple predictions
  single_preds <- dplyr::filter(df, n == 1)
  multiple_preds <- dplyr::filter(df, n > 1)
  
  # filter out images with multiple preds all below score threshold
  multiple_preds <- dplyr::group_by(multiple_preds, filename)
  low_scores <- dplyr::filter(multiple_preds, all(confidence_score < score_threshold))
  if(nrow(low_scores > 0)){
    low_scores <- dplyr::filter(low_scores, confidence_score == max(confidence_score))
  }
  
  
  # keep remaining preds above score threshold
  multiple_preds <- dplyr::ungroup(multiple_preds)
  multiple_preds <- dplyr::filter(multiple_preds, confidence_score > score_threshold)
  
  # combine formatted dfs
  df_out <- dplyr::bind_rows(single_preds, low_scores, multiple_preds)
  
  # rename predictions for rows under score threshold
  df_out <- dplyr::mutate(df_out, prediction = ifelse(confidence_score < score_threshold, "Empty", prediction))
  
  # remove n 
  df_out <- dplyr::select(df_out, !n)
  
  return(df_out)
}
