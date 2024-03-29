#' Write model output
#' 
#' @description Helper function for `deploy_model`. Takes prediction df and 
#' cleans into reportable output for csv files and R data frame
#' 
#' @param full_df predictions list filtered by score_threshold
#' 
#' @import dplyr
#' 
#' @export
write_output <- function(full_df) {

  #-- Make Predictions Dataframe
  
  # add column for prediction certainty
  full_df <- dplyr::mutate(full_df, 
                           certainty = ifelse(prediction == "Empty", "no_detection", 
                                                       ifelse(prediction == "Empty" & confidence_score < 1, 
                                                              "detections_below_score_threshold", 
                                                              ifelse(number_predictions > 1, "multiple_predictions",
                                                                     "single_prediction"))))
  
  # get prediction counts by class and join to df
  full_df <- dplyr::group_by(full_df, filename)
  class_cts <- dplyr::count(full_df, prediction)
  count_df <- dplyr::left_join(full_df, class_cts,
                               by = c("filename", "prediction"))
  
  # rename column
  count_df <- dplyr::rename(count_df, count = n)
  
  # assign zero counts to empty or error images
  count_df <- dplyr::mutate(count_df,
                            count = ifelse(prediction == "Empty", 0,
                                           ifelse(prediction == "image_error", 0, count)))
  
  # filter multiple class-wise preds, keeping lowest confidence score above threshold
  count_df <- dplyr::group_by(count_df, filename, prediction)
  count_df <- dplyr::filter(count_df, confidence_score == min(confidence_score))
  
  # 
  # # aggregate predictions by count
  # min.vals<-stats::aggregate(confidence_in_pred~filename+prediction+certainty,
  #                            data=full_df[full_df$certainty!="detections_below_score_threshold",],
  #                            FUN=min)
  # 
  # cnt.val<-stats::aggregate(number_predictions~filename+prediction+certainty,
  #                           data=full_df[full_df$certainty!="detections_below_score_threshold",],
  #                           FUN=length)  
  # 
  # det_df <- cbind.data.frame(min.vals,number_predictions=cnt.val[,"number_predictions"])
  # det_df <- dplyr::left_join(det_df, full_df, 
  #                            by = c("filename", "prediciton"), 
  #                            suffix = c("", ".y"), 
  #                            multiple = "all")
  # det_df <- det_df[,colnames(full_df)]
  # 
  # full_df_cnt<-rbind.data.frame(det_df, full_df[full_df$certainty=="detections_below_score_threshold",])
  # # colnames(full_df_cnt) <- c("filename","prediction","confidence_score","count","certainty")
  
  # remove redundant columns
  count_df <- dplyr::select(count_df, !c("XMin", "YMin", "XMax", "YMax", "number_predictions"))
  count_df <- remove_na(count_df)
  
  # # rename count column
  # colnames(full_df_cnt)[colnames(full_df_cnt) == "number_predictions"] = "count"
  # 
  # # assign zero to counts if empty
  # full_df_cnt <- dplyr::mutate(full_df_cnt,
  #                              count = ifelse(prediction == "Empty", 0, count))
  # full_df_cnt[full_df_cnt$prediction=="Empty","count"]<-0
  
  # reorder rows by image name, predicted class
  df_out <-count_df[order(count_df$filename, count_df$prediction),]
  
  # reorder columns
  df_out <- dplyr::relocate(df_out, c("filename", "prediction", "confidence_score", "count", "certainty"))
  
  # return data frame
  return(df_out)
  
# END
}