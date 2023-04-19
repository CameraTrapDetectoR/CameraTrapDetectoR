#' Write model output
#' 
#' @details Finalize model predictions in df and csv output
#' 
#' @param full_df predictions list filtered by score_threshold
#' 
#' @export
write_output <- function(full_df) {

  #-- Make Predictions Dataframe
  
  # add certainty measures
  full_df$certainty <- "single_prediction"
  
  # if number of predictions is > 1 indicate that there are multiple predictions
  full_df[full_df$number_predictions>1,"certainty"] <-"multiple_predictions"
  
  # if model did not detect object
  full_df[full_df$prediction=="empty","certainty"] <-"no_detection"
  
  full_df[full_df$prediction=="empty" & full_df$confidence_in_pred<1,"certainty"] <-"detections_below_score_threshold"
  
  min.vals<-stats::aggregate(confidence_in_pred~filename+prediction+certainty,
                             data=full_df[full_df$certainty!="detections_below_score_threshold",],
                             FUN=min)
  
  cnt.val<-stats::aggregate(number_predictions~filename+prediction+certainty,
                            data=full_df[full_df$certainty!="detections_below_score_threshold",],
                            FUN=length)  
  
  det_df <- cbind.data.frame(min.vals,number_predictions=cnt.val[,"number_predictions"])
  det_df <- dplyr::left_join(det_df, full_df, 
                             by = "filename", 
                             suffix = c("", ".y"), 
                             multiple = "all")
  det_df <- det_df[,colnames(full_df)]
  
  full_df_cnt<-rbind.data.frame(det_df, full_df[full_df$certainty=="detections_below_score_threshold",])
  # colnames(full_df_cnt) <- c("filename","prediction","confidence_score","count","certainty")
  
  # remove redundant columns
  full_df_cnt <- dplyr::select(full_df_cnt, !c("label", "XMin", "YMin", "XMax", "YMax"))
  full_df_cnt <- remove_na(full_df_cnt)
  
  # rename count column
  colnames(full_df_cnt)[colnames(full_df_cnt) == "number_predictions"] = "count"
  
  # assign zero to counts if empty
  full_df_cnt[full_df_cnt$prediction=="empty","count"]<-0
  
  # reorder rows by image name, predicted class
  df_out <-full_df_cnt[order(full_df_cnt$filename,full_df_cnt$prediction),]
  
  # reorder columns
  df_out <- dplyr::relocate(df_out, c("filename", "prediction", "confidence_in_pred", "count", "certainty"))
  
  # return data frame
  return(df_out)
  
# END
}