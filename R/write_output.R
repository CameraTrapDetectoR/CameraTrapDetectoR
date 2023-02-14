#' Write model output
#' 
#' @details Finalize model predictions in df and csv output
#' 
#' @param full_df df filtered by score_threshold
#' @param prediction_format wide or long
#' @param label_encoder class label dict
#'
#' @import exifr
#' 
#' @export
write_output <- function(full_df, prediction_format, label_encoder) {
  
  # define all possible class labels
  all_categories <- c(label_encoder$label, 'image_error')

  #-- Make Predictions Dataframe
  
  # make wide format prediction file
  if(prediction_format=="wide"){
    
    # get just the cross table
    tbl1 <- as.data.frame.matrix(table(full_df[,c("filename", "prediction")]))
    df_wide <- data.frame('filename' = rownames((tbl1)),
                         tbl1)
    
    # rownames are filenames; replace with numbers (only if there are actually some images)
    if(nrow(df_wide) > 0){
      rownames(df_wide) <- 1:nrow(df_wide)
    }
    
    # add column names for classes not found in any images in the dataset
    cols_df <- colnames(df_wide)
    not_pred_in_any <- setdiff(all_categories, cols_df) # categories not predicted to be in any image
    
    # remove background, "empty" in use
    not_pred_in_any <- not_pred_in_any[!(not_pred_in_any %in% "background")]
    # merge unpredicted categories df back to predictions
    not_pred_df <- data.frame(matrix(0, ncol=length(not_pred_in_any), nrow=nrow(df_wide)))
    colnames(not_pred_df) <- not_pred_in_any
    # add these columns to df
    df_wide <- data.frame(df_wide, not_pred_df)
    
    # re-arrange columns
    cols_wanted0 <- label_encoder$label[!(label_encoder$label %in% "background")]
    # if there are images with errors, include a column for this
    if("image_error" %in% colnames(tbl1)){
      cols_wanted <- c("filename", cols_wanted0, 'image_error')
    }else{
      cols_wanted <- c("filename", cols_wanted0)
    }
    
    df_wide <- df_wide[cols_wanted]
    
    # Add empty predictions to df
    file_list <- unique(full_df$filename)
    file_names_to_add <- setdiff(normalizePath(file_list), df_wide$filename)
    if(length(file_names_to_add) > 0){
      df_add <- data.frame('filename' = file_names_to_add, 
                           # matrrix of 0s to match df_out
                           matrix(0, length(file_names_to_add), (ncol(df_wide) -1))
      )
      colnames(df_add) <- colnames(df_wide)
      # assign these rows as empty
      df_add$empty <- rep(1, length(file_names_to_add))
      # add this df to df_out
      df_wide <- rbind(df_wide, df_add)
      
      # sort the dataframe, so that it matches the order of images (and of plots made by this package)
      df_wide <- df_wide[order(df_wide$filename), ]

    }
    
    # save df as output
    df_out <- df_wide
  }
  
  # make long format prediction file
  if(prediction_format=="long"){
    
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
    
    det_df<-cbind.data.frame(min.vals,number_predictions=cnt.val[,"number_predictions"])
    det_df<-det_df[,colnames(full_df)]
    
    full_df_cnt<-rbind.data.frame(det_df, full_df[full_df$certainty=="detections_below_score_threshold",])
    colnames(full_df_cnt) <- c("filename","prediction","confidence_score","count","certainty")
    
    # assign zero to counts if empty
    full_df_cnt[full_df_cnt$prediction=="empty","count"]<-0
    
    df_long <-full_df_cnt[order(full_df_cnt$filename,full_df_cnt$prediction),]
    
    # save df as output
    df_out <- df_long
  }
  
  # -- Add timestamp data if available
  
  # create holder df
  df_out$timestamp <- NA
  
  # loop through image exif files and extract timestamps
  for(i in 1:nrow(df_out)){
    
    # set filepath
    path <- df_out$filename[i]
    
    # read in exif data
    exif_dat <- exifr::read_exif(path)
    
    # Search columns for original datetime
    if("DateTimeOriginal" %in% colnames(exif_dat)){
      df_out$timestamp[i] <- exif_dat$DateTimeOriginal
    }
    else if("CreateDate" %in% colnames(exif_dat)){
      df_out$timestamp[i] <- exif_dat$CreateDate
    }
    else {
      df_out$timestamp[i] <- NA
    }
  }
  
  
  # return data frame
  return(df_out)
  
# END
}