#' Convert output to wide format
#' 
#' @details Under development! Function to convert model output to wide format. 
#' 
#' @param df df of predictions returned from `deploy_model` function
#'
#' @import
#' 
#' @export
write_output <- function(df) {
  # write function to widen output
  # do we need full list of available model labels, or just predicted model labels?
    
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
    
  
  # save df as output
  df_out <- df_wide
  
  }
}