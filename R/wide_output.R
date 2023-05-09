#' -UNDER DEVELOPMENT- Convert output to wide format
#' 
#' @description Convert output from `deploy_model` to wide format 
#' 
#' @param df df of predictions returned from `deploy_model` function
#' 
#' @returns df with output widened to facilitate counts
#' 
#' @export
wide_output <- function(df) {
    
  # create predictions cross-table
  # need to account for 'count' col here!
  tbl1 <- as.data.frame.matrix(table(df[,c("filename", "prediction")]))

  # convert this to dataframe
  df_wide <- data.frame('filename' = rownames((tbl1)),
                        tbl1, row.names = NULL)
  
  # pull metadata from preds if it's there
  pred_cols <- c("prediction", "confidence_in_pred", "count", "certainty")
  df_minus_preds <- subset(df, select = -pred_cols)
  
  # join output to other columns in preds
  df_out <- merge(df, df_wide, by = "filename")
  

}

