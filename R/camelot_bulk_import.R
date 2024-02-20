#' Camelot bulk import
#' 
#' @description Format CameraTrapDetectoR results for bulk import into Camelot
#' image tagging software: https://camelotproject.org/
#' 
#' @param df output of CameraTrapDetectoR `deploy_model` function
#' @param project_df name of dataframe with required camelot fields
#' @param Camera_Name vector corresponding to unique camera name
#' @param Trap_Station vector corresponding to trap station name associated with each camera. 
#' A max 2 cameras per trap station, will usually just have
#' @param score_threshold recode any predictions below score threshold as empty
#' 
#' @returns data frame formatted for import to Camelot
#' 
#' @import dplyr
#' @import lubridate
#' @import stringr
#' 
#' @export
camelot_bulk_import <- function(df, project_df, Camera_N){
  
  # confirm df is output from CTD - R or cl versions
      # it has a prediction column, file_path column, count col, confidence 
  
  # confirm project_df contains cols for camera name, trap station name,
  # lat, long 
  
  # format min/max of date range to the start, end dates of session
  
  
  
  
  
}