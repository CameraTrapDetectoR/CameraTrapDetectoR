#' Camelot bulk import
#' 
#' @description Format CameraTrapDetectoR results for bulk import into Camelot
#' image tagging software: https://camelotproject.org/
#' 
#' @param df output of CameraTrapDetectoR `deploy_model` function. The file_path column must match the local
#' paths to your images.
#' @param project_df name of dataframe with required camelot fields
#' @param Camera_Name required column in project_df corresponding to unique camera name. This marker must correspond to a character substring
#' of the file_path in your CameraTrapDetectoR model output.
#' @param Latitude required column in project_df with latitude of each camera
#' @param Longitude required column in project_df with longitude of each camera
#' @param Trap_Station optional column in project_df corresponding to trap station name associated with each camera. 
#' At max 2 cameras per trap station, will usually just have one
#' @param Start_Date optional column in project_df with the start date for each camera
#' @param End_Date optional column in project_df with the end date for each camera
#' @param score_threshold recode any predictions below score threshold as empty.
#' Takes values between 0 and 1. Default is not to adjust any predictions
#' 
#' @returns data frame formatted for import to Camelot
#' 
#' @import dplyr
#' @import lubridate
#' @import stringr
#' 
#' @export
camelot_bulk_import <- function(df, project_df, Camera_N){
  
  # standardize filename column if needed
  if("file_path" %in% colnames(df)) {
    df <- dplyr::rename(df, filename = file_path)
  }
  
  # confirm df is output from CTD - R or cl versions
      # it has a prediction column, file_path column, count col, confidence 
  ctd_cols <- c("filename", "prediction", "confidence") 
  if(!all(ctd_cols %in% colnames(df))) {
    stop(paste0("Cannot find necessary columns in your dataframe pointing to CameraTrapDetectoR results.  
             Please check that your dataframe has columns for the full filepath to your images, 
             model predictions, and confidence scores.\n"))
  }
  
  # confirm existence of date, timestamp columns
  if(!(c({Start_Date}, {End_Date}) %in% colnames(project_df))) {
    
  }
  
  # check df for metadata; if it does not have any, extract it
  if(!("timestamp" %in% colnames(df))){
    print("We cannot find image metadata in your CameraTrapDetectoR results.\n
          Please be patient while we extract it now, if we can.\n
          The filepath in your data must match the filepaths to your images.")
    
    # get list of unique full filenames
    img_files <- unique(df$file_path)
    
    # extract metadata
    tryCatch( { meta_df <- extract_metadata(img_files) }, error = function(e) {
      stop(paste0("The file paths in your prediction data do not match image files on your machine.
                  Please edit this column and try again."))
      })
    
    # join metadata to df
    df <- dplyr::left_join(df, meta_df,
                           by = dplyr::join_by("filename" == "FilePath"))
  }
  
  # account for no lat, long user args entered
  if(!(c({Latitude}, {Longitude}) %in% colnames(project_df))) {
    
    # check common names to see if they were included anyways
    # use tryCatch + gsub ???
    
    # if no columns found  
    cat("No variables for lat/long detected in your project df.\n Creating dummy variables; note that these coordinates are not usable in analysis.")
    
    # create dummy vars w
    project_df <- dplyr::mutate(project_df, Latitude = 40.7826, Longitude = -73.9656)
    
    }
  
  # format min/max of date range to the start, end dates of session
  
  
  
  
  
}