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
  
  # check df for metadata; if it does not have any, extract it
  if(!("timestamp" %in% colnames(df))){
    print("We cannot find image metadata in your CameraTrapDetectoR results.\n
          Please be patient while we extract it now, if we can.\n
          The filepath in your data must match the filepaths to your images.")
    
    # get list of unique full filenames
    img_files <- unique(df$file_path)
    
    # extract metadata
    tryCatch( { meta_df <- extract_metadata(img_files) }, error = function(e) {
      stop(print("The file paths in your prediction data do not match image files on your machine.
                 \nPlease edit this column and try again."))
      })
    
    # join metadata to df
    

    df <- dplyr::left_join(df, meta_df,
                           by = dplyr::join_by("filename" == "FilePath"))
  }
  
  # confirm project_df contains cols for camera name, trap station name,
  # lat, long 
  
  # format min/max of date range to the start, end dates of session
  
  
  
  
  
}