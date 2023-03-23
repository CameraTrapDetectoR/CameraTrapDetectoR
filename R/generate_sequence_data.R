#' Extract sequence and metadata info and assign info to image filename
#' 
#' @description create df of metadata and sequence information for all images in user image directory
#' 
#' @details This function extracts and stores image-level metadata for your dataset. Depending on your
#' camera metadata, this will include image dimensions, timestamp, temperature, comments, camera and sequence ids.
#' If you provide a string vector of camera ids represented in your directory, sequence length and 
#' wait time between triggers, this function will also create camera and sequence identifiers based on directory
#' and timestamp information. This function utility depends on either the sufficient metadata fields in each image
#' or reasonable directory organization in order to generate accurate sequence information. Sequences with 
#' different burst lengths must also be run through the function separately if sequence information does
#' not already exist on image metadata.    
#' 
#' @param data_dir absolute path to image directory
#' @param file_extensions The types of extensions on your image files. Case insensitive; enter as a string.
#' Accepts the following file types: ".jpg", ".png", ".tif", ".pdf". Default is ".jpg"
#' @param recursive boolean. Do you have images in subfolders within your
#' data_dir that you want to analyze, if so, set to TRUE. If you only want to 
#' analyze images within your data_dir and not within sub-folders, set to FALSE. Default is TRUE.
#' @param burst_length number of images per sequence burst.
#' @param wait_time time between bursts in seconds. If your cameras have a 10-minute wait time 
#' between triggers, your wait time is 600. 
#' @param cam_prefix character vector, where each value corresponds to a unique name 
#' for each camera in your dataset as it appears in the absolute path. The function will use
#' this identifier if your camera metadata do not contain serial numbers. 
#' 
#'  
#' @import exifr
#' @import lubridate
#' @import stringr
#' @import dplyr
#'  
#' @export
extract_metadata <- function(data_dir = NULL, file_extensions = ".jpg",
                             recursive = TRUE, burst_length = 1, wait_time = 0,
                             cam_prefix = NULL) {
  
  # -- load operators so we can use the dataset function
  load_operators()
  
  # -- Get list of files
  file_list <- dataset(data_dir, recursive, file_extensions)
  
  # -- Load metadata into df
  
  # initialize df to hold metadata
  meta_df <- data.frame(matrix(nrow = length(file_list), ncol = 13))
  colnames(meta_df) <- c("FilePath", "ImageName", "ImageWidth", "ImageHeight", 
                         "TimeStamp", "MakeModel", "SerialNumber", "EventNumber",
                         "SeqNumber", "TempF", "TempC",  "Tigger", "Notes")
  print(paste0("Collecting image metadata from ", data_dir))
  
  # loop through data_dir
  for(i in 1:length(file_list)){
    # load metadata
    dat <- exifr::read_exif(file_list[i])
    
    # add filepath
    meta_df$FilePath[i] <- dat$SourceFile
    
    # add image name
    meta_df$ImageName[i] <- dat$FileName
    
    # add image height
    meta_df$ImageHeight[i] <- dat$ImageHeight
    
    # add image width
    meta_df$ImageWidth[i] <- dat$ImageWidth
    
    # add timestamp
    if("DateTimeOriginal" %in% colnames(dat)){
      meta_df$TimeStamp[i] <- dat$DateTimeOriginal
    } else if("CreateDate" %in% colnames(dat)){
      meta_df$TimeStamp[i] <- dat$CreateDate
    } else if("ProfileDateTime" %in% colnames(dat)){
      meta_df$TimeStamp[i] <- dat$ProfileDateTime
    } else if("ModifyDate" %in% colnames(dat)){
      meta_df$TimeStamp[i] <- dat$ModifyDate
    } else if("FileModifyDate" %in% colnames(dat)){
      meta_df$TimeStamp[i] <- dat$FileModifyDate
    } else {
      meta_df$TimeStamp[i] <- NA
    }
    
    # add serial number
    if("SerialNumber" %in% colnames(dat)){
      meta_df$SerialNumber[i] <- dat$SerialNumber
    } else {
      meta_df$SerialNumber[i] <- NA 
    }
    
    # add camera make/model
    if(("Make" %in% colnames(dat)) & ("Model" %in% colnames(dat))) {
      meta_df$MakeModel[i] <- paste(dat$Make, dat$Model, sep = "_")
    } else {
      meta_df$MakeModel[i] <- NA
    }
    
    # Pull available sequence info
    if("EventNumber" %in% colnames(dat)) {
      meta_df$EventNumber[i] <- dat$EventNumber
    } else {
      meta_df$EventNumber[i] <- NA
    }
    
    # create sequence id if SerialNumber and EventNumber are not NA
    if(!is.na(meta_df$SerialNumber[i]) & !is.na(meta_df$EventNumber[i])) {
      meta_df$SeqNumber[i] <- paste0(meta_df$SerialNumber[i], "_SEQ",
                                    meta_df$EventNumber[i])
    } else {
      meta_df$SeqNumber[i] <- NA
    }
    
    # add temp data
    if("AmbientTemperatureFahrenheit" %in% colnames(dat)) {
      meta_df$TempF[i] <- dat$AmbientTemperatureFahrenheit
    } else {
      meta_df$TempF[i] <- NA
    }
    
    if("AmbientTemperature" %in% colnames(dat)) {
      meta_df$TempC[i] <- dat$AmbientTemperature
    } else {
      meta_df$TempC[i] <- NA
    }
    
    # add trigger data
    if("TriggerMode" %in% colnames(dat)) {
      meta_df$Trigger[i] <- dat$TriggerMode
    } else {
      meta_df$Trigger[i] <- NA
    }
    
    # add comment data
    if("Comment" %in% colnames(dat)) {
      meta_df$Notes[i] <- dat$Comment
    } else if("UserLabel" %in% colnames(dat)) {
      meta_df$Notes[i] <- dat$UserLabel
    } else if("UserComment" %in% colnames(dat)) {
      meta_df$Notes[i] <- dat$UserComment
    }
    
    # print update
    if(i %% 10 == 0) {
      cat(paste0("\nMetadata loaded from ", i, " images."))
    }
  }
  
  cat(paste0("\nMetadata loaded from all images.\n"))
  
  # -- Create directory-based camera IDs
  if(!is.null(cam_prefix)) {
    cat(paste0("Loading camera ids from directory.\n"))
    
    # create new column for dir-based camera id
    meta_df$CameraId <- NA
    
    # loop through values of camera id
    for(i in 1:length(cam_prefix)) {
      # comb filepaths for matches to each camera id
      id <- data.frame(id_match = stringr::str_match(meta_df$FilePath, cam_prefix[i]))
      
      # add matches to df
      meta_df$CameraId <- ifelse(is.na(meta_df$CameraId), id$id_match, meta_df$CameraId)
    }
    
    # -- Create timestamp-based sequence ids
    if(burst_length > 1) {
      
      cat(paste0("Generating sequence ids.\n"))
      
      # convert timestamp from character to date
      meta_df$TimeStamp <- lubridate::ymd_hms(meta_df$TimeStamp)
      
      # create empty column to add sequence id
      meta_df$SequenceId <- NA
      
      # sort obs by camera, timestamp
      meta_df$TimeDiff <- NA
      meta_df <- meta_df[with(meta_df, order(CameraId, TimeStamp)), ]
      
      # calculate time between obs
      for(i in 2:nrow(meta_df)){
        meta_df$TimeDiff[i] <- as.numeric(lubridate::dseconds(
          lubridate::as.duration(meta_df$TimeStamp[i] - meta_df$TimeStamp[i-1])))
      }
      
      # randomly assign starting sequence id
      seq_no <- floor(stats::runif(1, 100, 1001))
      
      # create vector to hold ids
      meta_df$SequenceId <- NA
      
      # manually set first id
      meta_df$SequenceId[1] <- paste0(meta_df$CameraId[1], "_SEQ", seq_no)
      
      # loop through rest of df and update sequence ids
      for(i in 2:nrow(meta_df)){
        # update sequence id based on wait time, burst length, cam id
        if(meta_df$TimeDiff[i] > wait_time | 
           (meta_df$CameraId[i] != meta_df$CameraId[i-1]) | 
           (i %% burst_length == 1)) {
          seq_no <- seq_no + 1
        } 
        #set sequence id
        meta_df$SequenceId[i] <- paste0(meta_df$CameraId[i], "_SEQ", seq_no)
      }
      
    }
    
  }
  
  # -- Remove extraneous columns
  
  # cols with all NA values
  not_all_na <- function(x) {any(!is.na(x))}
  meta_df <- dplyr::select(meta_df, where(not_all_na))
  # time diff column
  meta_df <- dplyr::select(meta_df, !TimeDiff)
  
  return(meta_df)
}
#END