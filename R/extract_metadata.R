#' Extract sequence and metadata info and assign info to image filename
#' 
#' @description create df of metadata and sequence information for all images in user image directory
#' 
#' @details 
#' 
#' @param data_dir absolute path to image directory
#' @param file_extensions The types of extensions on your image files. Case insensitive; enter as a string.
#' Accepts the following file types: ".jpg", ".png", ".tif", ".pdf". Default is ".jpg"
#' @param recursive boolean. Do you have images in subfolders within your
#' data_dir that you want to analyze, if so, set to TRUE. If you only want to 
#' analyze images within your data_dir and not within sub-folders, set to FALSE. Default is TRUE.
#' @param burst_length number of images per sequence burst.
#' @param wait_time time between bursts. 
#' @param cam_prefix chr. String pattern in absolute directory or file path denoting camera id. 
#'  
#' @import exifr
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
  meta_df <- data.frame(matrix(nrow = length(file_list), ncol = dim(dat)[2]))
  colnames(meta_df) <- c("FilePath", "ImageName", "ImageWidth", "ImageHeight", 
                         "TimeStamp", "MakeModel", "SerialNo", "EventNumber",
                         "TempF", "TempC",  "Tigger", "Notes")
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
      meta_df$SerialNo[i] <- dat$SerialNumber
    } else {
      meta_df$SerialNo[i] <- NA 
    }
    
    # add camera make/model
    if(("Make" %in% colnames(dat)) & ("Model" %in% colnames(dat))) {
      meta_df$MakeModel[i] <- paste(dat$Make, dat$Model, sep = "_")
    } else {
      meta_df$MakeModel[i] <- NA
    }
    
    # Pull available sequence info
    if("EventNumber" %in% colnames(dat)){
      meta_df$EventNumber <- dat$EventNumber
    } else {
      meta_df$EventNumber <- NA
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
      meta_df$Trigger <- dat$TriggerMode
    } else {
      meta_df$Trigger <- NA
    }
    
    # add comment data
    if("Comment" %in% colnames(dat)) {
      meta_df$Notes <- dat$Comment
    } else if("UserLabel" %in% colnames(dat)) {
      meta_df$Notes <- dat$UserLabel
    } else if("UserComment" %in% colnames(dat)) {
      meta_df$Notes <- dat$UserComment
    }
    
    # print update
    if(i %% 10 == 0) {
      cat(paste0("\nMetadata loaded from ", i, " images."))
    }
  }
  
  cat(paste0("\nMetadata loaded from all images. Generating camera and sequence ids.\n"))
  
  # -- 
  
}