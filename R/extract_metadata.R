#' Extract metadata helper function
#' 
#' @description extract metadata for a single image file
#' 
#' @details Helper function for `deploy_model`, `generate_sequences` functions
#' 
#' @param file absolute path to image file
#'  
#' @import exifr
#' @import lubridate
#' @import stringr
#' @import dplyr
#'  
#' @export
extract_metadata <- function(file){
  
  # initialize df to hold metadata
  meta_df <- data.frame(matrix(nrow = 1, ncol = 13))
  colnames(meta_df) <- c("FilePath", "ImageName", "ImageWidth", "ImageHeight", 
                         "TimeStamp", "MakeModel", "SerialNumber", "EventNumber",
                         "SeqNumber", "TempF", "TempC",  "Tigger", "Notes")
  
  # load metadata
  dat <- exifr::read_exif(file)
  
  # add filepath
  meta_df$FilePath <- normalizePath(dat$SourceFile, winslash = "/")
  
  # add image name
  meta_df$ImageName <- dat$FileName
  
  # add image height
  meta_df$ImageHeight <- dat$ImageHeight
  
  # add image width
  meta_df$ImageWidth <- dat$ImageWidth
  
  # add timestamp
  if("DateTimeOriginal" %in% colnames(dat)){
    meta_df$TimeStamp <- dat$DateTimeOriginal
  } else if("CreateDate" %in% colnames(dat)){
    meta_df$TimeStamp <- dat$CreateDate
  } else if("ProfileDateTime" %in% colnames(dat)){
    meta_df$TimeStamp <- dat$ProfileDateTime
  } else if("ModifyDate" %in% colnames(dat)){
    meta_df$TimeStamp <- dat$ModifyDate
  } else if("FileModifyDate" %in% colnames(dat)){
    meta_df$TimeStamp <- dat$FileModifyDate
  } else {
    meta_df$TimeStamp <- NA
  }
  
  # add serial number
  if("SerialNumber" %in% colnames(dat)){
    meta_df$SerialNumber <- dat$SerialNumber
  } else {
    meta_df$SerialNumber <- NA 
  }
  
  # add camera make/model
  if(("Make" %in% colnames(dat)) & ("Model" %in% colnames(dat))) {
    meta_df$MakeModel <- paste(dat$Make, dat$Model, sep = "_")
  } else {
    meta_df$MakeModel <- NA
  }
  
  # Pull available sequence info
  if("EventNumber" %in% colnames(dat)) {
    meta_df$EventNumber <- dat$EventNumber
  } else {
    meta_df$EventNumber <- NA
  }
  
  # create sequence id if SerialNumber and EventNumber are not NA
  if(!is.na(meta_df$SerialNumber) & !is.na(meta_df$EventNumber)) {
    meta_df$SeqNumber <- paste0(meta_df$SerialNumber, "_SEQ",
                                   meta_df$EventNumber)
  } else {
    meta_df$SeqNumber <- NA
  }
  
  # add temp data
  if("AmbientTemperatureFahrenheit" %in% colnames(dat)) {
    meta_df$TempF <- dat$AmbientTemperatureFahrenheit
  } else {
    meta_df$TempF <- NA
  }
  
  if("AmbientTemperature" %in% colnames(dat)) {
    meta_df$TempC <- dat$AmbientTemperature
  } else {
    meta_df$TempC <- NA
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
  
  # return obs
  return(meta_df)
  
}