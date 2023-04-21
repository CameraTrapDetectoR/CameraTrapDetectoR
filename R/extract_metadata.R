#' Extract metadata helper function
#' 
#' @description extract metadata for a single image file or list of files
#' 
#' @details Helper function for `deploy_model`, `generate_sequences` functions
#' 
#' @param files character vector with absolute path to image file/files
#'  
#' @import exifr
#'  
#' @export
extract_metadata <- function(files){
  # break list down to unique files
  meta_files <- unique(files)
  
  # initialize df to hold metadata
  meta_df <- data.frame(matrix(nrow = length(meta_files), ncol = 13))
  colnames(meta_df) <- c("FilePath", "ImageName", "ImageWidth", "ImageHeight", 
                         "TimeStamp", "MakeModel", "SerialNumber", "EventNumber",
                         "SeqNumber", "TempF", "TempC",  "Tigger", "Notes")
  
  # load metadata
  dat <- exifr::read_exif(meta_files)
  
  # bulk col values
  # add filepath
  meta_df$FilePath <- normalizePath(dat$SourceFile, winslash = "/")
  
  # add image name
  meta_df$ImageName <- dat$FileName
  
  # add image height
  meta_df$ImageHeight <- dat$ImageHeight
  
  # add image width
  meta_df$ImageWidth <- dat$ImageWidth
  
  # loop through indiv files for remaining values
  for(i in 1:nrow(dat)){
    # add timestamp
    if("DateTimeOriginal" %in% colnames(dat)){
      if(!is.na(dat$DateTimeOriginal[i])){
        meta_df$TimeStamp[i] <- dat$DateTimeOriginal[i]
      }
    }
    if("CreateDate" %in% colnames(dat)){
      if(is.na(meta_df$TimeStamp[i])){
        meta_df$TimeStamp[i] <- dat$CreateDate[i]
      }
    }
    if("ProfileDateTime" %in% colnames(dat)){
      if(is.na(meta_df$TimeStamp[i])){
        meta_df$TimeStamp[i] <- dat$ProfileDateTime[i]
      }
    }
    if("ModifyDate" %in% colnames(dat)){
      if(is.na(meta_df$TimeStamp[i])){
        meta_df$TimeStamp[i] <- dat$ModifyDate[i]
      }
    }
    if("FileModifyDate" %in% colnames(dat)){
      if(is.na(meta_df$TimeStamp[i])){
        meta_df$TimeStamp[i] <- dat$FileModifyDate[i]
      }
    }
    
    # add serial number
    if("SerialNumber" %in% colnames(dat)){
      meta_df$SerialNumber[i] <- dat$SerialNumber[i]
    } 
    
    # add camera make/model
    if(("Make" %in% colnames(dat)) & ("Model" %in% colnames(dat))) {
      meta_df$MakeModel[i] <- paste(dat$Make[i], dat$Model[i], sep = "_")
      # replace NA_NA with true NA value
      meta_df$MakeModel[i] <- ifelse(meta_df$MakeModel[i] == "NA_NA", NA, meta_df$MakeModel[i])
    } 
    
    # Pull available sequence info
    if("EventNumber" %in% colnames(dat)) {
      meta_df$EventNumber[i] <- dat$EventNumber[i]
    } 
    
    # create sequence id if SerialNumber and EventNumber are not NA
    if(!is.na(meta_df$SerialNumber[i]) & !is.na(meta_df$EventNumber[i])) {
      meta_df$SeqNumber[i] <- paste0(meta_df$SerialNumber[i], "_SEQ",
                                     meta_df$EventNumber[i])
    } 
    
    # add temp data
    if("AmbientTemperatureFahrenheit" %in% colnames(dat)) {
      meta_df$TempF[i] <- dat$AmbientTemperatureFahrenheit[i]
    } 
    if("AmbientTemperature" %in% colnames(dat)) {
      meta_df$TempC[i] <- dat$AmbientTemperature[i]
    }
    
    # add trigger data
    if("TriggerMode" %in% colnames(dat)) {
      meta_df$Trigger[i] <- dat$TriggerMode[i]
    }
    
    # add comment data
    if("Comment" %in% colnames(dat)) {
      meta_df$Notes[i] <- dat$Comment[i]
    } 
    if("UserLabel" %in% colnames(dat)) {
      if(is.na(meta_df$Notes[i])) {
        meta_df$Notes[i] <- dat$UserLabel[i]
      }
    }
    if("UserComment" %in% colnames(dat)) {
      if(is.na(meta_df$Notes[i])) {
        meta_df$Notes[i] <- dat$UserComment[i]
      }
    }
  }
  
  # remove all NA columns
  meta_df <- remove_na(meta_df)
  
  
  # return obs
  return(meta_df)
  
}