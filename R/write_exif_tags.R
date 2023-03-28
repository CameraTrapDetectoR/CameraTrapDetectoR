#' Write results to image metadata
#' 
#' @param class predicted class
#' @param conf prediction confidence score
#' @param count prediction count
#' @param review_threshold review threshold for accepting predictions
#' 
#' @details write exiftool config file 
#' @return call for exifr::exiftool_call
#' 
#' @export
write_exif_tags <- function(class, conf, count, review_threshold){
  # generate config file 
  txt <-"%Image::ExifTool::UserDefined = (
    'Image::ExifTool::Exif::Main' => {
      0xd001 => {
        Name => 'CTDPredictedClass',
        Writable => 'string',
      },
      0xd002 => {
        Name => 'CTDPredictionCount',
        Writable => 'rational64s',
      },
      0xd003 => {
        Name => 'CTDPredictionConfidence',
        Writable => 'rational64s',
      },
      0xd004 => {
        Name => 'CTDReviewStatus',
        Writable => 'string',
      },
      
    }
  )"
  
  # save file to CTD R lib
  config_file <- paste0(.libPaths()[1], "/CameraTrapDetectoR/extdata/CTD_Exiftool.config")
  fileConn<-file(config_file)
  writeLines(txt, fileConn)
  close(fileConn)
  
  # determine review status
  review_status <- ifelse(conf >= review_threshold, 
                          "Accepted", "Pending")
  
  # create command call to exiftool
  exif_call <- c(paste0(" -config ", config_file), 
                 paste0(" -CTDPredictedClass=", class), 
                 paste0(" -CTDPredictionCount=", count),
                 paste0(" -CTDPredictionConfidence=", conf), 
                 paste0(" -CTDReviewStatus=", review_status))
  
  return(exif_call)
  
}
