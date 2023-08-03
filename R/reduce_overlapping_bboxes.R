#' Take overlapping bounding boxes and return classification with maximum confidence
#' 
#' @description Evaluates overlapping bounding boxes using user specified threshold that 
#' determines overlap and returns bounding boxes and classification with maximum confidence.
#' Helper function for `deploy_model`
#' 
#' @param df The data frame containing bounding box values and predictions
#' @param overlap_threshold The threshold used in determining which bounding boxes are considered unique detections
#' 
#' @returns data frame bboxes above overlap threshold aggregated by maximum confidence
#' 
#' @export
#' 
reduce_overlapping_bboxes <- function(df, overlap_threshold){
  
  #--Find unique polygon sets
  unique.sets <- find_unique_sets(df, overlap_threshold=overlap_threshold)
  
  #--Initialize Storage
  out<-df[0,]
  
  #--Find maximum confidence for each polygon set
  for(j in 1:length(unique.sets)){
    tmp<-df[unique.sets[[j]],]
    out[j,]<-tmp[which.max(df[unique.sets[[j]],"scores"]),]
    out[j,"number_bboxes"] <- length(unique.sets[[j]])
  }
  
  #--Remove duplicates
  out<-unique(out)
  
  #--When sets overlap and the same box is found in each set aggregate
  #(this is a short term solution but results in inflated number of bboxes)
  out<-stats::aggregate(number_bboxes~XMin+YMin+XMax+YMax+scores+prediction, data=out, FUN=sum)
  
  return(out)
}