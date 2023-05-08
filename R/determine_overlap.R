#' Find overlap of two bounding boxes
#' 
#' @description Calculates the proportion of overlap among two bounding boxes. 
#' Run when the user specifies an overlap correction; helper function for `find_unique_sets`
#' 
#' @param a Bounding box that will be compared with b
#' @param b Second bounding box to compare with a
#' 
#' @returns overlap area of `a` and `b`
#'
#' @export
#' 

determine_overlap <- function(a,b){
  
  dx = min(a$XMax, b$XMax) - max(a$XMin, b$XMin)
  dy = min(a$YMax, b$YMax) - max(a$YMin, b$YMin)
  
  if(dx>=0 & dy>=0){
    r.overlap<-dx*dy
    
    area.a <- (a$XMax - a$XMin) * (a$YMax - a$YMin)
    area.b <- (b$XMax - b$XMin) * (b$YMax - b$YMin)
    per.overlap <- r.overlap / (area.a+area.b-r.overlap)
  } else{per.overlap<-0}
  
  return(per.overlap)
}