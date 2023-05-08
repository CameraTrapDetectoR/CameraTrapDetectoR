#' Finds all potential combinations within data frame
#' 
#' @description Generate table of all possible combinations of
#' bounding boxes to evaluate. Helper function for `find_unique_sets`
#' 
#' @param df The data frame containing bounding box values and predictions
#' @returns matrix of all possible combinations
#' 
#' @export
#' 

find_all_combinations <- function(df){
  
  expand.grid(seq(1,nrow(df)))
  
  l <- rep(list(0:1), nrow(df))
  
  mat<-expand.grid(l)
  mat$tot<-rowSums(mat)
  mat<-mat[mat$tot==2,]
  mat<-mat[,colnames(mat)!="tot"]
  
  return(mat)
}