#' Load data describing geographic extent of species presence
#' 
#' @description Loads csv file documenting species range for all classes in the trained species model
#' @return species.extent.data.csv
#' 
#' @export
#' 
#' 
species_extent_loader <- function(){
  
  #--Download Species Extent Data
  data.path <- download_cache(url="https://raw.githubusercontent.com/CameraTrapDetectoR/CameraTrapDetectoR/main/inst/extdata/species.extent.data.csv")
  
  #--Read Species Extent Data
  extent.data<-utils::read.csv(data.path, stringsAsFactors=TRUE)
  
  #extent.data<-readr::read_csv(data.path, show_col_types=FALSE, progress=FALSE)
  #extent.data<-as.data.frame(extent.data)
  
  return(extent.data)
}#END Function