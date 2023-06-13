#' Get List of Possible Species
#' 
#' @description Evaluate user-provided latitude / longitude values with range
#' extent data for species included in the trained models. Helper function for
#' `delpoy_model`
#' 
#' @param location dataframe containing a single longitude and latitude value
#' @param extent.data auxiliary location database
#' 
#' @returns possible species list
#' 
#' @export


get_possible_species <- function(location, extent.data, model_type){
  
  #--Test which species to consider
  location.test <- vector()
  
  for(i in 1:nrow(extent.data)){
    location.test[i]<-location_contained_in_extent(location, extent.data[i,])
  }#END Loop
  
  possible.labels <- extent.data[location.test==TRUE,]
  
  if("general" %in% model_type){
    possible.labels <- possible.labels[possible.labels$model_type == "general",]
  }
  if("family" %in% model_type){
    possible.labels <- possible.labels[possible.labels$model_type == "family",]
  }
  if("species" %in% model_type){
    possible.labels <- possible.labels[possible.labels$model_type == "species",]
  }
  if("pig" %in% model_type){
    possible.labels <- possible.labels[possible.labels$label == "Wild_Pig",]
    possible.labels$model_type <- "pig_only"
    # create a generic 'not_pig' row
    possible.labels[2, ] <- data.frame(taxa = 'species',
                                       label = 'Not_Pig',
                                       model_type = 'pig_only',
                                       xmin = -180, ymin = -90,
                                       xmax = 180, ymax = 90)
  }
  
  #--Generate unique set of species 
  possible.labels <- unique(possible.labels[,c("taxa","label","model_type")])
  
  possible.labels <- possible.labels[order(possible.labels$taxa, possible.labels$label),]
  
  return(possible.labels)
}#END Function
