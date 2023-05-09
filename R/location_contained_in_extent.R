#' Determine if location falls within a species range
#' 
#' @description Evaluate if a location (latitude / longitude) is within a geographic 
#' extent. Helper function for `get_possible_species`
#' 
#' @param location dataframe containing a single longitude and latitude value
#' @param extent.values dataframe containing xmin, xmax, ymin, ymax representing an extent (bounding box)
#'
#' @returns location extent helper function
#' 
#' @export

location_contained_in_extent<-function(location, extent.values){
  
  return((location$longitude > extent.values$xmin &
            location$longitude < extent.values$xmax &
            location$latitude > extent.values$ymin &
            location$latitude < extent.values$ymax))
}
