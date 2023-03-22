#' Remove NA columns
#' 
#' @details Helper function to remove columns with all NA values 
#' 
#' @param df df to filter
#' 
#' @return df with all NA columns removed
#' 
#' @import dplyr
#' 
#' @export
remove_na <- function(df){
  all_na <- function(x) {any(!is.na(x))}
  filtr_df <- dplyr::select(df, where(all_na))
  
  return(filtr_df)
}
