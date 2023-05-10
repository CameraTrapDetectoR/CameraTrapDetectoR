#' Remove NA columns
#' 
#' @description Remove columns with all NA values. Helper function 
#' for `extract_metadata`
#' 
#' @param df df to filter
#' 
#' @returns df with all NA columns removed
#' 
#' @import dplyr
#' 
#' @export
remove_na <- function(df){
  all_na <- function(x) {any(!is.na(x))}
  filtr_df <- dplyr::select(df, where(all_na))
  
  return(filtr_df)
}
