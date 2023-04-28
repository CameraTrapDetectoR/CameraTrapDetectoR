#' CameraTrapDetectoR example set data
#' 
#' Data frames related to the example_set image folder used in function examples
#' 
#' 
#' preds
#' 
#' example_set image predictions from the "species_v2" model
#' 
#' @format ## `preds`
#' A data frame with 33 rows and 17 columns:
#' \describe{
#' \item{\code{filename}}{path to image file}
#' \item{\code{prediction}}{model class prediction, given in species common name}
#' \item{\code{confidence_in_pred}}{model confidence score in the prediction}
#' \item{\code{count}}{individual prediction count per image}
#' \item{\code{certainty}}{categorical measure of certainty in prediction}
#' ...
#' }
"preds"

#'
#'
#'labels
#'
#'example_set ground truth classification by species
#'
#'@format ## `labels`
#'A data frame with 31 rows and 3 columns