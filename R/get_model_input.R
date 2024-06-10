#' get_model_input
#' 
#' @description load image and format to pass to model
#'
#' @param filename full path to image filename
#' @param w image width - must correspond to model training, DO NOT CHANGE
#' @param h image height - must correspond to model training, DO NOT CHANGE
#'
#' @return list of two tensors image and target
#' @export
#'
#' @examples
get_model_input <- function(filename, w=408, h=307) {
  
  # load image file
  img <- magick::image_read(filename)
  
  # resize image
  img <- magick::image_scale(img, paste0(w, 'x', h, '!'))
  
  # convert to tensor
  img_tensor <- tryCatch(img_tensor <- torchvision::transform_to_tensor(img), 
                         error = function(e) 'error')
  
  # create a dummy target - Faster R-CNN only
  target <- torch::torch_rand(3, h, w)
  
  # combine tensor and target into the object that will get passed to network 
  nn_input <- list(img_tensor, target)
  
  if("error" %in% input){
    # set up output so that I can put into the data frame
    
    nn_input <- data.frame(XMin = NA, YMin = NA, XMax=NA, YMax=NA,
                           confidence_score = NA, prediction = 'image_error', 
                           number_predictions = 0, filename = filename)
  }
  
  return(nn_input)
}