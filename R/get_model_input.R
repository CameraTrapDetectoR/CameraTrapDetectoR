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
                         error = function(e) 
                        data.frame(XMin = NA, YMin = NA, XMax=NA, YMax=NA,
                                   confidence_score = NA, prediction = 'image_error', 
                                   number_predictions = 0, filename = filename))
  
  if(is.data.frame(img_tensor)){
    # format error as dataframe compatible with model outputs
    # nn_input <- data.frame(XMin = NA, YMin = NA, XMax=NA, YMax=NA,
    #                        confidence_score = NA, prediction = 'image_error', 
    #                        number_predictions = 0, filename = filename)
    nn_input <- img_tensor
  } else {
    # create a dummy target - Faster R-CNN only
    target <- torch::torch_rand(3, h, w)
    
    # combine tensor and target into the object that will get passed to network 
    nn_input <- list(img_tensor, target)
    
  }
  
  return(nn_input)
}
