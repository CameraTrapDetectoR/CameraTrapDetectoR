#' Load model weights
#' 
#' @param folder path to folder containing model files
#' 
#' @details Loads model architecture and weights from the provided folder
#' 
#' @return weighted model
#' 
#' @import torch
#' 
#' @export
#' 
weightLoader <- function(folder = file.path(cache_path, fs::path_file("general_v1"))) {
  
  # determine if windows or mac so I can load the dll file 
  windows <- ifelse(Sys.info()["sysname"] == "Windows", TRUE, FALSE)
  
  # get paths to model weights and architecture
  weights_path <- paste0(folder, "/model_weights.pth")
  arch_path <- paste0(folder, "/model_arch.pt")
  
  # load model architecture
  model <- torch::jit_load(arch_path)
  
  # load model weights
  state_dict <- torch::load_state_dict(weights_path)
  model$load_state_dict(state_dict)
  
  return(model)
}
