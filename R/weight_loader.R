#' Initiate model
#' 
#' @description Initiate model architecture and load model weights. Helper
#' function for `deploy_model`
#' 
#' @param folder path to folder containing model files
#' 
#' @returns loaded model
#' 
#' @import torch
#' 
#' @export
#' 
weight_loader <- function(folder){
  
  # determine if windows or mac so I can load the dll file 
  windows <- ifelse(Sys.info()["sysname"] == "Windows", TRUE, FALSE)
  
  # define path for model weights
  weights_path <- file.path(paste0(folder, "/model_weights.pth"))
  
  # load model weights
  state_dict <- torch::load_state_dict(weights_path)
  
  # define path to model architecture
  arch_path <- file.path(paste0(folder, "/model_arch.pt"))
  
  # initiate the model
  model <- torch::jit_load(arch_path)
  model$load_state_dict(state_dict)

  
  return(model)
}
