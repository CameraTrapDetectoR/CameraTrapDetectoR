#' Load model weights
#' 
#' @param model_type The type of model: general, family, species, or pig-only.  
#' @param num_classes The number of classes in the model
#' 
#' @details Keep model weight names generic by model type so latest weights will automatically be downloaded
#' 
#' @return model weights
#' 
#' @export
#' 
weightLoader <- function(
    model_type = 'general',
    num_classes = 5
){
  
  # determine if windows or mac so I can load the dll file 
  windows <- ifelse(Sys.info()["sysname"] == "Windows", TRUE, FALSE)
  
  # based on model type, get path 2 weights and number of classes
  if(model_type == 'pig_only'){
    # load weights
    # AB : use relabeled family weights until we can retrain pig-only model
    path2weights <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/weights_family_cpu.pth")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    # AB : use family classifications until pig model can be retrained
    arch_path <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/fasterrcnnArch_33classes.pt")
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "general"){
    # load weights
    path2weights <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/weights_general_cpu.pth")
    state_dict <- torch::load_state_dict(path2weights)

    # load model architecture
    arch_path <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/fasterrcnnArch_5classes.pt")
    model <- torch::jit_load(arch_path)
    
    model$load_state_dict(state_dict)
  }
  if(model_type == "species"){
    # load weights
    path2weights <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/weights_species_cpu.pth")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    arch_path <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/fasterrcnnArch_77classes.pt")
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "family"){
    # load weights
    path2weights <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/weights_family_20220308_cpu.pth")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    arch_path <- download_cache(url="https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/raw/main/inst/weights/fasterrcnnArch_33classes.pt")
    model <- torch::jit_load(arch_path)
    
    # initiate the model
    model$load_state_dict(state_dict)
  }
  
  return(model)
}
