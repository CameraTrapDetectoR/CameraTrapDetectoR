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
    path2weights <- download_cache(url="https://www.dropbox.com/s/ap2pbut2eycibw0/weights_family_cpu.pth?dl=0")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    # AB : use family classifications until pig model can be retrained
    arch_path <- download_cache(url="https://www.dropbox.com/s/2ny0372ug7o7c1z/fasterrcnnArch_33classes.pt?dl=0")
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "general"){
    # load weights
    path2weights <- download_cache(url="https://www.dropbox.com/s/nrlj0at6qq9p92b/weights_general_cpu.pth?dl=0")
    state_dict <- torch::load_state_dict(path2weights)

    # load model architecture
    arch_path <- download_cache(url="https://www.dropbox.com/s/ch7g81go4r1gr6p/fasterrcnnArch_5classes.pt?dl=0")
    model <- torch::jit_load(arch_path)
    
    model$load_state_dict(state_dict)
  }
  if(model_type == "species"){
    # load weights
    path2weights <- download_cache(url="https://www.dropbox.com/s/hh72h5zzujsqzdz/weights_species_cpu.pth?dl=0")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    arch_path <- download_cache(url="https://www.dropbox.com/s/n0wbrx37w9bzddm/fasterrcnnArch_77classes.pt?dl=0")
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "family"){
    # load weights
    path2weights <- download_cache(url="https://www.dropbox.com/s/ap2pbut2eycibw0/weights_family_cpu.pth?dl=0")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    arch_path <- download_cache(url="https://www.dropbox.com/s/2ny0372ug7o7c1z/fasterrcnnArch_33classes.pt?dl=0")
    model <- torch::jit_load(arch_path)
    
    # initiate the model
    model$load_state_dict(state_dict)
  }
  
  return(model)
}
