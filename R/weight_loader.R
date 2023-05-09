#' Initiate model
#' 
#' @description Initiate model architecture and load model weights. Helper
#' function for `deploy_model`
#' 
#' @param model_type The type of model: general, family, species, or pig-only.  
#' @param num_classes The number of classes in the model
#' @param redownload Should model weights be redownloaded?
#' 
#' @returns loaded model
#' 
#' @export
#' 
weight_loader <- function(
    model_type = 'general',
    num_classes = 5,
    redownload = TRUE
){
  
  # determine if windows or mac so I can load the dll file 
  windows <- ifelse(Sys.info()["sysname"] == "Windows", TRUE, FALSE)
  
  # based on model type, get path 2 weights and number of classes
  if(model_type == 'pig_only'){
    # load weights
    # AB : use relabeled family weights until we can retrain pig-only model
    path2weights <- download_cache(name="weights_family_cpu.pth", redownload=redownload)
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    # AB : use family classifications until pig model can be retrained
    arch_path <- download_cache(name="fasterrcnnArch_pig.pt", redownload=redownload)
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "general"){
    # load weights
    path2weights <- download_cache(name="weights_general_cpu.pth", redownload=redownload)
    state_dict <- torch::load_state_dict(path2weights)

    # load model architecture
    arch_path <- download_cache(name="fasterrcnnArch_general.pt", redownload=redownload)
    model <- torch::jit_load(arch_path)
    
    model$load_state_dict(state_dict)
  }
  if(model_type == "species"){
    # load weights
    path2weights <- download_cache(name="weights_species_cpu.pth", redownload=redownload)
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    arch_path <- download_cache(name="fasterrcnnArch_species.pt", redownload=redownload)
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "family"){
    # load weights
    path2weights <- download_cache(name="weights_family_cpu.pth", redownload=redownload)
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    arch_path <- download_cache(name="fasterrcnnArch_family.pt", redownload=redownload)
    model <- torch::jit_load(arch_path)
    
    # initiate the model
    model$load_state_dict(state_dict)
  }
  
  return(model)
}
