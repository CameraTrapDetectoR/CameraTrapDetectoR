#' Load model weights
#' 
#' @param model_type The type of model: general, family, species, or pig-only.  
#' @param num_classes The number of classes in the model
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
    path2weights <- download_cache(url="https://www.dropbox.com/s/9u4isbz0fv4gwda/weights_family_20220308_cpu.pth?raw=1")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load model architecture
    # AB : use family classifications until pig model can be retrained
    arch_path <- download_cache(url="https://www.dropbox.com/s/obqc1ffmnq1hprq/fasterrcnnArch_33classes.pt?raw=1")
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "general"){
    path2weights <- download_cache(url="https://www.dropbox.com/s/mrlwow1935v97yd/weights_mammalBirdHumanVehicle_20220124_cpu.pth?raw=1")
    #path2weights <- "C:/Users/mtabak/projects/aphis_cftep_2021_2022/output/20211228_fasterRCNN_mammalBirdHumanVehicle_16bs_15epochs_9momentum_0005weight_decay_005lr/weights_mammalBirdHumanVehicle_cpu.pth"
    
    # load weights
    state_dict <- torch::load_state_dict(path2weights)
    # load the torchvision ops
    #dll_path <- download_cache(dll_url)
    #dyn.load(dll_path)
    
    arch_path <- download_cache(url="https://www.dropbox.com/s/40ms1ly823uw44j/fasterrcnnArch_5classes.pt?raw=1")
    #arch_path <- "C:/Users/mtabak/projects/aphis_cftep_2021_2022/fasterrcnn_5classes.pt"
    model <- torch::jit_load(arch_path)
    
    model$load_state_dict(state_dict)
  }
  if(model_type == "species"){
    # load weights
    path2weights <- download_cache(url="https://www.dropbox.com/s/f6i0520ichlk6d7/weights_species_20220126_cpu.pth?raw=1")
    state_dict <- torch::load_state_dict(path2weights)
    
    # load the torchvision ops
    #dll_path <- download_cache(dll_url)
    #dyn.load(dll_path)
    
    
    # load model architecture
    arch_path <- download_cache(url="https://www.dropbox.com/s/jdfjnbfagvn4hfq/fasterrcnnArch_77classes.pt?raw=1")
    
    
    # initiate the model
    model <- torch::jit_load(arch_path)
    model$load_state_dict(state_dict)
  }
  if(model_type == "family"){
    # load weights
    path2weights <- download_cache(url="https://www.dropbox.com/s/9u4isbz0fv4gwda/weights_family_20220308_cpu.pth?raw=1")
    state_dict <- torch::load_state_dict(path2weights)

    # load model architecture
    arch_path <- download_cache(url="https://www.dropbox.com/s/obqc1ffmnq1hprq/fasterrcnnArch_33classes.pt?raw=1")
    model <- torch::jit_load(arch_path)
    
    model$load_state_dict(state_dict)
  }
  
  return(model)
}
