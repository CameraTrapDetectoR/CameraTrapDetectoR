#' Evaluate results for images subject to manual review
#' 
#' @description join model_predictions output to a user-generated df of ground 
#' truth identifications and assess overall and/or classwise performance
#' 
#' @details this function takes your output results from deploy_model and joins them
#' to a dataframe with ground truth identifications. The object returned is a list. Item 
#' 1 is the joined df with predictions and ground truths; item 2 
#' 
#' 
#' @param preds df of output from deploy_model. Currently only supports 'long' prediction format
#' @param data ground truth results. Must be an R dataframe and contain the following columns:
#' @param filepath name of the column in `data` that contains absolute paths to the the images being assessed
#' @param true_class name of the column in `data` that contains ground truth classifications. 
#' Must match the class level of `preds` i.e. if `preds` contained predictions for the species model, this
#' column must contain ground truth species. 
#' @param assess_counts boolean. Evaluate how well the model predicts counts in addition to classes. Default = FALSE.
#' @param true_count name of the column in `data` that contains ground truth counts for the class listed in 
#' `true_class` for a given row.  
#' 
#' @export
eval_model <- function(preds = NULL, data = NULL,
                       filepath = NULL, true_class = NULL, 
                       assess_counts = FALSE, true_count = NULL,
                       assess_seq = FALSE, seq_id = NULL){
  
  # warnings if receiving the wrong type of input:
  if(!is.data.frame(preds)){
    stop(paste0(preds, " must be in data.frame format. 
                \nUse as.data.frame(", preds, ") to format your predictions.\n"))
  }
  
  if(!is.data.frame(data)){
    stop(paste0(data, " must be in data.frame format. 
                \nUse as.data.frame(", data, ") to format your annotations.\n"))
  }
  
  # warning if incorrect format for counts 
  if(assess_counts & is.null(true_counts)){
    stop(paste0("Please provide the column name for animal counts in ", data, 
                " in the argument `true_count`\n"))
  }
  
  # warning if incorrect format for counts 
  if(assess_seq & is.null(seq_id)){
    stop(paste0("Please provide the column name for sequence id in ", data, 
                " in the argument `seq_id`\nTo define sequence info for your dataset,
                see the function *assign_seq_metadata*\n"))
  }
  
  # write function to :
  # 1. join results to ground truths 
  
  # 2. calculate eval metrics  
  # 3. output object containing joined df and evals
  
}
  