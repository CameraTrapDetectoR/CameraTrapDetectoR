#' Evaluate results for images subject to manual review
#' 
#' @description join model_predictions output to a user-generated df of ground 
#' truth identifications and assess overall and/or classwise performance
#' 
#' @details this function takes your output results from deploy_model and joins them
#' to a dataframe with ground truth identifications. The final object returned is a list containing 
#' overall and class-wise evaluation metrics for class identification at the image and/or
#' sequence level, and optionally for counts at the image and/or sequence level.
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
#' @param event_level evaluate results at the individual level or at the sequence level? 
#' Accepts values `c("image", "sequence")`. If including evaluation by sequence, @param seq_id must be specified
#' @param seq_id name of the column in `data` that contains sequence ids for each image. If you need to generate this
#' information, see the *generate_sequence_data* function.
#' 
#' @import dplyr
#' 
#' @export
eval_model <- function(preds = NULL, data = NULL,
                       filepath = NULL, true_class = NULL, 
                       assess_counts = FALSE, true_count = NULL,
                       event_level = "image", seq_id = NULL){
  
  # -- Warnings
  
  # warnings if receiving the wrong type of input:
  if(!is.data.frame(preds)){
    stop(paste0(preds, " must be in data.frame format. 
                \nUse as.data.frame(", deparse(substitute(preds)), ") to format your predictions.\n"))
  }
  
  # warnings if cannot find filename column
  if(!("filename" %in% colnames(preds))){
    stop(paste0("Cannot find column of absolute paths to image files in ", deparse(substitute(preds)), ". 
                \nPlease ensure this column is named 'filename' .\n"))
  }
  
  # warnings if cannot find predictions column
  if(!("prediction" %in% colnames(preds))){
    stop(paste0("Cannot find column of predictions in ", deparse(substitute(preds)), ". 
                \nPlease ensure this column is named 'prediction' .\n"))
  }
  
  if(!is.data.frame(data)){
    stop(paste0(data, " must be in data.frame format. 
                \nUse as.data.frame(", deparse(substitute(data)), ") to format your annotations.\n"))
  }
  
  # warning if incorrect format for counts 
  if(assess_counts & is.null(true_counts)){
    stop(paste0("Please provide the column name for animal counts in ", deparse(substitute(data)), 
                " in the argument `true_count`\n"))
  }
  
  # warning if incorrect format for counts 
  if(event_level == "sequence" & is.null(seq_id)){
    stop(paste0("Please provide the column name for sequence id in ", deparse(substitute(data)), 
                " in the argument `seq_id`\nTo define sequence info for your dataset,
                see the function *generate_sequence_data*\n"))
  }
  
  # -- Join
  
  # join results to ground truths 
  results <- merge(preds, data, by.x = "filename", by.y = filepath, all.x = TRUE, all.y = FALSE)
  
  # flag any predictions that do not have associated annotations
  results[, true_class] <- ifelse(is.na(results[, true_class]), 
                                  "Annotation_not_found",
                                  results[, true_class])
  
  # print("Removing predictions without associated annotations from evaluation calculations.\n")
  
  # filter out predictions without labels
  evals <- results[results[,true_class] != "Annotation_not_found",]
  
  # update name for ground truth
  evals$true_class <- evals[, true_class]
  
  # update var types for class truths and predictions
  evals$prediction <- as.factor(evals$prediction)
  evals$true_class <- as.factor(evals$true_class)
  
  # calculate overall eval metrics
  evals$TP <- ifelse(evals$prediction == evals$true_class, 1, 0)
  evals$FP <- ifelse((evals$prediction != "empty" & evals$true_class == "empty"), 1, 0)
  evals$FP <- ifelse((evals$prediction != evals$true_class & evals$prediction != "empty"), 1, evals$FP)
  evals$FN <- ifelse((evals$prediction == "empty" & evals$true_class != "empty"), 1, 0)
  
  mAP <- sum(evals$TP) / sum(sum(evals$TP) + sum(evals$FP))
  mAR <- sum(evals$TP) / sum(sum(evals$TP) + sum(evals$FN))
  F1 <- 2 * ((mAP * mAR) / (mAP + mAR))
  
  image_metrics <- data.frame("mAP" = mAP, "mAR" = mAR, "F1_score" = F1)
  
  
  # -- Calculate image-level id eval metrics  
  
  if("image" %in% event_level) {
    # create contingency table of class levels
    class_tab <- table(evals$prediction, evals$true_class)
    
    # pull out true positives
    class_df <- as.data.frame.matrix(class_tab)
    class_name <- sort(as.character(unique(evals$true_class)))
    
    # sum class counts for preds and truths
    TP_FP <- data.frame(ct = rowSums(class_df)) # true positives + false positives
    TP_FP$class <- rownames(TP_FP)
    
    TP_FN <- data.frame(ct = colSums(class_df)) # true positives + false negatives
    TP_FN$class <- rownames(TP_FN)
    
    # create empty vectors to hold performance metrics
    TP <- rep(NA, length(class_name))
    precision <- rep(NA, length(class_name))
    recall <- rep(NA, length(class_name))
    f1_score <- rep(NA, length(class_name))
    
    # loop through ground truth categories
    for(i in 1:length(class_name)){
      
      # define class name and denominators
      classi <- class_name[i]
      
      # get TP+FP for class[i]
      if(classi %in% TP_FP$class){
        TPFP <- TP_FP$ct[TP_FP$class == classi]
      } else {TPFP <- 0}
      
      # get TP+FN for class[i]
      if(classi %in% TP_FN$class){
        TPFN <- TP_FN$ct[TP_FN$class == classi]
      } else {TPFN <- 0}
      
      # calculate true positives
      TP[i] <- class_df[classi, classi]
      if(is.na(TP[i])){TP[i] <- 0}
      
      # calculate metrics
      precision[i] <- TP[i] / TPFP
      if(is.na(precision[i])) {precision[i] <- -1}
      
      recall[i] <- TP[i] / TPFN
      if(is.na(recall[i])) {recall[i] <- -1}
      
      f1_score[i] <- 2 * ((precision[i]*recall[i])/(precision[i]+recall[i]))
      if(is.na(f1_score[i])) {f1_score[i] <- -1}
      
    }
    
    # combine classwise evals into a dataframe
    image_df <- data.frame("class" = class_name,
                           "precision" = precision,
                           "recall" = recall,
                           "f1_score" = f1_score)
    
    
  }
  
  # -- Calculate image-level id+count eval metrics
  
  if("image" %in% event_level & assess.counts) {
    
    # function to perform evals on counts
    
    
    
  }
  
  # -- Calculate sequence-level id eval metrics
  
  if("sequence" %in% event_level) {
    # make sure confidence score is included
    if(!("confidence_in_pred" %in% colnames(evals))){
      stop(paste0("Cannot find column with confidence scores in ", deparse(substitute(preds)), ". 
                \nPlease ensure this column is named 'confidence_in_pred' .\n"))
    }
    # # get list of unique sequence ids
    # sequences <- unique(evals[, seq_id])
    
    # # loop through each sequence
    # for(i in 1:length(sequences)){
    #   # isolate a given sequence
    #   imgs <- evals[evals[,seq_id] == sequences[i],]
    #   
    #   # group sequence images by prediction
    #   imgs <- dplyr::group_by(imgs, prediction)
    #   
    #   # keep only highest class-wise predictions
    #   imgs <- dplyr::filter(imgs, confidence_in_pred == max(confidence_in_pred))
    # }
    
    # rename seq_id variable for ease of use
    evals$seq_id <- evals[, seq_id]
    
    # group df by sequence id and predicted class
    seq_evals <- dplyr::group_by(evals, seq_id, prediction)
    
    # Keep highest confidence class-wise predictions for each sequence
    seq_evals <- dplyr::filter(seq_evals, confidence_in_pred == max(confidence_in_pred))
    
    # exclude redundant empty predictions
    seq_evals <- dplyr::filter(seq_evals, prediction != "empty", .preserve = TRUE)
    
    
  }
  
  # -- Calculate sequence-level id+count eval metrics
  

  
  
  # -- Wrap all outputs in a list
  
  eval_obj <- list("class_evals" = image_df, "overall_metrics" = image_metrics)
  
  # -- Return final object
  
  return(eval_obj)
}
  