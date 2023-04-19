#' Calculate class-wise evaluation metrics
#' 
#' @description helper function for `eval_model` 
#' 
#' @details given a dataframe of predictions joined to manual results, 
#' calculates the precision, recall, and F1 scores for the dataset
#' 
#' @param df dataframe formatted upstream in `eval_model` of predictions joined 
#' to ground truths, either at the image level or at the sequence level
#' 
#' @export
#' 
classwise_metrics <- function(df) {
  
  # convert predictions and ground truths to factors
  df$prediction <- as.factor(df$prediction)
  df$true_class <- as.factor(df$true_class)
  
  # create contingency table of class levels
  class_tab <- table(df$prediction, df$true_class)
  
  # pull out true positives
  class_df <- as.data.frame.matrix(class_tab)
  class_name <- sort(as.character(unique(df$true_class)))
  
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
  metrics_df <- data.frame("class" = class_name,
                         "precision" = precision,
                         "recall" = recall,
                         "f1_score" = f1_score)
  
  return(metrics_df)
}