#' Aggregate predictions by sequence
#' 
#' @description Apply sequence id to results from the `deploy_model` function to 
#' aggregate predictions to the sequence level. 
#' 
#' @returns Returns a dataframe of predictions filtered down to the highest 
#' confidence prediction per class per sequence. 
#' 
#' @details If all predictions for a given sequence are "empty", the first image
#' prediction in the sequence will be retained. If a sequence has non-"empty" 
#' predictions, all "empty" predictions from that sequence will be removed. 
#' The function requires model output from the `deploy_model` function and sequence
#' information about the dataset. Sequences may be provided by the user or from the 
#' `generate_sequences` or `extract_metadata` functions. 
#' 
#' @param preds dataframe of predictions provided by `deploy_model`
#' @param sequences sequence information for the dataset included in "preds". 
#' Must be either a column in preds, or a dataframe with corresponding image paths 
#' sequence ids. A separate dataframe must have two columns: a column containing the absolute path
#' to images in the dataset that matches the corresponding column in "preds", and a
#' column of sequence ids corresponding to each image. Such a dataframe can be created
#' using the function `generate_sequences` and selecting the appropriate columns.
#' 
#' @import dplyr
#' 
#' @examples 
#' 
#' # with sequences in the same data frame as predictions
#' load(preds)
#' seq_preds <- generate_sequences(preds$filename, c("example_set"), 5, 300)
#' agg_preds <- aggregate_predictions(seq_preds, SequenceId) # directory-generated sequences
#' agg_preds <- aggregate_predictions(seq_preds, SeqNumber) # metadata-generated sequences
#' 
#' # with sequences as a separate data frame
#' load(preds)
#' meta_df <- extract_metadata(preds$filename)
#' # this will take the first sequence column in meta_df
#' agg_preds <- aggregate_predictions(preds, meta_df) 
#' 
#' @export
#' 
aggregate_predictions <- function(preds = NULL,
                                  sequences = NULL){
  # -- Warnings
  
  # warning if receiving the wrong type of input:
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
    
  # warnings if cannot find confidence score column
  if(!("confidence_in_pred" %in% colnames(preds))){
    stop(paste0("Cannot find column of predictions in ", deparse(substitute(preds)), ". 
              \nPlease ensure this column is named 'confidence_in_pred' .\n"))
  }
  
  # add warnings related to the sequences input
  
  # -- Join
  
  # if sequences is a separate df
  if(is.data.frame(sequences)){
    # identify column with filename/filepath
    file_col <- colnames(sequences)[grepl("File", colnames(sequences), ignore.case = TRUE)]
    
    # identify column of sequence ids
    seq_col <- colnames(sequences)[grepl("seq", colnames(sequences), ignore.case = TRUE)][1]
    
    seq_df <- sequences[, c(file_col, seq_col)]
    
    # merge dataframes
    preds <- merge(preds, seq_df, by.x = "filename", by.y = file_col, all.x = TRUE, all.y = FALSE)

  }
  
  # -- Aggregate

  # group df by sequence id and predicted class
  pred_agg <- dplyr::group_by(preds, sequences, prediction)
  
  # Keep highest confidence class-wise predictions for each sequence
  filtrd_preds <- dplyr::filter(pred_agg, confidence_in_pred == max(confidence_in_pred))
  
  # create df with sequences that have only empty predictions; keep just first prediction of each sequence
  empt_preds <- dplyr::filter(filtrd_preds, all(prediction == "empty"))
  empt_seqs <- dplyr::slice(empt_preds, 1)
  
  # remove all empty predictions non-empty sequences
  nonempt_seqs <- dplyr::filter(filtrd_preds, prediction != "empty")
  
  # merge the two dfs
  seq_df <- rbind(empt_seqs, nonempt_seqs)
  
  # ungroup the df and sort by seq_id
  seq_df <- dplyr::ungroup(seq_df)
  seq_preds <- dplyr::arrange(seq_df, seq_id)
  
  return(seq_preds)
}