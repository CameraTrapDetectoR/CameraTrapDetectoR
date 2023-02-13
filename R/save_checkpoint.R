#' Save model output every ten images to retain results if something happens to R session mid-run
#' 
#' 
#' @param predictions_list prediction output
#' @param score_threshold score threshold to subset predictions
#' @param prediction_format wide or long
#' @param label_encoder class label dict
#' @param output_dir directory to save output
#'
#' @import exifr
#' 
#' @export
save_checkpoint <- function(predictions_list, score_threshold, 
                            prediction_format, label_encoder, output_dir) {
  
  # make predictions df
  df_out <- write_output(predictions_list, score_threshold, prediction_format, label_encoder)
  
  # save predictions to csv
  utils::write.csv(df_out, file.path(output_dir, 'model_predictions.csv'), row.names=FALSE)
  
  # if saving all bboxes, make df and save to csv
  # Write Bounding Box File
  if(write_bbox_csv){
    bbox_df <- write_bbox_df(predictions_list)
    utils::write.csv(bbox_df, file.path(output_dir, "predicted_bboxes.csv"), row.names=FALSE)
  }
  
  # END
}