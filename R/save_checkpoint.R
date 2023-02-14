#' Save model output every ten images to retain results if something happens to R session mid-run
#' 
#' 
#' @param out_df formatted df to write to csv
#' @param output_dir directory to save output
#' @param write_bbox_csv bool indicator to create df of all bbox predictions

#' 
#' @export
save_checkpoint <- function(out_df, output_dir, write_bbox_csv) {

  # save predictions to csv
  utils::write.csv(out_df, file.path(output_dir, 'model_predictions.csv'), row.names=FALSE)
  
  # if saving all bboxes, make df and save to csv
  # Write Bounding Box File
  if(write_bbox_csv){
    bbox_df <- write_bbox_df(predictions_list, w, h)
    utils::write.csv(bbox_df, file.path(output_dir, "predicted_bboxes.csv"), row.names=FALSE)
  }
  
  # END
}