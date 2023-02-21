#' Save model output every ten images to retain results if something happens to R session mid-run
#' 
#' 
#' @param out_df formatted df to write to csv
#' @param output_dir directory to save output
#' @param model_type model type
#' @param write_bbox_csv bool indicator to create df of all bbox predictions

#' 
#' @export
save_checkpoint <- function(out_df, output_dir, model_type, write_bbox_csv) {

  # save predictions to csv
  utils::write.csv(out_df, file.path(output_dir, paste(model_type, 'model_predictions.csv', sep="_")), row.names=FALSE)
  
  # if saving all bboxes, make df and save to csv
  # Write Bounding Box File
  if(write_bbox_csv){
    bbox_df <- write_bbox_df(predictions_list, w, h, bboxes)
    utils::write.csv(bbox_df, file.path(output_dir, paste(model_type, "predicted_bboxes.csv", sep="_")), row.names=FALSE)
  }
  
  # END
}