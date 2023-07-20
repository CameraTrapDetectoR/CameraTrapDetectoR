#' Extract relevant model output  
#' 
#' @description Converts the output from the neural network into a format that 
#' can be used in reporting results. Helper function for `deploy_model`
#' 
#' @param output this is a subset of the list output from the neural net
#' @param score_threshold threshold score for keeping bounding boxes
#' @param label_encoder passed from deploy model function
#' @param h image height after resizing. Recommend not changing this
#' 
#' @returns a dataframe of model output for an image that can be interpreted in R
#' 
#' @import torchvisionlib
#' 
#' @export
decode_output <- function(
    output, # this is the list output from the neural net
    label_encoder,
    h, 
    score_threshold,
    overlap_threshold
){
  # subset the output for the part we want
  preds <- output[[2]][[1]]
  
  # perform non-maximum suppression on overlapping predictions 
  pred_ids <- torchvisionlib::ops_nms(boxes = preds$boxes, 
                                      scores = preds$scores, 
                                      iou_threshold = overlap_threshold)
  pred_ids <- as.matrix(pred_ids)
  
  # extract bboxes, scores, and labels for retained predictions
  boxes <- matrix(as.matrix(preds$boxes[pred_ids, ]), nrow=length(pred_ids), ncol=4)
  scores <- as.matrix(preds$scores[pred_ids])
  img_labels <- as.matrix(preds$labels[pred_ids])

  
  # collect outputs into a dataframe
  pred_df <- data.frame('boxes' = boxes,
                        'scores' = scores,
                        'label' = img_labels)
  
  # assign column names
  colnames(pred_df)[1:4] <- c('XMin', 'YMin', 'XMax', 'YMax')
  
  # check to ensure YMax and YMin are returned as expected - if not then reorder columns
  if(all((h > pred_df$YMax) & (pred_df$YMax > pred_df$YMin) & (pred_df$YMin > 0))){
    # rename switching ymax and ymin
    colnames(pred_df)[1:4] <- c('XMin', 'YMax', 'XMax', 'YMin')
    # reorder columns
    pred_df <- pred_df[,c('XMin', 'YMin', 'XMax', 'YMax', 'scores', 'label')]
  }
  
  # remove boxes below threshold
  #pred_df <- pred_df[pred_df$scores >= score_threshold, ]
  
  # the predicted y coordinates from the model assume that the y axis 
  # starts in the upper left hand corner of the image, but this is not how
  # plots are made in R, so I need to inverse this value
  pred_df$YMin <- h-pred_df$YMin
  pred_df$YMax <- h-pred_df$YMax
  
  # get name of label
  pred_df <- merge(pred_df, label_encoder, by.x="label", by.y="encoder")
  
  # rename prediction
  colnames(pred_df)[colnames(pred_df) == "label.y"] = "prediction"
  
  return(pred_df)
}