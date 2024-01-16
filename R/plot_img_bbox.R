#' Plot image with bounding box predictions
#' 
#' @description Create a copy of the original image with predicted bounding box 
#' and, optionally, the predicted category plotted on the image copy. Helper
#' function for `deploy_model`
#' 
#' @param filename The file containing the image
#' @param plot_df Prediction dataframe that is output from deployment
#' @param output_dir Desired directory to make plots
#' @param data_dir absolute path to images
#' @param plot_label boolean. Do you want the predicted category on the plot?
#' @param h The image height (in pixels) for the annotated plot. Only used if
#'  \code{make_plots=TRUE}. 
#' @param w The image width (in pixels) for the annotated plot.
#' @param col color of the bbox (and label if `plot_label=TRUE`). See `?plot` 
#'  for an explanation of `col`, `lwd`, and `lty`
#' @param lwd line width of bbox
#' @param lty line type of bbox
#' 
#' @returns png image file in output_dir with bboxes and labels plotted
#' 
#' @import magick
#' 
#' @export
#' 
plot_img_bbox<- function(filename,
                         plot_df,
                         output_dir,
                         data_dir,
                         plot_label=TRUE,
                         col="red",
                         lty=1,
                         lwd=2,
                         w=w, h=h){
  
  # prop_bbox means that data are from megadetector, not from here, so 
  # things are a little different in the file_list. 
  filename_full <- dplyr::if_else(file.exists(filename), filename, file.path(data_dir, filename))
  img <- magick::image_read(filename_full)
  img <- magick::image_scale(img, paste0(w, 'x', h, '!'))
  
  
  # save file information
  if(!endsWith(data_dir, "/")){
    # add a slash to the end of data dir, for when I pull it from file name
    data_dir <- paste0(data_dir, "/")
  }
  # I want to replace slashes with _ for those recursive files. This will 
  # keep them all in the same place
  stripped_filename <- tools::file_path_sans_ext(gsub("/", "_", gsub(data_dir, "", filename)))
  output_nm <- file.path(output_dir, paste0(stripped_filename, ".png"))
  
  # rescale bounding box
  plot_df <- dplyr::mutate(plot_df, dplyr::across(c(XMin, XMax), ~.*w))
  plot_df <- dplyr::mutate(plot_df, dplyr::across(c(YMin, YMax), ~.*h))
  
  
  # make plot
  grDevices::png(output_nm, width=w, height=h)
  plot(img)
  if (nrow(plot_df) > 0){ # Only plot boxes if there are predictions
    for(i in 1:nrow(plot_df)){
      graphics::segments(x0=plot_df$XMin[i], y0=plot_df$YMin[i],
                         x1=plot_df$XMin[i], y1=plot_df$YMax[i], 
                         col=col, lty=lty, lwd=lwd)
      graphics::segments(x0=plot_df$XMin[i], y0=plot_df$YMin[i],
                         x1=plot_df$XMax[i], y1=plot_df$YMin[i], 
                         col=col, lty=lty, lwd=lwd)
      graphics::segments(x0=plot_df$XMin[i], y0=plot_df$YMax[i],
                         x1=plot_df$XMax[i], y1=plot_df$YMax[i], 
                         col=col, lty=lty, lwd=lwd)
      graphics::segments(x0=plot_df$XMax[i], y0=plot_df$YMax[i],
                         x1=plot_df$XMax[i], y1=plot_df$YMin[i], 
                         col=col, lty=lty, lwd=lwd)
      if(plot_label){
        graphics::text(x= plot_df$XMin[i]+6, y=plot_df$YMin[i]+10, plot_df$prediction[i],
                       col=col, adj=0)  
      }
    }
  }
  
  grDevices::dev.off()
  
}