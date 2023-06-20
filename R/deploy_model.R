#' Deploy model on camera trap images
#' 
#' @description This function deploys a model trained to identify and count the objects
#' in camera trap images. 
#' 
#' This function deploys a customized computer vision model to detect and 
#' classify objects in camera trap images. The function will find all indicated image
#' files in the user-specified directory and deploy the user-specified model on these
#' images. 
#' 
#' @details 
#' CameraTrapDetectoR contains four types of models: the 'general' classifies mammals, 
#' birds, humans, and vehicles; the 'family' model classifies taxonomic families; 
#' the 'species' model classifies taxonomic species; the 'pig_only' model classifies
#' wild pigs. The most recent version of a given model will be deployed unless a
#' previous version is expressly specified, i.e. \code{model_type='species_v1'}.
#' Each model type and version) is trained independently to give the user the 
#' option to run multiple model types and/or versions on the same dataset sequentially, 
#' and treat each set of predictions as an independent observer.
#' 
#' Model version details, including class-wise results on an out-of-sample test
#' data set, are available on the CameraTrapDetectoR github repository.
#' 
#' @returns a data frame of model predictions, with predicted number of individuals
#' within each detected class for each image in the data directory. This data frame 
#' is automatically saved as a .csv file in the output directory, along with a .txt
#' file of all arguments used in that instance of the `deploy_model` function. 
#' 
#' If the user specifies, a .csv of bounding box coordinates is also automatically 
#' saved in the output directory.If the user requests image plots with bounding boxes,
#' these plotted image copies will be saved as .png files in the output directory. 
#' 
#' @param data_dir Absolute path to the folder containing your images
#' @param recursive boolean. Do you have images in subfolders within your
#'  data_dir that you want to analyze, if so, set to TRUE. If you only want to 
#'  analyze images within your data_dir and not within sub-folders, set to FALSE.
#' @param model_type Options are 'general', 'species', 'family', or 'pig_only' 
#'  with appropriate version appended with an underscore. For example, to use the version 1 species model,
#'  list `model_type` as "species_v1". Specifying a model type without a version appended will default to
#'  the latest version of that model available for your package vesion.
#'  A full list of available models and details is available 
#'  on the CameraTrapDetectoR Github wiki.
#' @param file_extensions The types of extensions on your image files. Case insensitive; enter as a string.
#' Accepts the following file types: ".jpg", ".png", ".tif", ".pdf". Default is ".jpg"
#' @param make_plots boolean. Do you want to make plots of the images with
#'  their predicted bounding boxes?
#' @param plot_label boolean. Do you want the plots to contain the predicted
#'  class of object
#' @param output_dir You can specify absolute path to output. Default is `NULL`,
#'  and creates a folder within your data_dir. Only specify a path if you want the
#'  results stored somewhere else on your computer. 
#' @param sample50 boolean. Do you want to run the model only on a subset of 
#'  50 images from your dataset? This is a good idea if you are experimenting 
#'  with settings. 
#' @param write_bbox_csv boolean. Do you want to create a csv with all of the 
#'  information on predicted bounding boxes? This csv will include all bounding boxes,
#'  even those with low probability. 
#' @param score_threshold Confidence threshold for using a bounding box, accepts 
#'  values from 0-1. A lower number will produce more bboxes (it will be less
#'  stringent in deciding to make a bbox). A higher number will produce fewer
#'  bboxes (it will be more stringent).
#' @param overlap_correction boolean. Should overlapping detections be
#' evaluated for overlap and highest confidence detection be returned
#' @param overlap_threshold Overlap threshold used when determining if bounding box
#' detections are to be considered a single detection. Accepts values from 0-1
#' representing the proportion of bounding box overlap.
#' @param get_metadata boolean. Collect metadata for each image.
#' @param latitude image location latitude. Use only if all images in the model run come from the same location.
#' @param longitude image location longitude. Use only if all images in the model run come from the same location.
#' @param h The image height (in pixels) for the annotated plot. Only used if
#'  \code{make_plots=TRUE}. 
#' @param w The image width (in pixels) for the annotated plot.
#' @param lty line type for bbox plot. See \code{?plot} for details
#' @param lwd line width for bbox plot. See \code{?plot} for details
#' @param col line color for bbox plot. See \code{?plot} for details
#' 
#' @examples
#' # path to sample images shipped with the package
#' data_dir <- get_samples() 
#' 
#' # specify only the data_dir, except all other default args
#' df_gen <- deploy_model(data_dir = data_dir)
#' 
#' # run species model at varying confidence score thresholds
#' scores <- c(0.3, 0.5, 0.7)
#' for(i in 1:length(scores)){
#' deploy_model(data_dir = data_dir, model_type = 'species', score_threshold = scores[i])
#' }
#' 
#' # loop through each model
#' data(models)
#' for(i in 1:nrow(models)){
#' deploy_model(data_dir = data_dir, model_type = models$model_name[i])
#' }
#'  
#' @import torch
#' @import torchvision
#' @import torchvisionlib
#' @import magick
#' @import dplyr
#' 
#' @export
deploy_model <- function(
    data_dir = NULL,
    model_type = 'general',
    recursive = TRUE,
    file_extensions = c(".jpg"),
    make_plots = TRUE,
    plot_label = TRUE,
    output_dir = NULL,
    sample50 = FALSE, 
    write_bbox_csv = FALSE, 
    overlap_correction = TRUE,
    overlap_threshold = 0.9,
    score_threshold = 0.6,
    get_metadata = FALSE,
    latitude = NA,
    longitude = NA,
    h=307,
    w=408,
    lty=1,
    lwd=2, 
    col='red'
){
  
  #-- Load operators
  load_operators()
  
  #-- Check arguments provided 
  
  # check model_type
  models_available <- c('general', 'general_v1', 
                        'species', 'species_v1', 'species_v2',
                        'family', 'family_v1',
                        'pig_only', 'pig_only_v1')
  if(!model_type %in% models_available) {
    stop(paste0("model_type must be one of the available options: ",
                list(models_available)))
  }
  
  # check ext types
  acceptable_exts <- c(".jpg", ".png", ".tif", ".pdf",
                       ".JPG", ".PNG", ".TIF", ".PDF")
  extension_test <- file_extensions %in% acceptable_exts
  if(!all(extension_test)){
    stop(paste0(c("One or more of the `file_extensions` specified is not an accepted format. Please choose one of the accepted formats: \n",
                  acceptable_exts), collapse = " "))
  }
  
  # test overlap_threshold
  if (overlap_threshold < 0 | overlap_threshold > 1){
    stop("overlap_threshold must be between [0, 1]")
  }
  
  # test score_threshold
  if (score_threshold < 0 | score_threshold > 1){
    stop("score_threshold must be between [0, 1]")
  }
  
  # check location arguments
  if (!is.na(latitude)) {
    if (latitude < -90 | latitude > 90){
      stop("latitude must be between -90 and 90")
    } 
  }
  if (!is.na(longitude)) {
    if (longitude < -180 | latitude > 180) {
      stop("longitude must be between -180 and 180")
    }
  }
  if (is.na(latitude) & !is.na(longitude)){
    stop("invalid location; please include both latitude and longitude or leave both blank")
  }
  
  if (!is.na(latitude) & is.na(longitude)){
    stop("invalid location; please include both latitude and longitude or leave both blank")
  }
  
  # test lty 
  lty_options <- 1:6
  if(!lty %in% lty_options){
    stop("invalid lty option selected. Please select an integer from 1-6")
  }
  
  # test color
  tryCatch({grDevices::col2rgb(col)}, error=function(e) {
    print('col value entered is not a valid value')})
  
  # test lwd
  if (lwd <= 0){
    stop("lwd value must be greater than 0")
  }
  
  #-- Load model

  
  # download model files
  folder <- download_cache(model_type)
  
  # load label encoder
  label_encoder <- utils::read.table(file.path(folder, "label_encoder.txt"), 
                                     sep = ":", col.names = c("label", "encoder"))
  
  # standardize label format
  label_encoder <- dplyr::mutate(label_encoder,
                                 label = gsub("'", "", label),
                                 label = gsub(" ", "_", label),
                                 label = gsub("-", "_", label),
                                 label = tools::toTitleCase(label))
  
  # load model
  model <- weight_loader(folder)
  model$eval()
  
  # load inputs
  file_list <- define_dataset(data_dir, recursive, file_extensions)
  
  # take random sample if sample50=TRUE  
  if(sample50 & length(file_list) >50){
    file_list <- sample(file_list, 50)
  }
  
  # set placeholder for predicted bboxes
  if(write_bbox_csv){
    bboxes <- NULL
  }
  
  # if output_dir was specified, search for existing results
  if(!is.null(output_dir)){
    results_path <- list.files(output_dir, 
                          pattern = paste(model_type, "model_predictions", sep = "_"),
                          full.names = TRUE, ignore.case = TRUE)
    if(length(results_path)>0){
      results <- do.call(rbind, lapply(results_path, utils::read.csv))
      results_files <- unique(normalizePath(results$filename, winslash = "/"))
      # filter file_list to images NOT in results_files
      file_list <- file_list[!file_list %in% results_files]
      cat(paste0("\nLoading saved model results from ", output_dir, 
                 "\nModel will run only on images in ", data_dir, " not already in saved results. \n"))
    }
    if(write_bbox_csv==TRUE){
      bbox_path <- list.files(output_dir,
                              pattern = paste(model_type, "predicted_bboxes", sep = "_"),
                              full.names = TRUE, ignore.case = TRUE)
      # load saved predicted bboxes
      if(length(bbox_path)>0){
        bboxes <- do.call(rbind, lapply(bbox_path, utils::read.csv))
        bboxes <- unique(bboxes)
        cat(paste0("\nLoading saved bbox results from ", output_dir, "\n"))
      }
    }
  }
  
  # make output directory
  if(is.null(output_dir)){
    datenow <- format(Sys.Date(), "%Y%m%d")
    now <- unclass(as.POSIXlt(Sys.time()))
    current_time <- paste0("_", datenow, "_", sprintf("%02d", now$hour), 
                           sprintf("%02d", now$min), 
                           sprintf("%02d", round(now$sec)))
    output_dir <- file.path(data_dir, paste0("predictions_", model_type, current_time))
  } 
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  # Write Arguments to File
  arguments <- list (
    data_dir = normalizePath(data_dir),
    model_type = model_type,
    recursive = recursive,
    file_extensions = file_extensions,
    make_plots = make_plots,
    plot_label = plot_label,
    output_dir = normalizePath(output_dir),
    sample50 = sample50, 
    write_bbox_csv = write_bbox_csv, 
    overlap_correction = overlap_correction,
    overlap_threshold = overlap_threshold,
    score_threshold = score_threshold,
    get_metadata = get_metadata,
    latitude = latitude,
    longitude = longitude,
    h=h,
    w=w,
    lty=lty,
    lwd=lwd, 
    col=col
  )
  # write file
  #lapply(arguments, cat, "\n", file=file.path(output_dir, "arguments.txt"), append=TRUE)
  sink(file.path(output_dir, "arguments.txt"))
  print(arguments)
  sink()
  
  
  #-- Make dataframe of possible labels using species range data
  if (is.na(latitude) & is.na(longitude)) {
    location <- NULL
  } else {
    location <- data.frame(longitude=longitude, latitude=latitude)
  }
  
  if(is.null(location) == FALSE){
    cat(paste0("\nDetermining possible taxa based on location using latitude ",latitude," longitude ",longitude))
    
    #Load species extent data
    extent.data <- species.extent.data
    
    #Get possible labels based on model class
    possible.labels <- get_possible_species(location, extent.data, model_type)

    cat(paste0("\nIdentified ", nrow(possible.labels), " taxa out of ", nrow(label_encoder), " possible taxa.\n"))
  }#END
  
  
  #-- Make predictions for each image
  
  # empty list to hold predictions from loop
  predictions_list <- list()
  
  # add progress bar
  cat(paste0("\nDeploying model on ", length(file_list), " images. Two warnings will appear; ignore these. \nResults files are saved every 10 images in: ", normalizePath(output_dir), "\n"))
  if(make_plots){
    cat(paste0("During deployment, you can optionally view predicted bounding boxes as they are produced."))
  }
  pb = utils::txtProgressBar(min = 0, max = length(file_list), initial = 0,
                             style=3, char="*")  
  
  # explicitly load torch packages into environment
  suppressWarnings(suppressPackageStartupMessages({
    library(torch)
    library(torchvision)
    library(torchvisionlib)
  }))

  # loop over each image
  toc <- Sys.time()
  torch::with_no_grad({
    for(i in 1:length(file_list)){
      #input <- dataLoader(file_list, index = i, w=408, h=307)
      # if any problems loading the file, catch these errors
      input <- tryCatch(data_loader(file_list, index = i, w=408, h=307),
                        error = function(e) 'error')
      if("error" %in% input){
        # set up output so that I can put into the data frame
        # get file name
        filename <- normalizePath(file_list[i], winslash = "/")
        pred_df <- data.frame(label = 0, XMin = NA, YMin = NA, XMax=NA, YMax=NA,
                              scores = 1.0, label.y = 'image_error', number_bboxes = 0,
                              'filename' = filename)
        predictions_list[[i]] <- pred_df
      } else {
        # deploy the model. suppressing warnings here, because it is not important
        defaultW <- getOption("warn")
        output <- suppressMessages({model(input)})
        options(warn = defaultW)
        
        pred_df <- decode_output(output, label_encoder, 307, score_threshold)
        
        # evaluate predictions using possible species
        if(is.null(location)==FALSE){
          pred_df<-smart_relabel(pred_df, possible.labels, label_encoder)
          pred_df<-pred_df[pred_df$prediction %in% possible.labels$label,]
        }
        
        if(nrow(pred_df)==1){
          pred_df$number_bboxes<-1
        }
        
        if(nrow(pred_df) > 1) {
          pred_df$number_bboxes<-0
          
          # address overlapping bboxes
          if(overlap_correction){
            pred_df <- reduce_overlapping_bboxes(pred_df, overlap_threshold)
          }
        }
        # add filename
        filename <- normalizePath(file_list[i], winslash = "/")
        
        # subset by score threshold for plotting
        pred_df_plot <- pred_df[pred_df$scores >= score_threshold, ]
        
        # make plots
        if(make_plots){
          plot_img_bbox(filename, pred_df_plot, output_dir, data_dir, plot_label, col,
                        lty, lwd, FALSE, w, h)
        }
        
        # when there is no predicted bounding box, create a relevant pred_df
        # first get the encoder value for the background class. This should always be zero
        if(nrow(pred_df) < 1) {
          #background_encoder <- label_encoder[which("empty"%in%label_encoder$label),]$encoder
          # pred_df[1,] <- c(0, # using 0 instead of background_encoder, because empty will always be 0
          #                  rep(NA, (ncol(pred_df)-2)),
          #                  "empty")
          pred_df <- data.frame(label = 0, XMin = 0, YMin = 0, XMax = 0, YMax = 0,
                                prediction = "Empty", number_bboxes = 0, scores = 1)
          
          # # add column for number of bboxes
          # pred_df$number_bboxes<-0
          # 
          # # add value for scores to address NA logical issues later
          # pred_df$scores<-1.0
          
        }
        
        # add full filepath to prediction
        pred_df$filename <- rep(filename, nrow(pred_df))
        
        # add prediction df to list
        predictions_list[[i]] <- pred_df
        
        # save results every 10th image
        if (i %% 10 == 0) {
          # filter df by score_threshold
          full_df <- apply_score_threshold(predictions_list, score_threshold)
          
          # convert to output format
          df_out <- write_output(full_df)
          
          # cat previous results if they exists
          if(exists("results")){
            df_out <- unique(dplyr::bind_rows(results, df_out))
          }
          
          # if saving all bboxes, make df and save to csv
          # Write Bounding Box File
          if(write_bbox_csv){
            bbox_df <- write_bbox_df(predictions_list, w, h, bboxes, score_threshold)
            utils::write.csv(bbox_df, file.path(output_dir, paste(model_type, "predicted_bboxes.csv", sep="_")), 
                             row.names=FALSE)
          }
          
          # extract metadata if requested
          if(get_metadata){
            meta_df <- extract_metadata(df_out$filename)
            # remove all NA columns
            meta_df <- remove_na(meta_df)
            #utils::write.csv(meta_df, file.path(output_dir, "metadata.csv"), row.names = FALSE)
            # join metadata to results
            df_out <- dplyr::left_join(df_out, meta_df, 
                                       dplyr::join_by(filename == FilePath), 
                                       suffix = c("", ".y"), keep=FALSE)
            # remove duplicates
            df_out <- dplyr::select(df_out, -ends_with(".y"))
          }
          
          # save predictions to csv
          utils::write.csv(df_out, file.path(output_dir, paste(model_type, 'model_predictions.csv', sep="_")), row.names=FALSE)
          
          # print update
          cat(paste0("\nResults saved for ", i, " images.\n"))
        }
      }
      
      # update progress bar
      utils::setTxtProgressBar(pb,i) 
      
    }# end for loop
    
  })
  
  tic <- Sys.time()
  runtime <- difftime(tic, toc, units="secs")
  timeper <- runtime/length(file_list)
  cat(paste0("\nInference time of: ", round(timeper, 3), " seconds per image."))
  
  
  #-- Make Output Files
  
  # filter df by score_threshold
  full_df <- apply_score_threshold(predictions_list, score_threshold)
  
  # convert to output format
  df_out <- write_output(full_df)
  
  # extract and join metadata if requested
  if(get_metadata){
    # extract metadata
    meta_df <- extract_metadata(df_out$filename)
    # remove all NA columns
    meta_df <- remove_na(meta_df)
    # join metadata to results
    df_out <- dplyr::left_join(df_out, meta_df, 
                               dplyr::join_by(filename == FilePath), 
                               suffix = c("", ".y"), keep=FALSE)
    # remove duplicate columns
    df_out <- dplyr::select(df_out, -ends_with(".y"))
    
  }
  
  # cat previous results if they exists
  if(exists("results")){
    df_out <- unique(dplyr::bind_rows(results, df_out))
  }
  
  # save predictions to csv
  utils::write.csv(df_out, file.path(output_dir, paste(model_type, 'model_predictions.csv', sep="_")), row.names=FALSE)

  
  cat(paste0("\nOutput can be found at: \n", normalizePath(output_dir), "\n",
             "The number of animals predicted in each category in each image is in the file: ", model_type, "_model_predictions.csv\n"))
  
  
  # if saving all bboxes, make df and save to csv
  # Write Bounding Box File
  if(write_bbox_csv){
    bbox_df <- write_bbox_df(predictions_list, w, h, bboxes, score_threshold)
    utils::write.csv(bbox_df, file.path(output_dir, paste(model_type, "predicted_bboxes.csv", sep="_")), row.names=FALSE)
    cat(paste0("The coordinates of predicted bounding boxes are in the file: ", model_type,  "_predicted_bboxes.csv"))
  }
  
  
  # return output dataframe
  return(df_out)

}