# -- Template script to run CameraTrapDetectoR on a high performance computing system
# -- Author: Amira Burns
# -- Last Updated: 21 Feb 2023


# Install and load CameraTrapDetectoR if it's not already installed
if(!"CameraTrapDetectoR" %in% rownames(installed.packages())){
  stop("Please download CameraTrapDetectoR in an interactive session to accept dependencies.")
}

# Install and load argparse for accepting positional arguments
if(!"argparse" %in% rownames(installed.packages())){
  install.packages("argparse")
}

# load packages
library(CameraTrapDetectoR)
suppressPackageStartupMessages(library("argparse"))

# create parser object
parser <- ArgumentParser(description = "Run CameraTrapDetectoR on command line")
out
# add argument options for deploy_model function
parser$add_argument("-v", "--verbose", action="store_true", default=TRUE,
                    help="Print extra output [default]")
parser$add_argument("-q", "--quietly", action="store_false", 
                    dest="verbose", help="Print little output")
parser$add_argument("-d", "--data_dir", type="character",
                    help="Absolute path to image directory")
parser$add_argument("--model_type", type="character", default='general',
                    help="Model to run. Choices are 'general', 'family', 'species', 'pig_only'.")
parser$add_argument("-r", "--recursive", type='logical', default=TRUE,
                    help="Search data_dir recursively for images to run.")
parser$add_argument("--redownload", type='logical', default=TRUE,
                    help="Download latest model weights.")
parser$add_argument("-f", "--file_extensions", choices=c('.jpg','.png','.tif','.pdf'),
                    default='.jpg', nargs='+',
                    help="Types of image files to search for within data_dir. 
                    Accepts '.jpg', '.png', '.tif', '.pdf', filetypes and is case insensitive.")
parser$add_argument("-p", "--make_plots", type='logical', default=TRUE,
                    help="Make plots of images with the model-predicted bounding boxes")
parser$add_argument("-l", "--plot_label", type='logical', default=TRUE,
                    help="Label model-predicted bounding boxes in image plots")
parser$add_argument("-o", "--output_dir", type='character', default=NULL,
                    help="Absolute path to output directory to store results")
parser$add_argument("--sample50", type='logical', default=F,
                    help="Run the model on a random sample of 50 images.")
parser$add_argument("--write_bbox_csv", type='logical', default=TRUE,
                    help="Return csv. file with coordinates for all predicted bounding boxes.")
parser$add_argument("--overlap_correction", type='logical', default=T,
                    help="Should overlapping detections be evaluated for overlap
                    and highest confidence detection be returned.")
parser$add_argument("--overlap_threshold", type='double', default=0.9,
                    help="Overlap threshold to determine if overlapping bounding 
                    boxes should be considered a single detection. Accepts values 0-1")
parser$add_argument("--score_threshold", type='double', default=0.6,
                    help="Confidence threshold for returning predictions.
                    Accepts values 0-1")
parser$add_argument("--prediction_format", type='character', default='long',
                    help='format for prediction results. Accepts values `wide`, `long`.')
parser$add_argument("--latitude", help="Image location latitude. Use only if all images in 
                    `data_dir` come from the same location.")
parser$add_argument("--longitude", help="Image location longitude. 
                    Use only if all images in `data_dir` come from the same location.")
parser$add_argument("--height", type='integer', default=307,
                    help="Image height in pixels for annotated plot")
parser$add_argument("--width", type='integer', default=408,
                    help="Image width in pixels for annotated plot")
parser$add_argument("--lty", type='integer', default=1,
                    help="Line type for bbox in annotated plot. Accepts integer values 1-6")
parser$add_argument("--lwd", type='double', default=1,
                    help="Line width for bbox in annotated plot.")
parser$add_argument("--col", type='character', default="red",
                    help="Line color for bbox in annotated plot.")

# get command line options
args <- parser$parse_args()

# print verbose output


# run CameraTrapDetectoR with user arguments
dat <- deploy_model(
  data_dir = args$data_dir,
  model_type = args$model_type,
  recursive = args$recursive,
  redownload = args$redownload,
  file_extensions = args$file_extensions,
  make_plots = args$make_plots,
  plot_label = args$plot_label,
  output_dir = args$output_dir,
  sample50 = args$sample50, 
  write_bbox_csv = args$write_bbox_csv, 
  overlap_correction = args$overlap_correction,
  overlap_threshold = args$overlap_threshold,
  score_threshold = args$score_threshold,
  prediction_format = args$prediction_format,
  latitude = args$latitude,
  longitude = args$longitude,
  h=args$height,
  w=args$width,
  lty=args$lty,
  lwd=args$lwd, 
  col=args$col
)
