% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sequences.R
\name{generate_sequences}
\alias{generate_sequences}
\title{Extract sequence and metadata info and assign info to image filename}
\usage{
generate_sequences(
  data_dir = NULL,
  cam_prefix = NULL,
  burst_length = 1,
  wait_time = 0
)
}
\arguments{
\item{data_dir}{absolute path to image directory or list of absolute path of image file names. If you specify a 
directory, the function will search recursively through this directory for all jpeg files.}

\item{cam_prefix}{character vector, where each value corresponds to a unique name 
for each camera in your dataset as it appears in the absolute path. The function will use
this identifier if your camera metadata do not contain serial numbers.}

\item{burst_length}{number of images per sequence burst.}

\item{wait_time}{time between bursts in seconds. If your cameras have a 10-minute wait time 
between triggers, your wait time is 600.}
}
\description{
create df of metadata and sequence information for all images in user image directory
}
\details{
This function extracts metadata and generates sequence information for your image dataset. 
You may either specify a path to your image directory or a character vector listing absolute paths 
to all images in your dataset. If your image metadata includes camera and sequence information, 
these will be returned. If you provide a string vector of camera ids represented in your directory, 
sequence length and wait time between triggers, camera and sequence identifiers based on directory
and timestamp information will also be returned. This function utility depends on either the 
sufficient metadata fields in each image or reasonable directory organization in order to generate 
accurate sequence information. Sequences with different burst lengths must  be run through the function 
separately if relying on `cam_prefix` and image timestamp to generate sequence information.
}
\examples{
data(preds)
seq_preds <- generate_sequences(preds$filename, c("example_set"), 5, 300)

}
