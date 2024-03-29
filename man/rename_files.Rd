% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_files.R
\name{rename_files}
\alias{rename_files}
\title{Rename Files}
\usage{
rename_files(
  img_dir,
  project_id = "",
  site_id = "",
  check_date = "",
  camera_id = ""
)
}
\arguments{
\item{img_dir}{character string; full path to image folder}

\item{project_id}{character string; name of project or survey}

\item{site_id}{character string; name of site}

\item{check_date}{character string; date images were collected}

\item{camera_id}{character string; name of camera}
}
\description{
rename files in an image directory
}
\details{
Cameras often default to identical naming conventions, which can create issues
during image processing if multiple different image files in different folders
have the same file name. This function renames all the images in a folder
by appending user-provided category ids to each file name. This function does 
NOT search recursively through sub-folders of a directory and must be deployed 
on each folder separately, although iteration could easily automate this process.
}
