% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_dependencies.R
\name{install_dependencies}
\alias{install_dependencies}
\title{Function to install packages}
\usage{
install_dependencies(
  packages = c("torchvision", "torch", "magick", "torchvisionlib", "shiny", "shinyFiles",
    "shinyBS", "shinyjs", "rappdirs", "fs", "operators")
)
}
\arguments{
\item{packages}{packages to be installed, as a vector. If you are planning to use 
the Shiny App, leave this value as default. If you want to avoid installing the
Shiny-specific dependencies, use \code{packages=c('torchvision', 'torch', 'magick')}}
}
\value{
note that all dependencies are installed or need to be installed
}
\description{
This function will install packages that are needed in CameraTrapDetectoR
}
