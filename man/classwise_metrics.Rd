% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classwise_metrics.R
\name{classwise_metrics}
\alias{classwise_metrics}
\title{Calculate class-wise evaluation metrics}
\usage{
classwise_metrics(df)
}
\arguments{
\item{df}{dataframe formatted upstream in `eval_model` of predictions joined 
to ground truths, either at the image level or at the sequence level}
}
\value{
a dataframe of evaluation metrics
}
\description{
Calculates the precision, recall, and F1 scores for a set of 
predictions joined to a set of corresponding annotations. Helper function for 
`eval_model`
}
