% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_wrangling.R
\name{cv_to_dt}
\alias{cv_to_dt}
\title{Loading cross-validation data}
\usage{
cv_to_dt(data_dir)
}
\arguments{
\item{data_dir}{path to where the cross-validation is stored.}
}
\value{
A data table.
}
\description{
This function loads cross-validation data (predictions and observations) from the provided directory.
The data is averaged over the target months specified in the file name.
}
