% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scores.R
\name{MSE}
\alias{MSE}
\title{Taking MSEs of ensemble forecasts stored in long data tables. Can also handle point forecast}
\usage{
MSE(
  dt,
  f,
  o = "obs",
  by = by_cols_ens_fc_score(),
  pool = "year",
  mem = "member",
  dim.check = T
)
}
\arguments{
\item{dt}{Data table containing the predictions.}

\item{f}{column name of the prediction.}

\item{o}{column name of the observations.}

\item{by}{column names of grouping variables, all of which need to be columns in dt.
Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.}

\item{pool}{column name(s) for the variable(s) along which is averaged, typically just 'year'.}

\item{mem}{Name of the column identifying the ensemble member. Only used if check_dimension is TRUE. Is NULL for a point forecast.}

\item{dim.check}{Logical. If True, a simple test whether the dimensions match up is conducted:
The data table should only have one row for each level of c(by,pool,mem)}
}
\description{
Taking MSEs of ensemble forecasts stored in long data tables. Can also handle point forecast
}
