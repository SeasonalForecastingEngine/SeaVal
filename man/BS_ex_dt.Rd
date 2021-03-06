% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scores.R
\name{BS_ex_dt}
\alias{BS_ex_dt}
\title{Calculate exceedence Brier score}
\usage{
BS_ex_dt(
  dt,
  fc,
  threshold_col,
  obs = "obs",
  by = intersect(c("month", "season", "lon", "lat", "system", "lead_time"), names(dt)),
  pool = c("year")
)
}
\arguments{
\item{dt}{Data table containing the predictions.}

\item{fc}{column name of the prediction. Contains predicted probabilities of exceedence}

\item{threshold_col}{which column contains the exceedence threshold?}

\item{obs}{column name of the observations. Can either be logical (exceedence or not) or real valued, containing precipitation (or the variable for which exceedence should be checked).}

\item{by}{column names of grouping variables, all of which need to be columns in dt.
Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.}

\item{pool}{column name(s) for the variable(s) over which is averaged. Typically just 'year'.}
}
\description{
Calculate exceedence Brier score
}
