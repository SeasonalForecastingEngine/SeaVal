% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scores.R
\name{crps_aux_esc}
\alias{crps_aux_esc}
\title{Auxiliary function for calculating crps with ensemble size correction by Ferro et al. 2008.
Mostly copy-paste from scoringRules::crps_edf. Adjusted to the data table format, where the observation is a vector of the same length as the ensemble forecast,
but is just repeated (which is why only y[1]) is used.}
\usage{
crps_aux_esc(y, dat)
}
\description{
Auxiliary function for calculating crps with ensemble size correction by Ferro et al. 2008.
Mostly copy-paste from scoringRules::crps_edf. Adjusted to the data table format, where the observation is a vector of the same length as the ensemble forecast,
but is just repeated (which is why only y[1]) is used.
}
