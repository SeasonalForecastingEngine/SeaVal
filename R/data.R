#' Monthly mean precipitation
#'
#' This dataset contains observed monthly mean precipitation for the greater horn of Africa,
#' for November - December 1991-2020. The unit of precipitation is mm/day. It also contains the tercile category, where -1
#' means below normal rainfall (lowest tercile for this location and month), 0
#' is normal and 1 is above normal.The data source is CHIRPS-blended, upscaled
#' to a half-degree grid.
#'
#' @docType data
#'
#' @usage data(chirps_monthly)
#'
#' @source
#'   \url{http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/}
"chirps_monthly"

#' Monthly mean precipitation forecast example dataset
#'
#' This is a small example dataset containing hindcasts of monthly mean precipitation for illustration purposes.
#' The forecasts are contained for the entire GHA-region, for November and December 2018-2020.
#' The forecasts are issued by the ECMWF SEAS 5 model and initialized in August.
#' The unit of precipitation is mm/day. Only the first 3 ensemble members are
#' provided. The dataset also contains tercile probability forecasts, which are
#' derived from the full 51 member ensemble. The probability for a tercile for a
#' given year, month and location is always computed as the fraction of ensemble
#' members falling into that tercile, computed from all ensemble predictions for
#' the month and location under consideration.
#' This dataset was generated using Copernicus Climate Change Service information (2020).
#'
#' @docType data
#'
#' @usage data(ecmwf_monthly)
#'
#' @source \url{https://cds.climate.copernicus.eu}
"ecmwf_monthly"
