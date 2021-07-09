#' Monthly mean precipitation
#'
#' This dataset contains observed monthly mean precipitation for October - December 1981-2020.
#' The unit of precipitation is mm/day. Only the greater horn of Africa is covered. It also contains the tercile category, where -1 means below normal rainfall
#' (lowest tercile for this location and month), 0 is normal and 1 is above normal.
#' The data source is CHIRPS-blended, upscaled to a half-degree grid.
#'
#' @docType data
#'
#' @usage data(chirps_monthly)
#'
#' @source \href{http://digilib.icpac.net/SOURCES/.ICPAC/.CHIRPS-BLENDED/.monthly/.rainfall/.precipitation/}
#'
#' @example print(data(chirps_monthly))
"chirps_monthly"

#' Monthly mean precipitation forecast example dataset
#'
#' This dataset contains hindcasts of the monthly mean precipitation for October - December 1993-2020. The forecasts are issued by the ECMWF SEAS 5 model and initialized in August.
#' The unit of precipitation is mm/day. Only the greater horn of Africa is covered, and only the first 5 ensemble members are provided. The dataset also contains tercile probability forecasts,
#' which are derived from the full 25 member ensemble. The probability for a tercile for a given year, month and location is always computed as the fraction of ensemble members falling into that
#' tercile, computed from all ensemble predictions for the month and location under consideration.
#'
#' @docType data
#'
#' @usage data(ecmwf_monthly)
#'
#' @source \href{https://cds.climate.copernicus.eu}
#'
#' @example print(data(ecmwf_monthly))
"ecmwf_monthly"
