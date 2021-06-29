

#' plotting spatial data for one country
#'
#' @description This function wraps \code{ggplot_dt}.
#'
#' @param dt Data table containing the data for plotting.
#' @param data_col The name of the column in dt containing the data for plotting.
#' @param country Name of the country to plot. Possible values are: "Burundi", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Rwanda", "Somalia", "South Sudan", "Sudan", "Tanzania", "Uganda"
#' @param tol length by which the plot window is widened (in lon/lat, so tol = 0.5 includes roughly one pixel beyond the country borders, when your data is on a half degree grid).
#' @param ... passed on to ggplot_dt
#'
#' @return a ggplot object.
#'
#' @import data.table
#' @import ggplot2
#'
#' @export
#'
#' @author Claudio Heinrich


countryplot_dt = function(dt,
                          data_col = colnames(dt)[3],
                          country,tol = 0,
                          ...)
{
  data(countries)
  country_new = as.data.table(countries)
  setnames(country_new,'country','count')
  coords = country_new[count == country]

  bbox_lon = range(coords[,lon])+c(-tol,tol)
  bbox_lat = range(coords[,lat])+c(-tol,tol)

  dt_sub = dt[lon %between% bbox_lon & lat %between% bbox_lat]

  pp = ggplot_dt(dt_sub,data_col = data_col,tol = 0,...)

  return(pp)

}

