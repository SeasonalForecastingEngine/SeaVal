
#' restricts data to a specified country
#' Only the following 10 countries can be used in this function: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tansania, Uganda, Rwanda, Burundi
#' @param dt the data table.
#' @param ct name of the country, or vector containing multiple country names
#' @param rectangle logical. If FALSE (default), the data is restricted to the gridcells for which the centerpoint lies within the selected country (e.g. for computing mean scores for a country).
#' If TRUE, the data is kept for a rectangle containing the entire country, therefore also containing gridpoints outside the country. This is the preferred option for plotting data
#' for a specific country.
#' @param tol Only used when \code{rectangle == TRUE}. A tolerance value for widening the plotting window, making things look a bit nicer.
#'
#' @export
#' @importFrom data.table as.data.table


restrict_to_country = function(dt,ct,rectangle = FALSE,tol = 0.5)
{
  data(countries)
  cs = as.data.table(countries)[country %in% ct]
  if(!rectangle)
  {
    return(merge(dt,cs,by = c('lon','lat'))[,country := NULL])
  }
  if(rectangle)
  {
    lon_range = range(cs[,lon]) + c(-tol,tol)
    lat_range = range(cs[,lat]) + c(-tol,tol)

    return(dt[lon %between% lon_range & lat %between% lat_range])
  }
}
