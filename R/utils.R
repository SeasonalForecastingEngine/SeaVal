#' adds country names to a data table containing lons and lats (on half or full degrees).
#' Only the 10 countries specified in the CONFER proposal are added (Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tansania, Uganda, Rwanda, Burundi)
#' @param dt the data table.
#'
#' @export
#' @importFrom data.table as.data.table


add_countries = function(dt)
{
  data(countries)
  cs = as.data.table(countries)
  return(merge(dt,cs,by = c('lon','lat')))
}
