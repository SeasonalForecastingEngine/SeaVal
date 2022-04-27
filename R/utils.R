#' adds a column with the countryname to a data table
#' Only the following 10 countries are added: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tansania, Uganda, Rwanda, Burundi
#' @param dt the data table.
#'
#' @export
#' @importFrom data.table as.data.table

add_country_names = function(dt)
{
  data(countries)
  cs = as.data.table(countries)
  return(merge(dt,cs,by = c('lon','lat')))
}

#' adds a column with the tercile category to a data table
#' @param dt the data table.
#' @param datacol Name of the column where the data is stored
#' @param by names of columns to group by
#'
#' @export
#' @importFrom data.table as.data.table

add_tercile_cat = function(dt,datacol = 'prec',by = intersect(c('month','lon','lat'),names(dt)))
{
  dt[!(is.na(get(datacol))),tercile_cat := -1*(get(datacol) <= quantile(get(datacol),0.33)) + 1 *(get(datacol) >= quantile(get(datacol),0.67)),by = by]
  return(dt)
}

#' Returns a leave-one-year-out climatology-based ensemble forecast
#'
#' for a given year, the ensemble forecast simply consists of the observations in all other years.
#' This is essentially an auxiliary function for computing skill scores relative to climatology.
#'
#' @param obs_dt Data table containing observations, must contain a column 'year'.
#' @param by character vector containing the column names of the grouping variables, e.g. \code{c('month','lon','lat')}.
#'
#' @return Long data table with the typical ensemble-forecast looks, i.e. containing a column 'member'.
#' @export

climatology_ens_forecast = function(obs_dt,
                                    by)
{
  years = unique(obs_dt[,year])

  ret_dt = data.table()
  for(yy in years)
  {
    dt_temp = obs_dt[year != yy][,member := 1:.N,by = by][,year:=yy]
    ret_dt = rbindlist(list(ret_dt,dt_temp))
  }
  return(ret_dt)
}

#' Get climatological prediction for exceedence probabilities.
#'
#' The climatological prediction for exceedence probabilities is the fraction of observed years where the observation exceeded the threshold.
#' It's calculated from leave-one-year-out climatology.
#'
#' @param obs_dt Data table containing observations.
#' @param o column name of the observation. Mostly observed precipitation in mm.
#' @param by By which columns should be grouped?
#' @param thresholds vector of thresholds for which the exceedence probabilities should be derived.
#'
#' @export

climatology_threshold_exceedence = function(obs_dt,
                                            o = 'prec',
                                            by = setdiff(coords(obs_dt),'year'),
                                            thresholds = c(200,300,350,400))
{
  clim_dt = climatology_ens_forecast(obs_dt,by = by)
  ret_dt = data.table()
  for(thr in thresholds)
  {
    thr_dt = clim_dt[,.(pexcd = mean(get(o) > thr)),by = c(by,'year')]
    thr_dt[,threshold := thr]
    ret_dt = rbindlist(list(ret_dt,thr_dt))
  }
  return(ret_dt)
}


#' Converts time given as 'months since date' (MSD) into years and months (YM)
#' @param dt a data table.
#' @param timecol name of the column containing the time.
#' @param origin The time column contains time in the format month since which date?
#'
#' @return data table with two new columns 'month' and 'year', the timecol is deleted.
#'
#' @export
#' @importFrom data.table as.data.table


MSD_to_YM = function(dt,timecol = 'time',origin = '1981-01-01')
{
  or_mon = month(as.Date(origin))
  or_year = year(as.Date(origin))

  mons = (dt[,get(timecol)] + or_mon ) %% 12

  # in case the middle of the month is supplied (i.e. non-integer times):
  mons = floor(mons)

  mons[mons == 0] = 12

  years = (dt[,get(timecol)] + or_mon ) %/% 12 + or_year
  years[mons == 12] = years[mons == 12] - 1

  dt[,c('year','month') := list(years,mons)]
  dt[,(timecol) := NULL]

  return(dt)
}


#' restricts data to a specified country
#' Only the following 10 countries can be used in this function: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tansania, Uganda, Rwanda, Burundi.
#' This function assumes the lon/lat grid to be a regular half degree or full degree grid.
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


#' restricts data to CONFER region
#' Only the following 10 countries can be used in this function: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tanzania, Uganda, Rwanda, Burundi
#' @param dt the data table.
#' @param rectangle logical. If FALSE (default), the data is restricted to the gridcells for which the centerpoint lies within the selected country (e.g. for computing mean scores for a country).
#' If TRUE, the data is kept for a rectangle containing the entire country, therefore also containing gridpoints outside the country. This is the preferred option for plotting data
#' for a specific country.
#' @param tol Only used when \code{rectangle == TRUE}. A tolerance value for widening the plotting window, making things look a bit nicer.
#'
#' @export
#' @importFrom data.table as.data.table


restrict_to_confer_region = function(dt,rectangle = FALSE,tol = 0.5)
{
  data(countries)
  cs = as.data.table(countries)
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
