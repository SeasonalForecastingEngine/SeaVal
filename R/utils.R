#' Takes a data table with lon/lat coordinates and returns the same data table with a new column 'country' containing the countryname
#' of each coordinate. Usually you'd do this using sf, but that introduces dependency to gdal which we want to avoid.
#'
#' @param dt the data table.
#' @param load_continent which continent to load. Loading only one continent makes the function faster.
#' Put NULL if you have data across several continents
#'
#' @export
#' @importFrom rnaturalearth ne_countries
#' @importFrom sp coordinates proj4string over

add_country_names = function(dt,load_continent = 'Africa')
{
  world <- rnaturalearth::ne_countries(scale = "large", continent = 'Africa')

  coords = unique(dt[,.(lon,lat)])
  # get coords as spatial points:
  sp_lonlat = data.table(x = coords[,lon], y = coords[,lat])
  sp::coordinates(sp_lonlat) = c('x','y')
  sp::proj4string(sp_lonlat) = sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

  #in which country does each point fall?
  cs = sp::over(sp_lonlat,world)

  coords[,country := cs$geounit]

  return(merge(dt,coords,by = c('lon','lat')))
}

#' adds a column with the tercile category to a data table
#' @param dt the data table.
#' @param datacol Name of the column where the data is stored
#' @param by names of columns to group by
#'
#' @export
#' @importFrom stats quantile

add_tercile_cat = function(dt,datacol = 'prec',by = intersect(c('month','lon','lat'),names(dt)))
{
  tercile_cat = NULL
  dt[!(is.na(get(datacol))),tercile_cat := -1*(get(datacol) <= stats::quantile(get(datacol),0.33)) + 1 *(get(datacol) >= stats::quantile(get(datacol),0.67)),by = by]
  return(dt)
}


#' Adds columns 'below', 'normal' and 'above', containing predicted tercile probabilities, to a data table with ensemble forecasts.
#' The data table should either already have a column 'tercile_cat' (as would be added by \code{add_tercile_cat}),
#' or \code{add_tercile_cat} will be run first.
#'
#' @param dt the data table.
#' @param by names of columns to group by
#' @param ... passed on to \code{add_tercile_cat}.
#'
#' @export

add_tercile_probs = function(dt,by = setdiff(dimvars(dt),'member'),...)
{
  if(! ('member' %in% names(dt))) stop('This only works for ensemble forecasts, so I need a column named "member"')
  if(!'tercile_cat' %in% names(dt))
  {
    dt = add_tercile_cat(dt,...)
  }

  dt[,c('below','normal','above') :=
       .(mean(tercile_cat == -1),
         mean(tercile_cat == 0),
         mean(tercile_cat == 1)),by = by]
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
                                            by = setdiff(dimvars(obs_dt),'year'),
                                            thresholds = c(200,300,350,400))
{
  # for devtools::check():
  threshold = NULL

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


#' Combine two data tables
#'
#' Convenience wrapper for data.tables merge. Merges by dimvars and returns user-friendly warnings.
#'
#' @param dt1 first data table
#' @param dt2 second data table
#' @param ... passed on to data.table::merge
#'
#' @export

combine = function(dt1,dt2,...)
{
  dv1 = dimvars(dt1)
  dv2 = dimvars(dt2)
  common_dimvars = intersect(dv1,dv2)
  if(length(common_dimvars)==0)
  {
    stop('The data tables do not seem to have any common dimension variables, so I cannot combine them.')
  }

  common_cols = intersect(setdiff(names(dt1),dv1),setdiff(names(dt2),dv2))

  if(length(common_cols) >0)
  {
    warning(paste0('The columns ',paste(common_cols,collapse = ', '),' were contained in both data tables but are not recognized as dimension variables.\n
Their names have changed. If that is not what you wanted use data.table::merge instead.'))
  }
  ret_dt = merge(dt1,dt2,by = common_dimvars,...)
  if(ret_dt[,.N] == 0) error('The resulting data table is empty. Did the two data tables use different spatial grids?')

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
#'
#' Restricts a dataset to one or more countries, specified by their names. If you have lon/lat data and don't know which countries these coordinates belong to, see \code{add_country_names}.
#'
#' @param dt the data table.
#' @param ct name of the country, or vector containing multiple country names
#' @param rectangle logical. If FALSE (default), the data is restricted to the gridcells for which the centerpoint lies within the selected country (e.g. for computing mean scores for a country).
#' If TRUE, the data is kept for a rectangle containing the entire country, therefore also containing gridpoints outside the country. This is the preferred option for plotting data
#' for a specific country.
#' @param tol Only used when \code{rectangle == TRUE}. A tolerance value for widening the plotting window, making things look a bit nicer.
#'
#' @export
#'
#' @importFrom utils data


restrict_to_country = function(dt,ct,rectangle = FALSE,tol = 0.5)
{
  # for devtools::check():
  country = lon = lat = NULL

  cs = unique(add_country_names(dt)[country %in% ct,.(lon,lat,country)])
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
#'
#' Wraps restrict_to_country, and restricts to the GHA-region usually considered in the Horizon2020 project CONFER, consisting
#' of the following countries: 'Burundi','Djibouti', 'Eritrea','Ethiopia','Kenya','Rwanda','Somalia','Somaliland','South Sudan','Sudan','Tanzania','Uganda'
#' @param dt the data table.
#' @param ... passed on to restrict_to_country
#'
#' @export
#' @importFrom data.table as.data.table

restrict_to_confer_region = function(dt,...)
{
  confer_countries = c('Burundi','Djibouti', 'Eritrea','Ethiopia','Kenya','Rwanda','Somalia','Somaliland','South Sudan','Sudan','Tanzania','Uganda')

  dt = restrict_to_country(dt,ct = confer_countries,...)
  return(dt)
}

#' Get tercile probability forecast from ensemble forecasts
#'
#' The function takes a data table containing ensemble predictions and reduces it to predicted tercile probabilities.
#' The data table should either have a column 'tercile_cat' or it will be generated in the process (by \code{add_tercile_cat}).
#' In particular, if you don't know the tercile category of the ensemble predictions, your data table should contain hindcasts as well,
#' such that the tercile categories are calculated correctly.
#' The probability for 'below', for example, is the fraction of ensemble members predicting below normal (for this coordinate).
#'
#' @param dt the data table.
#' @param by names of columns to group by
#' @param keep_cols A vector of column names that you want to keep. Column names in by are kept automatically.
#' @param ... passed on to \code{add_tercile_probs}.
#'
#' @export
tfc_from_efc = function(dt, by = setdiff(dimvars(dt),'member'), keep_cols = NULL,...)
{
  keep_cols = unique(c(by, keep_cols, 'below','normal','above'))
  dt = add_tercile_probs(dt,by,...)
  dt = unique(dt[,.SD,.SDcols = keep_cols])
  return(dt)
}
