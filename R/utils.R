
#' Add climatology to a data table
#'
#' The climatology is the average over years (and members for ensemble forecases),
#' taken separately for each month, season, and coordinate.
#' By default, the average is taken over all years in the data table,
#' but you can change this using the years-argument.
#' By default, climatologies (averages) are calculated for each column that is not
#' recognized as dimension variable and does not contain characters.
#'
#'
#' @param dt the data table.
#' @param years The average over which years should be considered as climatology.
#' The default is all years in dt.
#' @param data_cols For which columns do you want to derive the climatology?
#' The default i
#' @param by column names to group by.
#'
#' @return The provided data table with an extra climatology column
#'
#' @examples
#' dt = add_climatology(chirps_monthly)
#' @export
#'

add_climatology = function(dt,data_cols = NULL,years = NULL,by = dimvars(dt))
{

  if(!('year' %in% names(dt)))
  {
    stop('Your data does not contain a column "year".\nClimatology is usually the average over all years in the data,
so you need to have several years of data.')
  }
  if(is.null(years)) years = unique(dt[,year])

  by = setdiff(by,c('year','member'))

  if(is.null(data_cols))
  {
    data_cols = intersect(c(obs_cols(),fc_cols()),names(dt))

    if(length(data_cols) > 1)
    {
      message(paste0('Climatology is calculated for the columns ',paste(data_cols,collapse = ', '),'.\n',
                     'Use data_cols if that is not what you want.'))

    }
  }

  if(length(data_cols) > 1)
  {
  dt[,paste0('clim_',data_cols) := lapply(data_cols,function(x) mean(get(x))), by = by]
  } else {
    dt[,'clim' := mean(get(data_cols),na.rm = TRUE),by = by]
  }
  message(paste0('The climatology is based on the years ',min(years),'-',max(years),'.\n
It is calculated separately for each ',paste(by,collapse = ', ')))
  return(dt)
}


#' Add country names to a data table with lon/lat coordinates
#'
#' @description Takes a data table with lon/lat coordinates and adds a column
#' 'country' to it, containing the name of the country, the coordinate belongs to.
#'
#'
#' @param dt the data table.
#' @param regions Character vector of country names for which shapefiles are loaded.
#' By default, countries in East Africa are loaded, see \code{\link{EA_country_names}}.
#' If you set regions = '.', the entire world is loaded, but this makes the function slower.
#'
#' @return The provided data table with an extra column with country names
#'
#' @examples
#' dt = add_country_names(chirps_monthly)
#'
#' @export


add_country_names = function(dt,regions = EA_country_names())
{
  country = NULL

  if('country' %in% names(dt))
  {return(dt)}

  coords = unique(dt[,.(lon,lat)])
  coords[,country:= maps::map.where(x = lon,y = lat)]

  dt_new = merge(dt,coords,by = c('lon','lat'),sort = FALSE) # sort = FALSE for next line
  # add column to provided data table:
  dt[,country:=dt_new[,country]]

  return(dt)
}

#' Same as add_country_names
#'
#' @description This is a synonyme for \code{\link{add_country_names}}.
#' Following a more intuitive naming convention, that is more in-line
#' with \code{add_climatology} and \code{add_tercile_cat}.
#' @param dt the data table.
#' @param regions Character vector of country names for which shapefiles are loaded.
#'
#' @return The provided data table with an extra column with country names
#'
#' @examples
#' dt = add_country(chirps_monthly)
#'
#' @export

add_country = add_country_names

#' Add a tercile-category column to a data table
#'
#' Given a data table with multiple years of data, this function derives the tercile category per year.
#' It first derives terciles for the data and then returns, for each row, a -1 if the data falls into
#' the lowest tercile, 0 if it falls between 1st and second tercile, and +1 if it falls above the third tercile.
#' Allows grouping by levels (e.g. months and location-coordinates): Tercile categories are derived separately
#' for each level.
#'
#' @param dt the data table.
#' @param datacol Name of the column where the data is stored. If NULL, the function guesses.
#' @param years Optional, if provided only these years are used for establishing climatology terciles.
#' @param by names of columns to group by.
#'
#' @return The provided data table with an extra column tercile_cat
#'
#' @examples
#' \donttest{
#' dt = add_tercile_cat(chirps_monthly)
#' }
#' @export
#' @importFrom stats quantile

add_tercile_cat = function(dt,
                           datacol = NULL,
                           years = NULL,
                           by = setdiff(dimvars(dt),c('year','member')))
{
  lower_tercile = upper_tercile = tercile_cat = NULL
  # dt = dt[!is.na(get(datacol))] If you have this one in here, it does not add a column to existing object

  if(is.null(datacol)) datacol = intersect(c(obs_cols(),fc_cols()),names(dt))[1]
  if(length(datacol) == 0) stop("I don't understand which column contains the values to base terciles on.")

  if(!is.null(years))
  {
    terciles = dt[year %in% years,.(lower_tercile = stats::quantile(get(datacol),0.33),
                                    upper_tercile = stats::quantile(get(datacol),0.67)), by = by]
    dt = merge(dt,terciles,by = by)
    dt[,tercile_cat := -1*(get(datacol) <= lower_tercile) + 1 *(get(datacol) >= upper_tercile)]
    dt[,c('lower_tercile','upper_tercile') := NULL]
  } else {
    dt[,tercile_cat := -1*(get(datacol) <= stats::quantile(get(datacol),0.33)) +
         1 *(get(datacol) >= stats::quantile(get(datacol),0.67)),by = by]
  }

  return(dt)
}

#' Add tercile probabilities to ensemble forecasts
#'
#' @description Adds columns 'below', 'normal' and 'above', containing predicted tercile probabilities, to a data table with ensemble forecasts.
#' The predicted probability is always the fraction of members ending up in the respective tercile.
#' The data table should either already have a column 'tercile_cat' (added by \code{add_tercile_cat}),
#' or \code{add_tercile_cat} will be run first.
#'
#' @param dt the data table.
#' @param f name of the column containing the forecast.
#' @param by names of columns to group by
#' @param ... passed on to \code{add_tercile_cat}.
#'
#' @return The provided data table, with added columns 'above', 'normal', and 'below'
#'
#'@examples
#'\donttest{
#' dt = add_tercile_probs(ecmwf_monthly)
#'}
#'
#' @export

add_tercile_probs = function(dt,f = NULL,by = setdiff(dimvars(dt),'member'),...)
{
  if(!('member' %in% names(dt))) stop('This only works for ensemble forecasts, so I need a column named "member"')
  if(length(intersect(tc_cols(),names(dt)))==0)
  {
    if(is.null(f)) f = fc_cols(dt)
    dt = add_tercile_cat(dt,datacol = f,...)
  }

  tc_col = tc_cols(dt)
  dt[,c('below','normal','above') :=
       .(mean(get(tc_col) == -1),
         mean(get(tc_col) == 0),
         mean(get(tc_col) == 1)),by = by]
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
#'
#' @examples
#' \donttest{
#' dt = climatology_ens_forecast(chirps_monthly)
#' }
#'
#' @export

climatology_ens_forecast = function(obs_dt,
                                    by = setdiff(dimvars(obs_dt),'year'))
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
#' @return Data table with the climatological probabilities of exceedence for the provided thresholds.
#'
#' @examples
#' \donttest{
#' dt = climatology_threshold_exceedence(chirps_monthly)
#' }
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
#' @description Function for combining two data tables, e.g. with predictions and observations.
#' This is a user-friendly wrapper for \code{\link[data.table]{merge}}. It guesses the columns to merge by (the dimension variables
#' contained in both data tables) and adds some warnings when merges are attempted that are likely not correctly specified by the user.
#'
#' @param dt1 first data table
#' @param dt2 second data table
#' @param ... passed on to data.table::merge
#'
#'@return The merged data table
#'
#'@examples
#'# merge ECMWF-forecasts and CHIRPS observations:
#'dt = ecmwf_monthly[month == 11]
#'setnames(dt,'prec','forecast') # forecasts and observations both have a column 'prec'
#'dt_new = combine(dt,chirps_monthly)
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
If this is meant to be a dimension variable, use data.table::merge instead. Else it is probably better to change column names using data.table::setnames.'))
  }
  ret_dt = merge(dt1,dt2,by = common_dimvars,...)
  if(ret_dt[,.N] == 0) stop('The resulting data table is empty. Did the two data tables use different spatial grids?')

  return(ret_dt)
}


#' Converts time given as 'months since date' (MSD) into years and months (YM)
#' @param dt a data table.
#' @param timecol name of the column containing the time.
#' @param origin The time column contains time in the format month since which date?
#'
#' @return data table with two new columns 'month' and 'year', the timecol is deleted.
#'
#'@examples
#'dt = MSD_to_YM(data.table(time = 0:12))
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
#' Restricts a dataset to one or more countries, specified by their names. If you have lon/lat data and don't know
#' which countries these coordinates belong to, see \code{\link{add_country_names}}. Can restrict data to a rectangle around a given country
#' as well (usually looks nicer for plotting).
#'
#' @param dt the data table.
#' @param ct name of the country, or vector containing multiple country names
#' @param rectangle logical. If FALSE (default), the data is restricted to the gridcells for which the centerpoint lies within the selected country (e.g. for computing mean scores for a country).
#' If TRUE, the data is kept for a rectangle containing the entire country, therefore also containing gridpoints outside the country. This is the preferred option for plotting data
#' for a specific country.
#' @param tol Only used when \code{rectangle == TRUE}. A tolerance value for widening the plotting window, making things look a bit nicer.
#'
#'@return the data table, restricted to the selected country
#'
#'@examples
#'# example data:
#'ex_dt = chirps_monthly[lat < 0 & month == 11 & year == 2020]
#'dt = restrict_to_country(ex_dt,'Kenya')
#'
#' @export
#'
#' @importFrom utils data


restrict_to_country = function(dt,ct,rectangle = FALSE,tol = 1)
{
  # for devtools::check():
  country = lon = lat = NULL
  country_included = ('country' %in% names(dt))
  if(!country_included) dt = add_country_names(dt,regions = ct)
  cs = unique(dt[country %in% ct,.(lon,lat,country)])
  if(!rectangle)
  {
    ret = merge(dt,cs,by = intersect(names(dt),names(cs)))
    if(!country_included) ret[,country := NULL]
    return(ret)
  }
  if(rectangle)
  {
    lon_range = range(cs[,lon]) + c(-tol,tol)
    lat_range = range(cs[,lat]) + c(-tol,tol)

    if(!country_included) dt[,country := NULL]
    return(dt[lon %between% lon_range & lat %between% lat_range])
  }
}




#' restricts data to the Greater Horn of Africa
#'
#' Wraps \code{\link{restrict_to_country}}, and restricts to the GHA-region usually considered in CONFER, see \code{\link{EA_country_names}}.
#' @param dt the data table.
#' @param ... passed on to \code{\link{restrict_to_country}}
#'
#'@return the data table, restricted to the selected country
#'
#'@examples
#'ex_dt = chirps_monthly[lat < 0 & month == 11 & year == 2020]
#'dt = restrict_to_GHA(ex_dt)
#'
#' @export
#' @importFrom data.table as.data.table

restrict_to_GHA = function(dt,...)
{
  dt = restrict_to_country(dt,ct = EA_country_names(),...)
  return(dt)
}

#'@rdname restrict_to_GHA
restrict_to_confer_region = function(...)
{
  lifecycle::deprecate_warn('1.1.1','restrict_to_confer_region()','restrict_to_GHA')
  restrict_to_GHA(...)
}


#' Get tercile probability forecast from ensemble forecasts
#'
#' @description The function takes a data table containing ensemble predictions and reduces it to predicted tercile probabilities.
#' The data table should either have a column 'tercile_cat' or it will be generated in the process (by \code{\link{add_tercile_cat}}).
#' In particular, if you don't know the tercile category of the ensemble predictions, your data table should contain hindcasts as well,
#' such that the tercile categories are calculated correctly.
#' The probability for 'below', for example, is the fraction of ensemble members predicting below normal (for this coordinate).
#'
#' @param dt The data table.
#' @param by Names of columns to group by.
#' @param keep_cols A vector of column names that you want to keep. Column names in by are kept automatically.
#' @param ... passed on to \code{\link{add_tercile_probs}}.
#'
#'@return A new data table with tercile forecasts
#'
#'@examples
#'test_dt = ecmwf_monthly[lat < 0 & month == 11]
#'tfc = tfc_from_efc(test_dt)
#'
#'
#' @export
tfc_from_efc = function(dt, by = setdiff(dimvars(dt),'member'), keep_cols = NULL,...)
{
  keep_cols = unique(c(by, keep_cols, 'below','normal','above'))
  dt = add_tercile_probs(dt,by = by,...)
  dt = unique(dt[,.SD,.SDcols = keep_cols])
  return(dt)
}
