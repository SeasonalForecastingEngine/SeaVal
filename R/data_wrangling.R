
#' function for loading cross-validation data (predictions and observations).
#'
#' @param data_dir path to where the cross-validation is stored.
#'
#' @return A data table.
#'
#' @importFrom  ncdf4 nc_open
#' @import data.table
#' @import ForecastTools
#'
#' @examples {
#' fn = '/nr/user/claudio/bigdisk/SFE/ERA_monthly_nc/total_precipitation_era_1979_1.nc'
#' ncdf_to_dt(fn, subset_list = list('latitude' = -50:50)) # will read out everything between lat -50 and lat 50 (not only the latitudes matching -50:50)
#' }
#'
#' @author Claudio
#'
#' @export

cv_to_dt = function(data_dir)
{

  # find cross-validation file
  fns = list.files(data_dir)
  fn = fns[grepl('CrossVal',fns)]

  spec = strsplit(fn,split = '-')[[1]][2]
  #get rid of year number:

  target_months = strsplit(gsub("[[:digit:]]","",spec), split = '_')[[1]][1]

  #' takes a character string consisting of capital letters of successive months.
  #' Returns the indices of these months
  months_to_numbers = function(str)
  {
    nmon = nchar(str)

    allmons = 'JFMAMJJASOND'
    # in case you go over 12:
    allmons = paste0(allmons,allmons)


    for(i in 1:12)
    {
      if(identical(substr(allmons,i,i+nmon-1),str))
      {
        mons = i:(i+nmon - 1)
      }
    }

    mons = mons%%12
    mons[mons == 0] = 12
    return(mons)
  }

  convert_MonthsSinceDate_to_YearMonth = function(dt,timecol = 'time',origin = '1981-01-01')
  {
    or_mon = month(as.Date(origin))
    or_year = year(as.Date(origin))

    mons = (dt[,get(timecol)] + or_mon ) %% 12
    mons[mons == 0] = 12
    # in case the middle of the month is supplied (i.e. non-integer times):
    mons = floor(mons)

    years = (dt[,get(timecol)] + or_mon ) %/% 12 + or_year
    years[mons == 12] = years[mons == 12] - 1

    dt[,c('year','month') := list(years,mons)]
    dt[,(timecol) := NULL]

    return(dt)
  }

  target_months = months_to_numbers(target_months)

  ######### now get cross-validation data ############

  nc_cv = ncdf4::nc_open(paste0(data_dir,fn))

  cv_dt = ncdf_to_dt(nc_cv,printunits = F)

  cv_dt = cv_dt[!is.na(prec)]
  cv_dt = convert_MonthsSinceDate_to_YearMonth(cv_dt)
  #mean of FMA forecast:
  cv_dt = cv_dt[month %in% target_months,.(prec = mean(prec)),by = .(lon,lat,year)]


  obs_fn = paste0('ObservedRainfall-',spec)

  nc_obs = ncdf4::nc_open(paste0(data_dir,obs_fn))

  obs_dt = ncdf_to_dt(nc_obs,printunits = F)

  obs_dt = obs_dt[!is.na(prec)]
  obs_dt = convert_MonthsSinceDate_to_YearMonth(obs_dt)
  #mean of FMA obs:
  obs_dt = obs_dt[month %in% target_months,.(prec = mean(prec)),by = .(lon,lat,year)]


  setnames(obs_dt,'prec','obs')
  cv_dt = merge(cv_dt,obs_dt[,.(lon,lat,year,obs)],by = c('lon','lat','year'))
  return(cv_dt)
}
