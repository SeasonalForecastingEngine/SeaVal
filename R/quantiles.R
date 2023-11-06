#' Calculates and saves the quantiles of CHIRPS data required for verification maps.
#'
#' @param clim_period which years should be considered for the quantiles.
#' @param version which version of CHIRPS, 'UCSB' or 'ICPAC'? Can be a vector with both.
#' @param resolution If this is set to 'high', the quantiles are also calculated for high-resolution CHIRPS data. This is not nicely implemented right now and will take a lot of memory and time.
#' @param CHIRPS_dir directory the CHIRPS data is stored in.
#' @param seasons Are we plotting for seasonal or monthly forecasts?
#'
#' @return data table with quantiles.
#'
#' @examples
#' \dontrun{chirps_ver_map_quantiles()}
#'
#' @export


chirps_ver_map_quantiles =function(clim_period = 1991:2020,
                                   version = 'UCSB',
                                   resolution = 'low',
                                   CHIRPS_dir = chirps_dir(),
                                   seasons = TRUE)
{
  prec = NULL
  if(resolution == 'high' & interactive())
  {
    check = readline(prompt = paste0('I am about to calculate quantiles on ',max(clim_period)-min(clim_period) + 1,' years of high-resolution CHIRPS data.\n
  This will take a long time. Do you want to proceed?[y/n]'))
    if(check!='y')stop()
  }

  # low resolution is always calculated:
  for(ver in version)
  {
    print(paste0('Calculating quantiles for low-resolution CHIRPS data from ',ver,'...'))

    dir = file.path(CHIRPS_dir,version,'upscaled')
    dir.create(file.path(dir,'quantiles'),showWarnings = FALSE)

    dt = load_chirps(years = clim_period,version = ver,resolution = 'low')
    quantiles = get_quantiles(dt,save_file = file.path(dir,'quantiles',paste0('ver_map_quantiles_',min(clim_period),'__',max(clim_period),'.RData')))

    if(seasons)
    {
      dt = load_chirps(years = clim_period,version = ver,resolution = 'low')
      season = function(month)
      {ret = rep('',length(month))
        ret[month %in% 3:5] = 'MAM'
        ret[month %in% 6:9] = 'JJAS'
        ret[month %in% 10:12] = 'OND'
        return(ret)
      }
      dt[,season := season(month)]
      dt = dt[season != '',.(prec = mean(prec)),.(year,season,lon,lat)]
      quantiles = get_quantiles(dt,save_file = file.path(dir,'quantiles',paste0('ver_map_quantiles_seasonal_',min(clim_period),'__',max(clim_period),'.RData')))
    }
  }

  if(resolution == 'high')
  {
    print(paste0('Calculating quantiles for high-resolution CHIRPS data from ',ver,'...'))
    dir = file.path(CHIRPS_dir,version)
    dir.create(file.path(dir,'quantiles'),showWarnings = FALSE)

    dt = load_chirps(years = clim_period,version = ver,resolution = 'high')
    quantiles = get_quantiles(dt,save_file = file.path(dir,'quantiles',paste0('ver_map_quantiles_',min(clim_period),'__',max(clim_period),'.RData')))
    if(seasons)
    {
      dt = load_chirps(years = clim_period,version = ver,resolution = 'high')
      season = function(month)
      {ret = rep('',length(month))
      ret[month %in% 3:5] = 'MAM'
      ret[month %in% 6:9] = 'JJAS'
      ret[month %in% 10:12] = 'OND'
      return(ret)
      }
      dt[,season := season(month)]
      dt = dt[season != '',.(prec = mean(prec)),.(year,season,lon,lat)]
      quantiles = get_quantiles(dt,save_file = file.path(dir,'quantiles',paste0('ver_map_quantiles_seasonal_',min(clim_period),'__',max(clim_period),'.RData')))
    }
  }
}




#' Calculate quantiles from a data table
#'
#' @description The quantiles are saved in/returned as a list with the following elements:
#' * dt - A data table with quantiles for each level of by (not the same as the input-dt).
#' * quantiles - the vector of quantiles that were used.
#' * group - a data table containing the levels the quantiles are grouped over, e.g. all years the quantiles are calculated over.
#' * data_col_name - the name of data_col, see below, so that you know what the quantiles actually were computed from.
#' * description - the description string, if provided.
#'
#'
#' @param dt Data table containing the data.
#' @param data_col The name of the column in dt containing the data for which the quantiles are derived. By default the first column that is not a dimension variable is selected.
#' @param qqs Vector of quantiles. If one of them is larger 1 they are interpreted as percent. Default is the quantiles used in the verification maps.
#' @param by Column names in dt. Levels by which the quantiles are calculated
#' @param description Optional description string.
#' @param save_file Optional name of save file.
#'
#' @return Nothing if save_file is provided. Otherwise the list described above
#'
#' @importFrom stats quantile
#'
#' @examples
#' \donttest{get_quantiles(chirps_monthly)}
#'
#' @export


get_quantiles = function(dt,
                         data_col = setdiff(names(dt),dimvars(dt))[1],
                         qqs = c(10,20,33,67,80,90),
                         by = setdiff(dimvars(dt),c('year','member')),
                         description = NULL,
                         save_file = NULL)
{
  N = V1 = NULL

  if(max(qqs) > 1) qqs = qqs/100

  # remove missing values before checking number of values per coordinate
  dt = dt[!is.na(get(data_col))]

  # check whether different levels of by have different numbers of datapoints.
  Ns = unique(dt[,.N,by = by][,N])
  if(min(Ns) == 1) stop('The data only has a single value for some coordinates. I cannot calculate a quantile from one value.')
  if(min(Ns) < 5) warning(paste0('For some coordinates the data only has ',min(Ns),' values. The calculated quantiles are very unreliable.'))
  if(length(Ns)>1)
  {
    warning(paste0('You have a different number of values for different coordinates.\n There are coordinates with ',paste(Ns,collapse = ', '),' values.'))
  }

  # 1st quantile:
  qq = qqs[1]
  dt_res = dt[,.(stats::quantile(get(data_col),qq,names = FALSE,na.rm = TRUE)),by = by]
  setnames(dt_res,'V1',paste0('q',qq))

  if(length(qqs)>1)
  {
    for(qq in qqs[2:length(qqs)])
    {
      dt_temp = dt[,.(stats::quantile(get(data_col),qq,names = FALSE,na.rm = TRUE)),by = by]
      dt_res[,paste0('q',qq):= dt_temp[,V1]]
    }
  }

  # this needs to be made smarter at some point:
  if('member' %in% names(dt))
  {
    grouping_levels = unique(dt[,.(year,member)])
  }else{
    grouping_levels = unique(dt[,.(year)])
  }


  res = list(dt = dt_res,
             quantiles = qqs,
             group = grouping_levels,
             data_col_name = data_col,
             description = description)
  if(is.null(save_file))
  {
    return(res)
  } else{save(res,file = save_file)}
}

#' get terciles from a data table
#'
#' @description This function wraps \code{\link{get_quantiles}} with the fixed quantiles 0.33 and 0.67.
#'
#' @param ... passed on to \code{\link{get_quantiles}}.
#'
#' @return See \code{\link{get_quantiles}}.
#'
#' @examples
#' \donttest{
#' # takes a few seconds:
#' get_terciles(chirps_monthly)
#' }
#'
#' @export

get_terciles = function(...)
{
  get_quantiles(qqs = c(0.33,0.67),...)
}
