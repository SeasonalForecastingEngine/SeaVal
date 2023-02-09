
### Start working on code for conversion to seasonal data:


# convert to seasonal:
# takes a vector of months and returns a vector of corresponding seasons

season = function(mm)
{
  ret = rep(NA_character_,length(mm))
  ret[mm %in% 3:5] = 'MAM'
  ret[mm %in% 6:9] = 'JJAS'
  ret[mm %in% 10:12] = 'OND'

  ret = factor(ret,levels = c('MAM','JJAS','OND')) # for correct ordering
  return(ret)
}


#' Getting months from season
#'
#' @description takes a vector of up to three seasons and returns the months in them
#'
#' @param ss vector containing one or multiple season names ('MAM', 'JJAS', 'OND')
#'
#' @export
season_months = function(ss)
{
  ss = unique(as.character(ss))
  ret = NULL
  for (s in ss)
  {
    if(s == 'MAM') ret = c(ret,3:5)
    if(s == 'JJAS') ret = c(ret,6:9)
    if(s == 'OND') ret = c(ret,10:12)
  }

  return(ret)
}



convert_to_seasonal = function(dt)
{
  if(!('month' %in% names(dt))) stop('For converting to seasonal format, your data needs to contain the column "month"')

  season_present = 'season' %in% names(dt)

  if(!season_present) dt[,season := season(month)]

  dv = setdiff(dimvars(dt),'month')

  value_vars = setdiff(names(dt),dimvars(dt))
  if('above' %in% value_vars)
  {
    warning(paste0('The data contains tercile probabilities.\n',
                   'I cannot derive seasonal tercile probabilities from monthly ones.\n',
                   'Please convert your forecasts to seasonal format BEFORE getting the tercile probabilities.'))
  }
  if(length(value_vars) == 0) stop('Nothing to convert.')

  dt_new = dt[!is.na(season),lapply(.SD,mean,na.rm = TRUE),.SDcols = value_vars,by = dv] # this is weirdly ordered but whatever...

  if(!season_present) dt[,season := NULL]

  message(paste0('The data in the following columns was averaged over seasons:\n',
                 paste(value_vars,collapse = ', ')))
  return(dt_new)
}
