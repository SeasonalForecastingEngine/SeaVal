
### Start working on code for conversion to seasonal data:


#' Adjust Lead Time for Seasonal Forecasts
#'
#' This function adjusts the lead time values from monthly data for the seasonal format.
#'
#' @param lead_time A numeric vector of lead time values.
#' @param month A numeric vector of month values.
#'
#' @details
#' The function adjusts the lead time values by subtracting the difference between the month value and the corresponding starting month of the season.
#' The adjustment is performed for the months 3-5 (March to May), 6-9 (June to September), and 10-12 (October to December).
#' For example, if the month is March (3), the lead time is adjusted by subtracting (3 - 3) = 0.
#' This adjustment aligns the lead time values with the seasonal forecasts.
#'
#'
#' @return A numeric vector of lead time values adjusted for seasonal forecasts.
#' @export

lead_time_conversion = function(lead_time,month)
{
  message('lead times are adjusted for seasonal forecasts.')
  lead_time[month %in% 3:5]  = lead_time[month %in% 3:5] - (month[month %in% 3:5] - 3)
  lead_time[month %in% 6:9]  = lead_time[month %in% 6:9] - (month[month %in% 6:9] - 6)
  lead_time[month %in% 10:12]  = lead_time[month %in% 10:12] - (month[month %in% 10:12] - 10)
  return(lead_time)
}



# convert to seasonal:
# takes a vector of months and returns a vector of corresponding seasons
#' Getting seasons from season
#'
#' @description takes a vector of months and returns a same-length vector of seasons
#'
#' @param mm vector of months
#'
#' @export
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

#' Convert Data to Seasonal Format
#'
#' This function converts data to seasonal format by aggregating monthly values.
#'
#' @param dt The input data table.
#' @param vars A character vector specifying the columns to be converted (optional).
#' @param by A character vector specifying the grouping variables (optional).
#' @param FUN The function to be used for aggregation (default is \code{sum}).
#' @param subset logical. If TRUE (default), then we expect three entries per level of by in MAM and OND, and 4 levels in JJAS.
#' The data is subset to all levels of by satisfying this condition. This is important if your data may be incomplete for some
#' levels. For example, if you have monthly precip data and it ends in October 2023, then the OND-2023 precip entries would just be the October values,
#' because the function simply sums over all OND 2023 values it finds. This is usually not what you want, you'd prefer OND 2023 to be suppressed, which
#' is the functions behavior when subset = TRUE.
#'
#' @details
#' The function checks if the input data table is already in seasonal format.
#' If not, it requires the data table to contain a column named "month" for conversion.
#' The function then aggregates the data by season based on the month column and the specified grouping variables.
#' The specified columns are reduced to those that are numeric-like (numeric or logical).
#' The function returns a new data table with the aggregated values.
#'
#'
#' @return A data table in seasonal format with aggregated values.
#' @export

seasonal = function(dt,vars = NULL,by = NULL,FUN = sum, subset = TRUE)
{
  season_present = 'season' %in% names(dt)

  if(!('month' %in% names(dt)) & season_present){
    message('The data table seems to be in seasonal format already.')
    return(dt)
  }

  if(!('month' %in% names(dt)) & !season_present) stop('For converting to seasonal format, your data needs to contain the column "month"')



  if(!season_present) dt[,season := season(month)]

  if(is.null(by)) by = setdiff(dimvars(dt),'month')
  if(is.null(vars)) vars = setdiff(names(dt),c(by,'month'))


  # checks:
  if('above' %in% vars)
  {
    stop(paste0('The data contains tercile probabilities.\n',
                'I cannot derive seasonal tercile probabilities from monthly ones.\n',
                'Please convert your forecasts to seasonal format BEFORE getting the tercile probabilities.'))
  }

  # reduce to vars that are numLike:
  is_numLike = c()
  for(i in seq_along(vars))
  {
    is_numLike = c(is_numLike, (is.numeric(dt[,get(vars[i])])| is.logical(dt[,get(vars[i])])))
  }
  if(sum(is_numLike) < length(is_numLike)) # at some point, we might want to get
  {
    message(paste0('The following columns could not be converted:',
                   paste(vars[which(!is_numLike)]),collapse = ', '))
    vars = vars[-which(!is_numLike)]
  }

  if(length(vars) == 0) stop('Nothing to convert.')

  # fix lead times for seasonal data:
  lt = ('lead_time' %in% names(dt))
  if(lt) {
    dt[,lead_time_new := lead_time_conversion(lead_time,month)]
    if('lead_time' %in% by)
    {
      by = c(setdiff(by,'lead_time'),'lead_time_new')
    }
  }

  if(subset)
  {
    dt[,N := .N, by = by]
    check_n = function(season) return(3 + 1*(season == 'JJAS'))
    dt_new = dt[!is.na(season)][N == check_n(season),lapply(.SD,FUN,na.rm = TRUE),.SDcols = vars,by = by] # this is weirdly ordered but whatever... Why is this taking so freaking long?
    dt[,N:= NULL]
  } else {
    dt_new = dt[!is.na(season)][,lapply(.SD,FUN,na.rm = TRUE),.SDcols = vars,by = by] # this is weirdly ordered but whatever... Why is this taking so freaking long?
  }

  if(!season_present) dt[,season := NULL]
  if(lt)
    {
    setnames(dt_new,'lead_time_new','lead_time')
    dt[,lead_time_new := NULL]
    }

  message(paste0('The data in the following columns is aggregated:\n',
                 paste(vars,collapse = ', ')))
  return(dt_new)
}
