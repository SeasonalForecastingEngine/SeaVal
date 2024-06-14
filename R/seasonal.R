
### Code for conversion to seasonal data:

#' Auxiliary function for decoding season-strings
#' @param seasons A character vector of season strings, see [convert_monthly_to_seasonal()] for details

season_strings_to_int = function(seasons = c("MAM", "JJAS", "OND")){
  seasons = toupper(seasons)
  full_string = "JFMAMJJASONDJFMAMJJASOND" # need it twice to cover wrapping over the end of the year
  indices = c(1:12,1:12)
  locations = stringr::str_locate(full_string,seasons)
  month_matrix = matrix(indices[locations],ncol = 2, byrow = FALSE)
  which_na = which(is.na(month_matrix[,1]))
  if(length(which_na) > 0) stop(paste0("The following strings are not recognized seasons: ",paste(seasons[which_na], collapse = ", ")))
  return(month_matrix)
}



#' Convert a data table from monthly to seasonal format
#'
#' Converts monthly to seasonal data. The function default values are set for precipitation. In particular, default behavior is to *sum*
#' values over all months contained in a season. If you want to average instead (for temperature, for example), you can change the aggregation function `FUN`.
#'
#' Note that it is impossible to derive seasonal tercile categories from monthly ones (and similar for seasonal tercile forecasts). For getting these, you should convert to seasonal
#' *before* deriving the tercile categories or forecasts, e.g. by using [add_tercile_cat()] or [tfc_from_efc()].
#'
#' @details
#' Seasons are provided as sequences of capitalized initial characters of the months they contain, e.g. `'MAM'` for March-April-May.
#' They can have any length from 1 to 12 months and are allowed to overlap and wrap over the end of the year
#' (for example, you can provide `seasons = c('OND', 'NDJ')` to derive values for October-December and November-January).
#' If a season includes months from 2 years, it gets assigned the year of its starting month. For example, `season = 'NDJ'` and `year = 2000` refers to values for the season November 2000 to January 2001.
#'
#' Factor- or Character-valued columns cannot be aggregated to seasonal values. If `vars` contains columns that are factor- or character-valued, it checks whether they take a unique value for each grouping level
#' provided in `by`. If yes, they are kept, else they are discarded. A typical case where this is useful is when your data table contains country names (see [add_country()]).
#' The grouping levels usually include `'lon'`, `'lat'`, so there is only one country name per grouping level and the name is kept.
#'
#' @param dt A data table containing the values for conversion.
#' @param vars Character vector of names of the columns containing the values for conversion. Default is to try converting everything that is not contained in `by` and that is not recognized as tercile category (see [tc_cols()]) or
#' tercile forecast (`'below'`, `'normal'`, `'above'`).
#' @param by Character vector of column names to group by. Separate values are derived for each unique combination of values in `by`.
#' Defaults to all [dimvars()] that are not `'month'`, which is usually what you want.
#' @param FUN function for aggregation.
#' @param ... arguments passed to `FUN`, for example `na.rm`.
#' @param seasons Vector of character strings specifying seasons. See details. Defaults to `c('MAM', 'JJAS', 'OND')`, which are the seasons considered in the COFs for the Greater Horn of Africa.
#' @param only_complete_seasons Logical. If `TRUE`, only values are kept for which we have data for all months. For example, no `OND` values would be derived if the data for December is missing.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # returns empty data table, because the example data does not contain data for a full season:
#' dt = convert_monthly_to_seasonal(chirps_monthly)
#'
#' # remove terc_cat first to avoid the warning,
#' # and set season to the months we actually have data for:
#' dt2 = convert_monthly_to_seasonal(copy(chirps_monthly)[,terc_cat := NULL], seasons = c('ND'))
#' print(dt2)
#'
#' # season OND, get monthly averages rather than sums, and force the function to derive values
#' # even though we do not have October-data:
#' dt3 = convert_monthly_to_seasonal(chirps_monthly,
#'                                   seasons = c('OND'),
#'                                   FUN = mean,
#'                                   only_complete_seasons = FALSE)
#' print(dt3)
#' }

convert_monthly_to_seasonal = function(dt,
                                       vars = NULL,
                                       by = NULL,
                                       FUN = sum,
                                       ...,
                                       seasons = c("MAM", "JJAS", "OND"),
                                       only_complete_seasons = TRUE)
{
  #season_present = 'season' %in% names(dt)

  if(!('month' %in% names(dt)) & ('season' %in% names(dt))){
    message('The data table seems to be in seasonal format already.')
    return(dt)
  }

  if(!('month' %in% names(dt)) & !('season' %in% names(dt))) stop('Your data needs to contain a column named "month".')


  if(is.null(by)) by = setdiff(dimvars(dt), c('month',lt_cols()))
  if(is.null(vars)) vars = setdiff(names(dt),c(by,'month',lt_cols()))


  # checks:
  if('above' %in% vars)
  {
    vars = setdiff(vars, c('below','normal','above'))

    warning(paste('The data contains tercile probabilities.',
                  'I cannot derive seasonal tercile probabilities from monthly ones.',
                  'If the tercile probabilities were derived from an ensemble forecast,',
                  'please convert the forecasts to seasonal BEFORE extracting tercile probabilities.',
                  'I remove "below", "normal", and "above" from the columns that are converted.',sep = '\n'))
  }
  if(any(tc_cols() %in% vars))
  {
    vars = setdiff(vars, tc_cols())
    warning(paste('The data contains tercile categories.',
                  'I cannot derive seasonal tercile categories from monthly ones.',
                  'Please derive tercile categories AFTER converting to seasonal values.',
                  sep = '\n'))
  }
  if(any(!vars %in% names(dt))){
    warning('Not all provided vars are columns in dt.')
    vars = intersect(vars, names(dt))
  }


  # Subset vars to columns that contain numeric values.
  is_numLike = c()
  for(i in seq_along(vars))
  {
    is_numLike = c(is_numLike, (is.numeric(dt[,get(vars[i])])| is.logical(dt[,get(vars[i])])))
  }
  if(sum(is_numLike) < length(is_numLike)) # at some point, we might want to get
  {
    nnl_cols = vars[!is_numLike]
    # check whether these columns contain the same value for each level in by, then we can just carry them along.
    # Technically we could even take them along if they have the same value for each level in by and each season (which is weaker)
    # but that's more work to implement and likely not required.

    check = identical(unique(dt[,.SD,.SDcols = by]),
                      unique(dt[,.SD,.SDcols = c(by,nnl_cols)])[,.SD,.SDcols = by])

    if(check) by = c(by, nnl_cols)
    if(!check){
      warning(paste0("The columns ",paste(nnl_cols,collapse = ", "),' are non-numeric and cannot be converted to seasonal format.'))
    }
  }
  vars = vars[is_numLike]

  if(length(vars) == 0) stop("No variables found that could be converted.")

  ### Warn if predictions are detected ###
  # Predictions are more messy because of the leadtime-dimension. This is currently not handled by the function
  # Scenario 1: dt contains forecasts but not at several leadtimes
  cns_indicating_forecasts = c('fc','forecast','member','pred','prediction')
  if(any(cns_indicating_forecasts %in% names(dt)) | (any(lt_cols() %in% names(dt)))){
    warning(paste(paste0('Your data table contains columns named ',
                         paste(c(intersect(cns_indicating_forecasts, names(dt)),
                                 intersect(lt_cols(), names(dt))), collapse = ', '),'.'),
                  'This seems to indicate that it contains forecasts, and I assume they are valid for the month given by the columns "year" and "month".',
                  'Note that for seasonal forecasts we want different lead times for different months:',
                  'For example, a prediction initialized in February for the March-April-May-season requires forecasts with leadtime 1 month for March,',
                  '2 months for April, and 3 months for May. ' ,sep = '\n'))
  }
  if(any(lt_cols() %in% names(dt))){
    ltcol = which(lt_cols() %in% names(dt))
    test = unique(dt[,.SD,.SDcols = c('month',ltcol)])
    multiple_leadtimes_per_month = !identical(sort(test[, month]), sort(unique(dt[,month])))
    if(multiple_leadtimes_per_month){
      stop(paste('Your data table contains forecasts for the same month at several lead times.',
                 'This is not supported by the convert_monthly_to_seasonal function, which currently does not find the right leadtimes for you.', sep = '\n'))
    }
  }

  season_ints = season_strings_to_int(seasons)
  return_data = list()
  for(i in seq_along(seasons)){
    lims = season_ints[i,]
    # get months of the season:
    if(lims[1] <= lims[2]) {
      months = lims[1]:lims[2]
      which_months_shift_year = integer(0)
    }
    # the case where the season wraps around the end of the year:
    if(lims[2] < lims[1]) {
      lims[2] = lims[2] + 12
      months = lims[1]:lims[2]
      which_months_shift_year = months[months > 12] - 12 # year is always the year of the first month contained in the season
      months[months > 12] = months[months > 12] - 12
    }

    if(only_complete_seasons)
    {
      dt_temp = dt[month %in% months][, N := .N, by = by]
      dt_temp[month %in% which_months_shift_year,  year := year - 1]
      dt_new = dt_temp[N == length(months), lapply(.SD,FUN,...), .SDcols = vars, by = by] # this is weirdly ordered but whatever... Why is this taking so freaking long?
      dt_new[,season := seasons[i]]
      return_data[[i]] = dt_new
    } else {
      dt_temp = dt[month %in% months]
      dt_temp[month %in% which_months_shift_year,  year := year - 1]
      dt_new = dt_temp[,lapply(.SD,FUN,...), .SDcols = vars, by = by] # this is weirdly ordered but whatever... Why is this taking so freaking long?
      dt_new[,season := seasons[i]]
      return_data[[i]] = dt_new
    }
  }
  return_data = rbindlist(return_data)

  message(paste0('The data in the following columns is aggregated:\n',
                 paste(vars,collapse = ', ')))
  return(return_data)
}


