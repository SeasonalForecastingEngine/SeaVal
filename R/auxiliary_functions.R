
#### This file contains all kind of auxiliary functions. ####

#' Auxiliary function
#'
#' returns the default column names to group by when calculating scores of
#' ensemble forecasts.
#'
#' @param dt optional. You can provide a data table, then the function
#' returns the names of grouping variables in this data table.
#' @return A vector of characters with the column names.

by_cols_ens_fc_score = function(dt = NULL)
{
  return(setdiff(dimvars(dt),c('year','member')))
}

#' Auxiliary function
#'
#' returns the default column names to group by when calculating scores for
#' tercile forecasts.
#'
#' @param dt optional. You can provide a data table, then the function returns
#' the names of grouping variables in this data table.
#' @return A vector of characters with the column names.

by_cols_terc_fc_score = function(dt = NULL)
{
  return(setdiff(dimvars(dt),c('year','member')))
}

#' Auxiliary function
#'
#' Gets column names to group by when calculating scores for tercile forecasts.
#' Some tercile forecasts, such as ROC score or SRC (slope of reliability curve)
#' require many data points and should therefore be pooled in space.
#' This auxiliary function returns the default column names to group by for these scores.
#' The suffix _sp stands for spatial pooling.
#'
#' @param dt optional. You can provide a data table, then the function returns the names
#' of grouping variables in this data table.
#' @return A vector of characters with the column names.

by_cols_terc_fc_score_sp = function(dt = NULL)
{
  return(setdiff(by_cols_terc_fc_score(dt),c('lon','lat')))
}

#' Auxiliary function for scores of ensemble forecasts.
#'
#' Checks whether the data table contains columns with names that are not allowed,
#' or whether it is missing columns that are required.

checks_ens_fc_score = function()
{
  # inherit arguments passed to parent:
  dt = parent.frame(1)$dt
  f = parent.frame(1)$f
  o = parent.frame(1)$o
  pool = parent.frame(1)$pool
  by = parent.frame(1)$by
  mem = parent.frame(1)$mem
  dim.check = parent.frame(1)$dim.check

  if(dim.check) run_dimension_check_ens_fc_score()

  if(length(intersect(c('f','o','by','pool','mem'),names(dt))) > 0)
  {
    stop('For technical reasons your data table should not contain any column named "f", "o", "by", "pool", or "mem".')
  }
  for(cn in c(f,o))
  {
    if(!(cn %in% names(dt)))
    {
      stop(paste0('The data table does not contain a column named ',cn,'.'))
    }
  }
}

#' Auxiliary function for scores for tercile forecasts.
#'
#' Checks whether the data table contains columns with names that are not allowed,
#' or whether it is missing columns that are required.

checks_terc_fc_score = function()
{
  # inherit arguments passed to parent:
  f = parent.frame(1)$f
  o = parent.frame(1)$o
  dt = parent.frame(1)$dt
  by = parent.frame(1)$by
  pool = parent.frame(1)$pool
  dim.check = parent.frame(1)$dim.check

  if(dim.check) run_dimension_check_terc_forecast()

  if(length(intersect(c('f','o','by','pool'),names(dt))) > 0)
  {
    stop('For technical reasons your data table should not contain any column named "f", "o", "by", or "pool".')
  }
  for(cn in c(f,o))
  {
    if(!(cn %in% names(dt)))
    {
      stop(paste0('The data table does not contain a column named ',cn,'.'))
    }
  }
}

#' Get dimension variables
#'
#' @description The function returns all names currently considered dimension variables.
#' Following the logic of netcdfs, data tables usually have columns specifying coordinates
#' (or dimvars) and other columns containing data for these dimvars. Dimension variables can be spatial
#' or temporal coordinates, or the lead time of a forecast or the member in an ensemble forecast, etc...
#'
#'@param dt Optional data table. If a data table is provided only the dimvars of the data table are returned.
#'@return A vector of characters with the column names considered dimvars.
#'
#'@examples
#'dimvars()
#'
#'@export

dimvars = function(dt = NULL)
{
  options = c(obs_dimvars(),'lead_time','member','nwp','system')
  if(is.null(dt))
  {
    return(sort(options))
  } else {
    return(sort(intersect(options,names(dt))))
  }
}


#' Auxiliary function to access and change the directory used to load and save data.
#'
#' @description The package allows to download and organize CHIRPS data. This function specifies the directory where
#' the data is stored. The first time this function is called, it asks the user to configure the directory.
#'
#' @param set_dir logical. Set this to TRUE if you have to redefine your data directory.
#' @return The current data directory as string.
#'
#' @examples
#' if(interactive()){
#' data_dir()
#'}
#' @export

data_dir = function(set_dir = FALSE)
{
  if(file.exists('~/.config_SeaVal'))
  {
    dir = suppressWarnings(readLines(con = '~/.config_SeaVal'))
  }

  if(set_dir & file.exists('~/.config_SeaVal'))
  {
    if(!interactive()) stop('Setting a new data directory is only allowed in an interactive R session.')

    mm = menu(choices = c('yes','no'),
              title = paste0('Your current data directory is:\n',dir,'\nDo you want to change it?'))
    if(mm == 1)
    {
      file.remove('~/.config_SeaVal')
    }
  }

  if(!file.exists('~/.config_SeaVal'))
  {
    if(!interactive()) stop('Your data directory does not yet seem to be configured. Please call data_dir() in an interactive session first.')

    m1 = menu(choices = c('yes','no'),
              title = 'Please provide a directory for saving data.\nDo you use the package from inside the Norwegian Computing Center?')
    if(m1 == 1)
    {
      if(Sys.info()["sysname"] == 'Windows')
      {
        dir = 'M:\\CONFER\\Data\\'
      } else {
        dir = '/nr/project/stat/CONFER/Data/'
      }
      if(!dir.exists(dir))
      {
        stop(paste0('Working at NR, the data directory should be\n',dir,'\nBut that directory does not exist in your current setup.\nPlease rerun and specify your data directory manually.'))
      }
      cat(dir,file = '~/.config_SeaVal')
      message(paste0('Setup complete. Your data directory is:\n',dir))
    }
    if(m1 == 2)
    {
      message('Please type your data directory. \n
Use / on Linux (e.g. /nr/project/stat/CONFER/Data/) and \\ on Windows (e.g. C:\\Users\\Documents\\).')
      rl3 = readline('Input:')

      # check whether user has wrapped with ' or " and remove:
      if(substr(rl3,nchar(rl3),nchar(rl3)) %in% c("'",'"') & substr(rl3,1,1) %in% c("'",'"'))
      {
        rl3 = substr(rl3,2,nchar(rl3)-1)
      }
      # check whether a \ or / was put at the end, else add it:
      if(Sys.info()["sysname"] == 'Windows')
      {
        if(!substr(rl3,nchar(rl3),nchar(rl3)) == '\\')
        {rl3 = paste0(rl3,'\\')}
      }
      if(Sys.info()["sysname"] != 'Windows')
      {
        if(!substr(rl3,nchar(rl3),nchar(rl3)) == '/')
        {rl3 = paste0(rl3,'/')}
      }
      cat(paste0(rl3,'\n'),file = '~/.config_SeaVal')
    }

    dir = suppressWarnings(readLines(con = '~/.config_SeaVal'))

    if(!dir.exists(dir))
    {
      m3 = menu(choices = c('yes','abort'), title = "The data directory does not yet exist, do you want to create it?")
      if(m3 == 1)
      {
        dir.create(dir,showWarnings = FALSE,recursive = T)
      }
      if(m3 == 2)
      {
        stop('Aborted.')
      }
      message(paste0('Setup complete. Your data directory is:\n',dir,'\nIf you need to change your data directory, run data_dir(set_dir = TRUE)'))
    }
  }
  dir = suppressWarnings(readLines(con = '~/.config_SeaVal'))
  return(dir)
}

#' Get names of countries in east Africa
#'
#' This is an auxiliary function used in \code{\link{add_country_names}}, so only these names are recognized
#' by default.
#'
#' @return A character-vector of country names.
#'
#' @examples
#' EA_country_names()
#' @export

EA_country_names = function()
{ return(
  c('Burundi','Djibouti', 'Eritrea','Ethiopia',
    'Kenya','Rwanda','Somalia','Somaliland',
    'South Sudan','Sudan','Tanzania','Uganda'))
}

#' Forecast column names
#'
#' returns the columns names that are recognized as (ensemble-) forecast values
#'
#' @param dt optional data table. If provided, the function guesses which column contains the forecast values. Else it returns all recognized forecast column names.
#'
#' @return Character vector with column names.
#'
#' @examples
#' fc_cols()
#'
#' @export

fc_cols = function(dt = NULL)
{
  cols = c('fc','forecast','pred','prediciton','prec','precipitation')
  if(!is.null(dt))
  {
    cols = intersect(cols,names(dt))[1]
  }
  if(length(cols) == 0) stop("I don't recognize which column contains the forecast.")
  return(cols)
}

#' Observation column names
#'
#' @description Note that this function guesses column names for observed precip, not observed tercile category.
#'
#' @param dt optional data table. If provided, the function guesses which column contains the observations. Else it returns all recognized observation column names.
#'
#' @return Character vector with column names.
#'
#' @examples
#' obs_cols()
#'
#' @export

obs_cols = function(dt = NULL)
{
  cols = c('obs','observation','prec','precipitation')
  if(!is.null(dt))
  {
    cols = intersect(cols,names(dt))[1]
  }
  if(length(cols) == 0) stop("I don't recognize which column contains the observations.")
  return(cols)
}

#' Tercile column names
#'
#' which column names are interpreted as observed tercile categories
#'
#' @param dt optional data table. If provided, the function guesses which column contains the observations. Else it returns all recognized column names.
#'
#' @return Character vector with column names.
#'
#' @examples
#' tc_cols()
#'
#' @export

tc_cols = function(dt = NULL)
{
  cols = c('tercile_cat','tercile_category','terc_cat','tc_cat','tc_category')
  if(!is.null(dt))
  {
    cols = intersect(cols,names(dt))[1]
  }
  if(length(cols) == 0) stop("I don't recognize which column contains the observed tercile category.")
  return(cols)
}



#' Auxiliary function returning observation dimvars.
#'
#' Observation dimvars are column names in a data table that resemble coordinates for which only one observation may exist.
#'
#' @param dt optional. You can provide a data table, then the function returns the names of coordinate columns in this data table.
#'
#' @return Character vector with column names.
#'
#' @examples
#' obs_dimvars
#'
#' @export
obs_dimvars = function(dt = NULL)
{
  options = sort(c(space_dimvars(),time_dimvars()))
  if(is.null(dt))
  {
    return(options)
  } else {
    return(intersect(options,names(dt)))
  }
}


#' Auxiliary Function
#'
#' called inside functions that calculate scores
#' for ensemble forecasts. Checks whether the provided data table has the right format.

run_dimension_check_ens_fc_score = function()
{
  # inherit arguments passed to parent:
  dt = parent.frame(1)$dt
  by = parent.frame(1)$by
  pool = parent.frame(1)$pool
  mem = parent.frame(1)$mem

  cc = setdiff(dimvars(dt),c(by,pool,mem))

  colnames_check = intersect(names(dt),c(by,pool,mem))
  if(length(colnames_check) > 0)
  {
    check = unique(dt[,.SD,.SDcols = colnames_check])[,.N] == dt[,.N]
  } else
  {
    if(dt[,.N] == 1)
    {
      check = TRUE
    }else{stop(call. = FALSE,'Dimension check failed.\nNone of the columns provided in by or pool or member seems to be contained in the data table?')}
  }

  if(!check & length(cc) == 0) stop(call. = FALSE,'Dimension check failed.\nProbably you have multiple layers per ensemble member and year after grouping.
\nMaybe you forgot to include a grouping variable in by or a column to average over in pool?')
  if(!check & length(cc) > 0) stop(call. = FALSE,paste(c('Dimension check failed.\nProbably you have multiple layers per coordinate\n.
The following columns are classified as coordinate but were not included in by, pool, or mem:\n',cc)))
  if(length(cc)>0 & check) warning(call. = FALSE,paste(c('The following columns are classified as dimension variable but were not included in by, pool, or mem:\n',cc)))
  bypool = intersect(by,pool)
  if(length(bypool)>0 ) warning(call. = FALSE,paste(c('The following columns are contained in both by and pool:\n\n',bypool,'\n\nThey are interpreted as grouping variables, i.e., as part of "by".')))
}

#' Auxiliary Function
#'
#' called inside functions that calculate scores
#' for ensemble forecasts. Checks whether the provided data table has the right format.

run_dimension_check_terc_forecast = function()
{
  # inherit arguments passed to parent:
  dt = parent.frame(1)$dt
  by = parent.frame(1)$by
  pool = parent.frame(1)$pool

  cc = setdiff(dimvars(dt),c(by,pool))

  if('member' %in%  names(dt))
  {
    m1 = menu(choices = c('abort','by','pool'),
              title = "Your data contains a column named 'member' which is unusual for tercile forecasts.
\nIf your data table contains multiple members, but the same tercile-probabilities for each member, please reduce your data and delete the member column
(e.g. dt = dt[member == 1][,member := NULL]).
\nOtherwise, you need to select whether to include member in either 'by' or 'pool', see function documentation.")
    if(m1 == 1)
      {
      #stop without error:
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }
    if(m1 == 2)
    {
      by = c(by,'member')
      assign('by',by,pos = parent.frame(1))
    }
    if(m1 == 3)
    {
      pool = c(pool,'member')
      assign('pool',pool,pos = parent.frame(1))
    }
  }

  colnames_check = intersect(names(dt),c(by,pool))
  if(length(colnames_check) > 0)
  {
  check = unique(dt[,.SD,.SDcols = colnames_check])[,.N] == dt[,.N]
  } else
    {
      if(dt[,.N] == 1)
      {
        check = TRUE
      }else{stop(call. = FALSE,'Dimension check failed.\nNone of the columns provided in by or pool seems to be contained in the data table?')}
    }

  if(!check & length(cc) == 0) stop(call. = FALSE,'Dimension check failed.\nProbably you have multiple layers per ensemble member and year
                  after grouping.\nMaybe you forgot to include a grouping variable in by or a column to average over in pool?')
  if(!check & length(cc) > 0) stop(call. = FALSE, paste(c('Dimension check failed.\nProbably you have multiple layers per coordinate\n.
                                   The following columns are classified as coordinate but were not included in by, pool, or mem:\n',cc)))
  if(length(cc)>0 & check) warning(call. = FALSE, paste(c('The following columns are classified as dimension variable but were not included in by, pool, or mem:\n',cc)))

  bypool = intersect(by,pool)
  if(length(bypool)>0 ) warning(call. = FALSE, paste(c('The following columns are contained in both by and pool:\n\n',bypool,'\n\nThey are interpreted as grouping variables, i.e., as part of "by"')))
}


#' Auxiliary function
#'
#' returns all column names indicating a spatial coordinate.
#' @param dt optional. You can provide a data table, then the function returns the names of spatial coordinate columns in this data table.
#'
#' @return Character vector with column names.
#'
#' @examples
#' space_dimvars()
#' @export
space_dimvars = function(dt = NULL)
{
  options = c('lat','Lat','lon','Lon','X','Y')
  if(is.null(dt))
  {
    return(options)
  } else {
    return(intersect(options,names(dt)))
  }
}

#' Auxiliary function
#'
#' returns all column names indicating a temporal coordinate.
#' @param dt optional. You can provide a data table, then the function returns the names of temporal coordinate columns in this data table.
#'
#' @return Character vector with column names.
#'
#' @examples
#' time_dimvars()
#'
#' @export
time_dimvars = function(dt = NULL)
{
  options = c('month','season','T','time','year')
  if(is.null(dt))
  {
    return(options)
  } else {
    return(intersect(options,names(dt)))
  }
}

#' @importFrom utils globalVariables
# For including all possible dimension variables in the devtools::check() run:
utils::globalVariables(dimvars())
# while we're at it, let's include the following data.table syntax quirks:
utils::globalVariables(c('.'))
utils::globalVariables(c('countries'))
