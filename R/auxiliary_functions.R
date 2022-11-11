
#### This file contains all kind of auxiliary functions. ####

#' Auxiliary function returning the default column names to group by when calculating scores of ensemble forecasts.
#'
#' @param dt optional. You can provide a data table, then the function returns the names of grouping variables in this data table.
#' @export
by_cols_ens_fc_score = function(dt = NULL)
{
  return(setdiff(dimvars(dt),c('year','member')))
}

#' Auxiliary function returning the default column names to group by when calculating scores for tercile forecasts.
#'
#' @param dt optional. You can provide a data table, then the function returns the names of grouping variables in this data table.
#' @export
by_cols_terc_fc_score = function(dt = NULL)
{
  return(setdiff(dimvars(dt),c('year','member')))
}

#' Some tercile forecasts, such as ROC score or SRC (slope of reliability curve) require many data points and should therefore be pooled in space
#' This auxiliary function returns the default column names to group by for these scores. The suffix _sp stands for spatial pooling.
#'
#' @param dt optional. You can provide a data table, then the function returns the names of grouping variables in this data table.
#' @export
by_cols_terc_fc_score_sp = function(dt = NULL)
{
  return(setdiff(by_cols_terc_fc_score(dt),c('lon','lat')))
}



#' Auxiliary function for scores of ensemble forecasts. Checks whether the data table contains columns with names that are not allowed,
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

#' Auxiliary function for scores for tercile forecasts. Checks whether the data table contains columns with names that are not allowed,
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

#'Return the default column names that are considered to be coordinates rather than values:
#'@param dt Optional data table. If a data table is provided only the dimvars of the data table are returned.
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


#' Auxiliary function to access the directory used to load and save data. The first time this is called it asks the user to configure it.
#' @param set_dir logical. Set this to TRUE if you have to redefine your data directory.
#' @export

data_dir = function(set_dir = F)
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
      cat('Please type your data directory. \n
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

#' Auxiliary function returning all column names indicating an observation-coordinate, i.e. a coordinate for which only one observation may exist.
#'
#' @param dt optional. You can provide a data table, then the function returns the names of spatial coordinate columns in this data table.
#' @export
obs_dimvars = function(dt = NULL)
{
  options = sort(c(space_dimvars(),'month','season','T','year'))
  if(is.null(dt))
  {
    return(options)
  } else {
    return(intersect(options,names(dt)))
  }
}


#' Auxiliary Function called inside functions that calculate scores
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

#' Auxiliary Function called inside functions that calculate scores
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


#' Auxiliary function returning all column names indicating a spatial coordinate.
#' @param dt optional. You can provide a data table, then the function returns the names of spatial coordinate columns in this data table.
#' @export
space_dimvars = function(dt = NULL)
{
  options = c('country','lat','Lat','lon','Lon','X','Y')
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
