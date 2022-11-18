#' function for converting netcdfs to long data tables.
#'
#' @description This function is deprecated, use netcdf_to_dt instead. It crashes when the netcdf has 'empty' dimension variables that are not used by any variable.
#' This is the case for some netcdfs provided by ICPAC and CORDEX.
#'
#' @param nc either character string with the name of the .nc file (including path), or an object of type ncdf4
#' @param subset_list named list for subsetting. The names must match dimension names of the nc file. The range of the vector contained on each page defines which subset of the data is read out.
#' @param printunits Logical: Should the units be printed
#' @return long data table.
#'
#' @import ncdf4
#'
#' @author Claudio
#'
#' @export

ncdf_to_dt = function(nc,subset_list = NULL,printunits = TRUE)
{
  warning('This function is deprecated. Please use netcdf_to_dt instead.')

  if(is.character(nc)) nc = nc_open(nc)

  dim_list = c()
  dim_ids = c()
  units = c()
  dim_starts = c()
  dim_counts = c()

  #read dimension variables:
  for(dim_ind in 1:nc$ndim)
  {
    nn = nc$dim[[dim_ind]]$name[1]

    dd = ncvar_get(nc,nn)

    if(nn %in% names(subset_list))
    {
      rel_indices = which(dd %between% range(subset_list[[which(names(subset_list) == nn)]]))

    } else {
      rel_indices = 1:length(dd)
    }

    dim_start = min(rel_indices)
    dim_starts = c(dim_starts,dim_start)
    names(dim_starts)[dim_ind] = nn

    dim_count = max(rel_indices) - min(rel_indices) + 1
    dim_counts = c(dim_counts,dim_count)

    dd = dd[dim_start:(dim_start + dim_count - 1)]     # NOT dd[rel_indices], since dd is not neccessarily sorted?


    dim_list = c(dim_list,list(dd))
    names(dim_list)[dim_ind] = nn
    dim_ids = c(dim_ids,nc$dim[[dim_ind]]$id)

    units = c(units,paste0(nc$dim[[dim_ind]]$name[1],':   ',nc$dim[[dim_ind]]$units))
  }

  ### sometimes the dimensions follow a different order in the variables as in nc$dim??? ###

  dim_order = nc$var[[1]]$dimids

  # check whether all variables have the same dimesnion ordering:
  if(nc$nvars > 1)
  {
    for(var_id in 2:nc$nvars)
    {
      if(!identical(dim_order,nc$var[[var_id]]$dimids))
      {
        stop('different variables have different dimension ordering, currently not supported.')
      }
    }
  }

  dim_list = dim_list[match(dim_ids,dim_order)]
  dim_starts = dim_starts[match(dim_ids,dim_order)]
  dim_counts = dim_counts[match(dim_ids,dim_order)]

  dim_dt = as.data.table(expand.grid(dim_list))



  # append variable values to dim_dt:

  return_dt = dim_dt


  for(var_ind in 1:nc$nvars)
  {

    temp = ncvar_get(nc, varid = nc$var[[var_ind]]$name,start = unname(dim_starts),count = unname(dim_counts))

    var_dt = data.table( as.vector(temp))
    setnames(var_dt,nc$var[[var_ind]]$name)

    return_dt = data.table(return_dt,var_dt)
    units = c(units,paste0(nc$var[[var_ind]]$name,':   ',nc$var[[var_ind]]$units))
  }

  # printout units:
  if(printunits)
  {
    catout = paste0(c('Units:',units),sep = '',collapse = "\n")
    cat(catout)
  }
  return(return_dt)
}

#' function for converting netcdfs to long data tables.
#'
#' @description The function converts netcdfs into long data.tables.
#' Be aware that the data table can be much larger in memory, especially if you have many dimension variables.
#'
#' @param nc Either a character string with the name of the .nc file (including path), or an object of type ncdf4.
#' @param vars Which variables should be read from the netcdf? Either a character vector of variable names, or an
#' integer vector of variable indices. If this is NULL, all variables are read.
#' @param verbose Either 0, 1 or 2. How much information should be printed?
#' The default (2) is to print the entire netcdf information (as output by \code{ncdf4::nc_open}), 1 just prints the units for all variables, 0 (or any other input)
#' prints nothing.
#' @param trymerge logical. If TRUE, a single data table containing all variables is returned, else a list of data
#' tables, one for each variable. The latter is much more memory efficient if you have multiple variables depending
#' on different dimensions.
#' @param subset_list A named list for reading only subsets of the data. Currently only 'rectangle subsetting' is provided, i.e. you can provide two limit values for each dimension and everything between
#' will be read. The names of the pages of subset_list must correspond to the names of dimension variables in the netcdf, and each page should contain a (two-element-)range vector.
#' For example, subsetting a global dataset to just East Africa could look like this: subset_list = list(latitude = c(-15,25),longitude = c(20,55)).
#' Non-rectangular subsetting during reading a netcdf seems to be difficult, see ncvar_get. Every dimension variable not named in subset_list is read entirely.
#' @param keep_nas Should missing values be kept? If FALSE (the default), missing values are not included in the returned data table.
#' If this is set to TRUE, the data table is constructed from the full data-cube (meaning its number of rows is the product of the length of the dimension variables, even if many coordinates
#' have missing data). This makes the returned data table potentially much larger and is almost never an advantage. It is only allowed, because it can make complex bookkeeping tasks easier
#' (specifically upscaling many CHIRPS-netcdfs with the same coordinates while saving the upscaling weights in a matrix).
#'
#' @return A data table if \code{trymerge == TRUE} or else a list of data tables.
#'
#' @import ncdf4
#'
#' @examples {\dontrun{
#' fn = 'CorrelationSkillRain_Feb-Apr_Feb2021.nc'
#' test = netcdf_to_dt(nc)
#' }
#' }
#'
#' @author Claudio
#'
#' @export

netcdf_to_dt = function(nc, vars = NULL,
                        verbose = 2,
                        trymerge = TRUE,
                        subset_list = NULL,
                        keep_nas = FALSE)
{
  if(is.character(nc))
  {
    if(!file.exists(nc)) stop(paste0('The file ',nc,' does not exist.\nRemember that you need to include the path to the directory the file is located in.'))
    nc = ncdf4::nc_open(nc)
  }
  if(verbose == 2) print(nc)


  # convert vars to numeric vector indexing the variables you want to extract:
  if(is.null(vars))
  {
    vars = 1:nc$nvars
  } else if(is.character(vars))
  {
    vars = match(vars,names(nc$var))
  }

  dt_list = list()

  if(!is.null(subset_list))
  {
    which_dims_subsetted = match(names(subset_list),names(nc$dim))
    for(ii in which_dims_subsetted)
    {
      dim_name = nc$dim[[ii]]$name
      vals = nc$dim[[ii]]$vals
      read_inds = which(vals %between% subset_list[[dim_name]])
      subset_list[[dim_name]] = list(start = min(read_inds),count = max(read_inds) - min(read_inds) + 1)
    }
  }


  units = NULL
  for(var in vars)
  {
    v = nc$var[[var]]
    if(v$ndims == 0)
    {
      warning(paste0('The variable ',v$name,' has no dimensions and is skipped.'))
      next
    }

    units = c(units, paste0(v$name,': ',v$units))

    # subsetting:
    start = rep(1,v$ndims)
    count = rep(-1,v$ndims)
    for(ii in 1:length(v$dim))
    {
      if(v$dim[[ii]]$name %in% names(subset_list))
      {
        start[ii] = subset_list[[v$dim[[ii]]$name]]$start
        count[ii] = subset_list[[v$dim[[ii]]$name]]$count
      }
    }


    dim_lengths = v$varsize

    dim_lengths = count * (count != -1) + v$varsize * (count == -1)

    if(prod(dim_lengths) > 2^31) stop('The resulting data.table (for variable ',v$name,') is too long! Try subsetting.')

    dt_temp = NULL

    # generate data.table with dimension variables: Do NOT use nvar_get here because that fails when dimvarid is missing :-P

    for(i in 1:v$ndims)
    {

      ### vectorize dimension entries: ###
      if(count[i] == -1)
      {
        # we need to first repeat using times = {the product of lengths of 'later' dimension vectors}...
        dimension_vector = rep(v$dim[[i]]$vals,times = prod(dim_lengths[(i+1):(v$ndims + 1)],na.rm = T))
        #... and then to repeat this using each = {the product of lengths of 'earlier' dimension vectors}:
        dimension_vector = rep(dimension_vector,each = prod(dim_lengths[0:(i-1)],na.rm = T))
      } else {
        read_inds = start[i] : (start[i] + count[i] - 1)
        # we need to first repeat using times = {the product of lengths of 'later' dimension vectors}...
        dimension_vector = rep(v$dim[[i]]$vals[read_inds],times = prod(dim_lengths[(i+1):(v$ndims + 1)],na.rm = T))
        #... and then to repeat this using each = {the product of lengths of 'earlier' dimension vectors}:
        dimension_vector = rep(dimension_vector,each = prod(dim_lengths[0:(i-1)],na.rm = T))

      }
      dt_ttemp = data.table(dimension_vector)
      setnames(dt_ttemp,v$dim[[i]]$name)


      dt_temp = data.table(dt_temp, dt_ttemp)
    }

    # add variable values:

    dt_ttemp = data.table(as.vector(ncvar_get(nc,varid = v$name,start = start,count = count)))
    setnames(dt_ttemp,v$name)

    if(keep_nas) dt_list = c(dt_list,list(data.table(dt_temp,dt_ttemp)))
    if(!keep_nas) dt_list = c(dt_list,list(data.table(dt_temp,dt_ttemp)[!is.na(get(v$name))]))

  }


  # merge list into data.table:
  if(trymerge)
  {
    if(length(dt_list) == 1)
    {
      dt_list = rbindlist(dt_list) # for some reason unlist(,recursive = FALSE) does not work here and also unlists the data table
    } else
    {
      dt0 = dt_list[[1]]
      for(i in 2:length(dt_list))
      {
        if(length(intersect(names(dt0),names(dt_list[[i]]))) == 0)
        {
          stop('Your file has variables with disjoint dimensions, which should not be stored in a single data table. Either set trymerge to FALSE or select variables with overlapping dimensions in vars.' )
        }
        dt0 = merge(dt0,dt_list[[i]],by = intersect(names(dt0),names(dt_list[[i]])), all = T)
      }
      dt_list = dt0
    }
  }

  # print units of dimension variables:
  if(verbose == 1)
  {
    for(i in 1:length(nc$dim))
    {
      units = c(units, paste0(names(nc$dim)[i],': ',nc$dim[[i]]$units))
    }

    catout = paste0(c('Units:',units),sep = '',collapse = "\n")
    cat(catout)
  }

  ncdf4::nc_close(nc)

  return(dt_list)
}

#' Write a netcdf from a long data tables.
#'
#'
#' @description This function writes a netcdf from a long data table, the usual data format in SeaVal.
#' The function currently does not support writing netcdfs with multiple variables that have different sets of dimension variables!
#'
#' @param dt a data.table
#' @param vars names of columns in dt containing variables. If this is NULL, the function guesses and asks for confirmation.
#' @param units character vector containing the units for vars (in the same order). If this is NULL (default), the user is prompted for input.
#' @param dim_vars names of columns in dt containing dimension variables. If this is NULL, the function guesses and asks for confirmation.
#' @param dim_var_units character vector containing the units for dim_vars (in the same order). If this is NULL (default), the user is prompted for input (except for lon/lat).
#' @param nc_out File name (including path) of the netcdf to write.
#' @param check Only used when a file with the given name already exists. Default is to prompt user for input. This can be avoided (e.g. if you automatically want to overwrite a lot of files) by setting check = 'y'.
#' @param description For adding a global attribute 'Description' as a string.
#'
#' @return none.
#'
#' @import ncdf4
#'
#'
#' @author Claudio
#'
#' @export

dt_to_netcdf = function(dt,nc_out,
                        vars = NULL,
                        units = NULL,
                        dim_vars = dimvars(dt), dim_var_units = NULL,
                        check = TRUE,
                        description = NULL)
{
  if(check & file.exists(nc_out))
  {
    test = readline(prompt = paste0('The netcdf file already exists. Do you want to overwrite? [y/n]'))
    if(test == 'y') {
      invisible(file.remove(nc_out))
    }else(stop('aborted.'))
  }


  if(is.null(vars))
  {
    vars = setdiff(names(dt),dim_vars)
    if(check)
    {
    test = readline(prompt = paste0("You didn't provide information which columns of dt are dim_vars and which are variables.
My guess would be\nvariables: ",paste(vars,collapse = ', '),
"\ndim_vars: ",paste(dim_vars,collapse = ', '),
"\nIs that ok?  [y/n]"))
    if(test != 'y') stop('please provide vars and dim_vars manually.')
    }
  }

  # get data into correct order:
  setkeyv(dt,rev(dim_vars))

  # expand data.table by NAs to contain all values in a 'rectangular' array, and get dimension variables:
  dim_list = list()
  dim_vars_ncdf = list()

  for(ii in seq_along(dim_vars))
  {
    dv = dim_vars[ii]
    vals = sort(unique(dt[,get(dv)]))
    temp = list(vals)
    names(temp) = dv

    ### get unit for dimvar ###
    if(!is.null(dim_var_units)){
      unit = dim_var_units[ii]
    } else {
      if(dv == 'lon') unit = 'degree longitude'
      if(dv == 'lat') unit = 'degree latitude'
      if(!(dv %in% c('lon','lat')))
      {
        unit = readline(prompt = paste0('Enter the unit of the dimension variable ',dv,':\n'))
      }
    }

    dim_var = ncdf4::ncdim_def(name = dv,units = unit,vals = vals)
    dim_vars_ncdf = c(dim_vars_ncdf,list(dim_var))
    dim_list = c(dim_list,temp)
  }

  # get the 'rectangular' dt:
  expand_dt = as.data.table(expand.grid(dim_list))
  dt = merge(dt,expand_dt,all = TRUE)
  setkeyv(dt,rev(dim_vars))

  # define the variables of the netcdf:
  vars_ncdf = list()
  for(ii in seq_along(vars))
  {
    v = vars[ii]
    if(!v %in% names(dt)) stop(paste0(v,' is not a column name in dt.'))

    vals = dt[,get(v)]

    if(is.null(units))
      {
      unit = readline(prompt = paste0('Enter the unit of the variable ',v,':\n'))
    } else {
      unit = units[ii]
    }

    ncvar = ncdf4::ncvar_def(name = v,
                             units = unit,
                             dim = dim_vars_ncdf,
                             missval = NA)

    vars_ncdf = c(vars_ncdf,list(ncvar))
  }

  # write the netcdf file:
    nc = ncdf4::nc_create(filename = nc_out,vars = c(vars_ncdf))

  for(ii in seq_along(vars))
  {
    v = vars[ii]
    values = dt[,get(v)]

    ncdf4::ncvar_put(nc, varid = vars[ii],vals = values)
  }

  if(!is.null(description))
  {
    ncdf4::ncatt_put(nc,varid = 0,attname = 'Description',attval = description)
  }
    ncdf4::nc_close(nc)
}
