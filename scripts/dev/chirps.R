

#' Auxiliary function to access the directory used to load and save data. The first time this is called it asks the user to configure it.
#' @param set_dir logical. Set this to TRUE if you have to redefine your data directory.
#' @export

data_dir = function(set_dir = F)
{
  if(set_dir)
  {
    file.remove('~/.config_SeaVal')
  }
  if(!file.exists('~/.config_SeaVal'))
  {
    if(!interactive()) stop('Your data directory does not yet seem to be configured. Please call data_dir() in an interactive session first.')
    m1 = menu(choices = c('yes','no'),
              title = 'You have to set up a directory for saving data. Do you use the package from inside the Norwegian Computing Center?')
    if(m1 == 1)
    {
      cat('/nr/project/stat/CONFER/Data/\n',file = '~/.config_SeaVal')
      print("Thanks, you're set up.")
    }
    if(m1 == 2)
    {
      rl3 = readline('Please type your data directory. For example /nr/project/stat/CONFER/Data/:')
      cat(paste0(rl3,'\n'),file = '~/.config_SeaVal')
      print("Thanks, you're set up.")
    }
  }
  dir = readLines(con = '~/.config_SeaVal')
  return(dir)
}


#' Auxiliary function to access/set the directory for loading and saving CHIRPS data.
#' @param dir The directory
#' @export

chirps_dir = function(dir = paste0(data_dir(),'CHIRPS/'))
{
    return(dir)
}


#' Returns a lon/lat bounding box for the greater horn of Africa region. Format is c(xmin,xmax,ymin,ymax), as for raster::extent
#' @export

GHA_extent = function()
{
  return(c(21.5,51.5,-12,22.5))
}




#' Downloads monthly CHIRPS-data
#'
#' downloads CHIRPS monthly data for the GHA-region and saves it as netcdf. The data is downloaded from the IRI data library (see function code for link), because this data library allows to subset before downloading,
#' unlike the original source at UCSB. As of Feb 2022, the entire CHIRPS-monthly data for this region is roughly 800MB on disk. By default, the function loads everything there is and stops as soon it comes to a file that's
#' not available. It then finishes on an error, nothing to worry about.
#'
#' @param save_dir where should the data be saved?
#' @param update Logical, if TRUE, files for previous years that already exist are not loaded again.
#' @param years,months Which years and months do you want to load? NULL loads everything there is.
#' @param timeout_limit how many seconds (per file, i.e. per year) before the download is aborted?
#'
#' @export


download_chirps_monthly = function(update = TRUE,
                                   years = NULL,
                                   months = NULL,
                                   extent = GHA_extent(),
                                   timeout_limit = 300)
{
  save_dir = paste0(chirps_dir(),'monthly/')
  options(timeout = max(timeout_limit, getOption("timeout")))

  if(!dir.exists(save_dir))
  {
    yn = readline(prompt="The directory you try saving in does not exist, do you want to create it? y/n:")
    if(yn == 'n') stop()
  }

  dir.create(save_dir,showWarnings = F,recursive = T)

  if(is.null(years)) years = 1981:year(Sys.Date())
  if(is.null(months)) months = 1:12

  mon_to_str = function(x)
  {
    return(c('Jan','Feb','Mar','Apr','May','Apr','Jun','Jul','Aug','Sep','Oct','Nov','Dec')[x])
  }

  ### get corner coordinates in the right format:
  left = extent[1]
  if(left < 0)
  {
    left = paste0(-left,'W')
  } else {left = paste0(left,'E')}

  right = extent[2]
  if(right < 0)
  {
    right = paste0(-right,'W')
  } else {right = paste0(right,'E')}

  lower = extent[3]
  if(lower < 0)
  {
    lower = paste0(-lower,'S')
  } else {lower = paste0(lower,'N')}

  upper = extent[4]
  if(upper < 0)
  {
    upper = paste0(-upper,'S')
  } else {upper = paste0(upper,'N')}


  for(yy in years)
  {
    for(mm in months)
    {
      if(update)
      {
        if(file.exists(paste0(save_dir,yy,'_',mm,'.nc')))
        {
          next
        }
      }

      mon = mon_to_str(mm)


      filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28',
                       mon,'%20',yy,'%29%28',mon,'%20',yy,'%29RANGEEDGES/Y/%28',upper,'%29%28',lower,'%29RANGEEDGES/X/%28',left,'%29%28',right,'%29RANGEEDGES/data.nc')
      download.file(filestr, destfile = paste0(save_dir,yy,'_',mm,'.nc'), method = "auto",
                    quiet = FALSE, mode="wb", cacheOK = TRUE)
    }
  }
}


#' Upscales the monthly CHIRPS data to a coarser grid and saves the resulting files as netcdf
#'
#' Uses the function ForecastTools::upscale_regular_lon_lat, but derives the weights for upscaling only once for efficiency and avoids simultaneous loading of all CHIRPS data.
#'
#' @param save_dir where should the data be saved?
#' @param update Logical, if TRUE, files that have already been upscaled are skipped
#' @param years,months Which years and months do you want to upscale? NULL upscales everything there is (except if update is TRUE).
#' @param upscale_grid A regular lon/lat grid for upscaling. Defaults to half degrees.
#'
#' @export


upscale_chirps = function(save_dir = paste0(chirps_dir(),'monthly/'),
                          update = TRUE,
                          years = NULL,
                          months = NULL,
                          upscale_grid = data.table(expand.grid(lon = seq(GHA_extent()[1],GHA_extent()[2],0.5),
                                                                lat = seq(GHA_extent()[3],GHA_extent()[4],0.5))))
{
  all_files = list.files(save_dir)

  us_files = all_files[grep('_us',all_files)]
  files_for_us = setdiff(all_files,us_files)

  if(!is.null(years))
  {
    yys = as.integer(substr(files_for_us,1,4))
    files_for_us = files_for_us[yys %in% years]
  }

  if(!is.null(months))
  {
    mms = as.integer(substr(files_for_us,6,nchar(files_for_us) - 3))
    files_for_us = files_for_us[mms %in% months]
  }

  if(update)
  {
  already_upscaled = unlist(strsplit(us_files,split = '_us.nc'))
  files_for_us = unlist(strsplit(files_for_us,split = '.nc'))
  files_for_us = setdiff(files_for_us,already_upscaled)
  files_for_us = paste0(files_for_us,'.nc')
  }

  # use the first file to temporarily save the weights for upscaling:

  if(length(files_for_us) > 0)
  {
    print(paste0(1,'/',length(files_for_us)))

    fn = paste0(save_dir,files_for_us[1])
    dt_temp = netcdf_to_dt(fn,verbose = 0)
    setnames(dt_temp,c('X','Y'),c('lon','lat'))
    dt_temp = upscale_regular_lon_lat(dt_temp,upscale_grid,'precipitation',
                                      bycols = 'T',save_weights = paste0(save_dir,'temp.csv'))

    nc_out = copy(fn)
    nc_out = substr(nc_out,1,nchar(nc_out)-3)
    nc_out = paste0(nc_out,'_us.nc')

    dt_to_netcdf(dt_temp,'precipitation',
                 units = 'mm/month',
                 dim_vars = c('lon','lat','T'),
                 dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                 nc_out = nc_out,
                 check = 'y')
  }

  if(length(files_for_us) > 1)
  {
    upscale_weights = fread(paste0(save_dir,'temp.csv'))

    for(i in 2:length(files_for_us))
    {
      print(paste0(i,'/',length(files_for_us)))
      fn = paste0(save_dir,files_for_us[i])
      dt_temp = netcdf_to_dt(fn,verbose = 0)

      # get the fine grid index as in upscale_regular_lon_lat
      setnames(dt_temp,c('X','Y'),c('lon','lat'))
      setkey(dt_temp,lon,lat)
      dt_temp[,fg_index := 1:.N]

      # last bit of the upscaling function:
      dt_temp = merge(dt_temp,upscale_weights,'fg_index',allow.cartesian = TRUE)

      # take the weighted average for upscaling:
      dt_temp= dt_temp[!is.na(precipitation)]
      dt_temp = dt_temp[,lapply(.SD,FUN = function(x) sum(area_contr*x,na.rm = T)),.SDcols = 'precipitation',by = c('T','cg_lon','cg_lat')]

      setnames(dt_temp,c('cg_lon','cg_lat'),c('lon','lat'))

      # get name for save file
      nc_out = copy(fn)
      nc_out = substr(nc_out,1,nchar(nc_out)-3)
      nc_out = paste0(nc_out,'_us.nc')

      # save:
      dt_to_netcdf(dt_temp,'precipitation',
                   units = 'mm/month',
                   dim_vars = c('lon','lat','T'),
                   dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                   nc_out = nc_out,
                   check = 'y')
    }
  }

  file.remove(paste0(save_dir,'temp.csv'))
}




######################################


#' Function for reading in (previously downloaded) CHIRPS (monthly) data.
#'
#' The resulting data table contains precip in unit mm/day.
#'
#' @param ch_dir directory where the CHIRPS netcdfs are stored
#' @param years,months Optional subset of years and months you want to load
#' @param grid optional. A data table containing (a regular grid of) lons and lats. The chirps data is upscaled to this grid.
#' @param save_file If you want to save the resulting data table, provide a filename here (including path and .csv at the end). If nothing is provided, the result is not saved, only returned
#'
#' @return the derived data table
#'
#' @export

load_chirps = function(ch_dir = paste0(chirps_dir(),'monthly/'),
                       years =  NULL, months = NULL, us = T)
{
  if(is.null(years) & is.null(months) & !us)
  {
    rl = readline(prompt = 'You are trying to load all years and months of the CHIRPS data on the original scale. That is a lot of data (roughly 8GB in memory). do you want to proceed? y/n')
    if(rl == 'n') stop('loading aborted')
  }

  fns = list.files(ch_dir)

  # subset by upscaling
  fns = fns[grep('_us',fns,invert = !us)]

  # subset by years
  if(!is.null(years))
  {
    yys = as.integer(substr(fns,1,4))
    fns = fns[yys %in% years]
  }

  # subset by months
  if(!is.null(months))
  {
    if(us) mms = as.integer(substr(fns,6,nchar(fns)-6))
    if(!us) mms = as.integer(substr(fns,6,nchar(fns)-3))
    fns = fns[mms %in% months]
  }

  dt = list()

  for(i in seq_along(fns) )
  {
    ff = fns[i]
    dt_temp = netcdf_to_dt(paste0(ch_dir,ff),verbose = 0)
    if(!us) setnames(dt_temp,c('X','Y','precipitation'),c('lon','lat','prec'))
    if(us) setnames(dt_temp,c('precipitation'),c('prec'))

    dt[[i]] = dt_temp
  }

  dt = rbindlist(dt)

  # convert time to reasonable format and precipitation to mm/day
  dt[,year := floor(get('T')/12) + 1960]
  dt[,month := floor(get('T'))%%12 + 1]
  dt[,T:=NULL]
  dt[,prec := prec/30] # calendar is 360 days, original unit is mm/month

  return(dt)
}

