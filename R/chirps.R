###### functions for downloading and processing the chirps data #######

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
              title = 'Please set up a directory for saving data. Do you use the package from inside the Norwegian Computing Center?')
    if(m1 == 1)
    {
      if(Sys.info()["sysname"] == 'Windows')
      {
        cat('M:\\CONFER\\Data\\',file = '~/.config_SeaVal')
      } else {
        cat('/nr/project/stat/CONFER/Data/\n',file = '~/.config_SeaVal')
      }
      print("Thanks, you're set up.")
    }
    if(m1 == 2)
    {
      cat('Please type your data directory. \n
Do not use quotation marks. \n
Use / on Linux (e.g. /nr/project/stat/CONFER/Data/) and \\ on Windows (e.g. C:\\Users\\Documents\\). \n
Make sure you put a / or \\ at the end.')
      rl3 = readline('Input:')
      cat(paste0(rl3,'\n'),file = '~/.config_SeaVal')
      print("Thanks, you're set up.")
    }
  }
  dir = suppressWarnings(readLines(con = '~/.config_SeaVal'))
  return(dir)
}


#' Auxiliary function to access/set the directory for loading and saving CHIRPS data.
#' @param dir The directory
#' @export

chirps_dir = function(dir = file.path(data_dir(),'CHIRPS'))
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
#' downloads CHIRPS monthly data for the GHA-region and saves it as netcdfs.
#' The data is downloaded from the IRI data library (see function code for link), because this data library allows to subset before downloading,
#' unlike the original source at UCSB.
#' As of Feb 2022, the entire CHIRPS-monthly data for the GHA-region is roughly 800MB on disk.
#' The original spatial resolution of CHIRPS is 0.05 degree lon/lat. However, for many applications a coarser resolution is perfectly fine.
#' The function therefore offers the option to also create and save a coarser, upscaled version of the CHIRPS data that allows much faster data processing.
#' Alternatively you can also ONLY save the upscaled version to save disk space (roughly 8MB on disk).
#'
#' @param resolution Shall the data be upscaled? Takes one of three arguments: \itemize{
#'  \item{'both'}{(the default) downloads and saves the data on full resolution and additionally derives an upscaled version. Both will be available later.}
#'  \item{'high'}{downloads and saves on original resolution, but does not upscale.}
#'  \item{'low'}{(for saving disk space) downloads the original resolution, upscales immediately and only saves the upscaled version.}
#' }
#' @param update Logical, if TRUE, previously created files are skipped.
#' @param years,months Which years and months do you want to load? NULL loads everything there is.
#' @param extent vector of length four (xmin,xmax,ymin,ymax), restricting the spatial area.
#' @param timeout_limit how many seconds (per file, i.e. per year) before the download is aborted?
#' @param upscale_grid The coarse grid to which the data is upscaled (only used when resolution is either 'both' or 'high'). Only change this if you really have to.
#'
#' @export

download_chirps_monthly = function(resolution = 'both',update = TRUE,
                                   years = NULL,
                                   months = NULL,
                                   extent = GHA_extent(),
                                   timeout_limit = 300,
                                   upscale_grid = data.table(expand.grid(lon = seq(extent[1],extent[2],0.5),
                                                                         lat = seq(extent[3],extent[4],0.5))))
{
  if(resolution == 'both')
  {
    download_chirps_monthly_high(update = update,
                                 years = years,
                                 months = months,
                                 extent = extent,
                                 timeout_limit = timeout_limit)

    message('download complete, moving to upscaling')
    upscale_chirps(update = update,
                   years = years,
                   months = months,
                   upscale_grid = upscale_grid)
  }
  if(resolution == 'high')
  {
    download_chirps_monthly_high(update = update,
                                 years = years,
                                 months = months,
                                 extent = extent,
                                 timeout_limit = timeout_limit)
  }
  if(resolution == 'low')
  {
    download_chirps_monthly_low(update = update,
                                years = years,
                                months = months,
                                extent = extent,
                                timeout_limit = timeout_limit,
                                upscale_grid = upscale_grid)
  }
}

#' Auxiliary function called by download_chirps_monthly

download_chirps_monthly_high = function(update,
                                        years,
                                        months,
                                        extent,
                                        timeout_limit)
{
  save_dir = file.path(chirps_dir(),'monthly')
  options(timeout = max(timeout_limit, getOption("timeout")))

  if(!dir.exists(save_dir))
  {
    yn = readline(prompt="The directory you try save in does not exist, do you want to create it? y/n:")
    if(yn == 'n') stop()
  }

  dir.create(save_dir,showWarnings = F,recursive = T)

  if(is.null(years)) years = 1981:year(Sys.Date())
  if(is.null(months)) months = 1:12

  mon_to_str = function(x)
  {
    return(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')[x])
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
        if(file.exists(file.path(save_dir,paste0(yy,'_',mm,'.nc'))))
        {
          next
        }
      }

      # skip future months:
      if(yy == year(Sys.Date()) & month(Sys.Date()) < mm)
      {
        next
      }

      message(paste0('downloading ',mm,'/',yy))
      mon = mon_to_str(mm)


      filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28',
                       mon,'%20',yy,'%29%28',mon,'%20',yy,'%29RANGEEDGES/Y/%28',upper,'%29%28',lower,'%29RANGEEDGES/X/%28',left,'%29%28',right,'%29RANGEEDGES/data.nc')

      # the newest data might not be available, but we don't want the download_chirps_monthly function to exit with an error when that happens (e.g. because then it'd skip the upscaling for resolution = 'both')
      res = tryCatch(suppressWarnings(download.file(filestr, destfile = file.path(save_dir,paste0(yy,'_',mm,'.nc')), method = "auto",quiet = TRUE, mode="wb", cacheOK = TRUE)),
                     error = function(cond)
                     {message('This data is not yet available.')})
      if(inherits(res, "error"))
      {
         #error handling code, maybe just skip this iteration using
         next
      }
    }
  }
}




#' Auxiliary function called by download_chirps_monthly

download_chirps_monthly_low = function(update,
                                       years,
                                       months,
                                       extent,
                                       timeout_limit,
                                       upscale_grid)
{
  save_dir = file.path(chirps_dir(),'monthly')
  options(timeout = max(timeout_limit, getOption("timeout")))

  if(!dir.exists(save_dir))
  {
    yn = readline(prompt = "The directory you try save in does not exist, do you want to create it? y/n:")
    if(yn == 'n') stop()
  }

  dir.create(save_dir,showWarnings = F,recursive = T)

  if(is.null(years)) years = 1981:year(Sys.Date())
  if(is.null(months)) months = 1:12


  yms = data.table(expand.grid(year = years,month = months))
  setkey(yms,year,month)
  yms[ ,ym := 12*year + month]

  if(update)
  {
    existing_files = list.files(save_dir)[grep('_us',list.files(save_dir))]
    existing_years =  as.integer(substr(existing_files,1,4))
    existing_months =  as.integer(substr(existing_files,6,nchar(existing_files)-6))
    existing_yms = 12*existing_years + existing_months

    yms = yms[!(ym %in% existing_yms)]

    yms = yms[!(year == year(Sys.Date()) & month > month(Sys.Date()))]
  }

  # use the first file to temporarily save the weights for upscaling:

  if(yms[,.N] > 0)
  {
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


    # download first file:

    mm = yms[1,month]
    mon = mon_to_str(mm)
    yy = yms[1,year]

    filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28',
                     mon,'%20',yy,'%29%28',mon,'%20',yy,'%29RANGEEDGES/Y/%28',upper,'%29%28',lower,'%29RANGEEDGES/X/%28',left,'%29%28',right,'%29RANGEEDGES/data.nc')

    message(paste0('downloading ',mm,'/',yy))

    skip_to_next = FALSE
    res = tryCatch(suppressWarnings(download.file(filestr, destfile = file.path(save_dir,paste0(yy,'_',mm,'.nc')), method = "auto",quiet = TRUE, mode="wb", cacheOK = TRUE)),
                   error = function(cond)
                   {message('This data is not yet available.')
                     skip_to_next <<- TRUE
                     })


    if(! skip_to_next)
    {
      # upscale first file and save weights for upscaling:

      fn = file.path(save_dir,paste0(yy,'_',mm,'.nc'))
      dt_temp = netcdf_to_dt(fn,verbose = 0)
      setnames(dt_temp,c('X','Y'),c('lon','lat'))
      dt_temp = upscale_regular_lon_lat(dt_temp,upscale_grid,'precipitation',
                                        bycols = 'T',save_weights = file.path(save_dir,paste0('temp.csv')))

      nc_out = copy(fn)
      nc_out = substr(nc_out,1,nchar(nc_out)-3)
      nc_out = paste0(nc_out,'_us.nc')

      dt_to_netcdf(dt_temp,'precipitation',
                   units = 'mm/month',
                   dim_vars = c('lon','lat','T'),
                   dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                   nc_out = nc_out,
                   check = 'y')

      invisible(file.remove(fn))
    }
    ### now for all following year-month combinations: ###

    if(yms[,.N] > 1)
    {
      upscale_weights = fread(file.path(save_dir,paste0('temp.csv')))

      for(i in 2:yms[,.N])
      {
        mm = yms[i,month]
        mon = mon_to_str(mm)
        yy = yms[i,year]

        message(paste0('downloading ',mm,'/',yy,'...'))
        skip_to_next = FALSE
        filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28',
                         mon,'%20',yy,'%29%28',mon,'%20',yy,'%29RANGEEDGES/Y/%28',upper,'%29%28',lower,'%29RANGEEDGES/X/%28',left,'%29%28',right,'%29RANGEEDGES/data.nc')
        # use try in case some data is not available
        res = tryCatch(suppressWarnings(download.file(filestr, destfile = file.path(save_dir,paste0(yy,'_',mm,'.nc')), method = "auto",quiet = TRUE, mode="wb", cacheOK = TRUE)),
                       error = function(cond)
                       {message('This data is not yet available.')
                         skip_to_next <<- TRUE})
        if(skip_to_next)
        {
          #error handling code, maybe just skip this iteration using
          next
        }


        #upscaling

        fn = file.path(save_dir,paste0(yy,'_',mm,'.nc'))
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

        invisible(file.remove(fn))
      }
    }
    suppressWarnings(invisible(file.remove(file.path(save_dir,'temp.csv'))))
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


upscale_chirps = function(update = TRUE,
                          years = NULL,
                          months = NULL,
                          upscale_grid = data.table(expand.grid(lon = seq(GHA_extent()[1],GHA_extent()[2],0.5),
                                                                lat = seq(GHA_extent()[3],GHA_extent()[4],0.5))))
{
  save_dir = file.path(chirps_dir(),'monthly')

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
  if(length(files_for_us) == 0)
  {
    message('Everything is already upscaled.')
    return() # just to skip rest of function.
  }
  files_for_us = paste0(files_for_us,'.nc')
  }

  # use the first file to temporarily save the weights for upscaling:

  if(length(files_for_us) > 0)
  {
    print(paste0(1,'/',length(files_for_us)))

    fn = file.path(save_dir,files_for_us[1])
    dt_temp = netcdf_to_dt(fn,verbose = 0)
    setnames(dt_temp,c('X','Y'),c('lon','lat'))
    dt_temp = upscale_regular_lon_lat(dt_temp,upscale_grid,'precipitation',
                                      bycols = 'T',save_weights = file.path(save_dir,'temp.csv'))

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
    upscale_weights = fread(file.path(save_dir,'temp.csv'))

    for(i in 2:length(files_for_us))
    {
      print(paste0(i,'/',length(files_for_us)))
      fn = file.path(save_dir,files_for_us[i])
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

  file.remove(file.path(save_dir,'temp.csv'))
}







######################################

#' Function for reading in (previously downloaded) CHIRPS (monthly) data.
#'
#' The resulting data table contains precip in unit mm/day.
#'
#' @param years,months Optional subset of years and months you want to load
#' @param us logical. If TRUE, the upscaled version is loaded.
#'
#' @return the derived data table
#'
#' @export

load_chirps = function(years =  NULL, months = NULL, us = T)
{
  ch_dir = file.path(chirps_dir(),'monthly')

  if(is.null(years) & is.null(months) & !us)
  {
    rl = readline(prompt = 'You are trying to load all years and months of the CHIRPS data on the original scale. That is potentially a lot of data (roughly 8GB in memory). do you want to proceed? y/n')
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

  if(length(fns) == 0)
  {
    stop('You need to download this data first. Use download_chirps_monthly' )
  }
  dt = list()

  for(i in seq_along(fns))
  {
    ff = fns[i]
    dt_temp = netcdf_to_dt(file.path(ch_dir,ff),verbose = 0)
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

