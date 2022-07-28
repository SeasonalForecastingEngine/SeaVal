###### functions for downloading and processing the chirps data #######

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
  dir = file.path(chirps_dir(),'monthly')

  if(resolution == 'both')
  {

    message(paste0('The data is stored in\n',dir,'\nFor changing this, run data_dir(set_dir = TRUE).'))
    download_chirps_monthly_high(update = update,
                                 years = years,
                                 months = months,
                                 extent = extent,
                                 timeout_limit = timeout_limit,
                                 save_dir = dir)

    message('download complete, moving to upscaling')
    upscale_chirps(update = update,
                   years = years,
                   months = months,
                   upscale_grid = upscale_grid,
                   root_dir = dir)
  }
  if(resolution == 'high')
  {
    message(paste0('The data is stored in\n',dir,'\nFor changing this, run data_dir(set_dir = TRUE).'))
    download_chirps_monthly_high(update = update,
                                 years = years,
                                 months = months,
                                 extent = extent,
                                 timeout_limit = timeout_limit,
                                 save_dir = dir)
  }
  if(resolution == 'low')
  {
    message(paste0('The data is stored in\n',file.path(dir,'upscaled'),'\nFor changing this, run data_dir(set_dir = TRUE).'))
    download_chirps_monthly_low(update = update,
                                years = years,
                                months = months,
                                extent = extent,
                                timeout_limit = timeout_limit,
                                upscale_grid = upscale_grid,
                                root_dir = dir)
  }

  delete_redundant_files(dir)
}

#' Auxiliary function called by download_chirps_monthly

download_chirps_monthly_high = function(update,
                                        years,
                                        months,
                                        extent,
                                        timeout_limit,
                                        save_dir = file.path(chirps_dir(),'monthly'))
{

  options(timeout = max(timeout_limit, getOption("timeout")))

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

      message(paste0(mm,'/',yy))
      mon = mon_to_str(mm)

      filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28',
                       mon,'%20',yy,'%29%28',mon,'%20',yy,'%29RANGEEDGES/Y/%28',upper,'%29%28',lower,'%29RANGEEDGES/X/%28',left,'%29%28',right,'%29RANGEEDGES/data.nc')

      # the newest data might not be available, but we don't want the download_chirps_monthly function to exit with an error when that happens (e.g. because then it'd skip the upscaling for resolution = 'both')
      res = tryCatch(suppressWarnings(download.file(filestr, destfile = file.path(save_dir,paste0(yy,'_',mm,'.nc')), method = "auto",quiet = TRUE, mode="wb", cacheOK = TRUE)),
                     error = function(cond)
                     {message('Not yet available on the IRI data library. Trying to download preliminary version from UCSB.')

                       res_prelim = tryCatch(suppressWarnings(download_chirps_prelim_aux(years = yy,
                                                                        months = mm,
                                                                        extent = extent,
                                                                        timeout_limit = timeout_limit,
                                                                        nonprelim_dir = save_dir)),
                                             error = function(cond)
                                             {message('This data is not yet available.')})
                     })


    }
  }
}




#' Auxiliary function called by download_chirps_monthly

download_chirps_monthly_low = function(update,
                                       years,
                                       months,
                                       extent,
                                       timeout_limit,
                                       upscale_grid,
                                       root_dir = file.path(chirps_dir(),'monthly'))
{
  save_dir = file.path(root_dir,'upscaled')
  dir.create(save_dir,showWarnings = F,recursive = T)

  save_dir_prelim = file.path(root_dir,'upscaled','prelim')
  dir.create(save_dir_prelim,showWarnings = F,recursive = T)

  options(timeout = max(timeout_limit, getOption("timeout")))

  if(is.null(years)) years = 1981:year(Sys.Date())
  if(is.null(months)) months = 1:12

  yms = data.table(expand.grid(year = years,month = months))
  setkey(yms, year, month)
  yms[ ,ym := 12 * year + month]
  yms = yms[!(year == year(Sys.Date()) & month > month(Sys.Date()))]

  if(update)
  {
    existing_files = list.files(save_dir)

    existing_years =  as.integer(substr(existing_files,1,4))
    existing_months =  as.integer(substr(existing_files,6,nchar(existing_files)-3))
    existing_yms = 12 * existing_years + existing_months

    # remove the year-months for which the final data has been downloaded:
    yms = yms[!(ym %in% existing_yms)]

    # keep track of the year-months for which the preliminary data has been loaded:
    existing_files_prelim = list.files(save_dir_prelim)

    existing_years_prelim =  as.integer(substr(existing_files_prelim,1,4))
    existing_months_prelim =  as.integer(substr(existing_files_prelim,6,nchar(existing_files_prelim)-3))
    existing_yms_prelim = 12 * existing_years_prelim + existing_months_prelim
  }


  ### process data: ###
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


    # download files:
    for(i in 1:yms[,.N])
    {
      # download high-resolution file:

      mm = yms[i,month]
      mon = mon_to_str(mm)
      yy = yms[i,year]

      filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28',
                       mon,'%20',yy,'%29%28',mon,'%20',yy,'%29RANGEEDGES/Y/%28',upper,'%29%28',lower,'%29RANGEEDGES/X/%28',left,'%29%28',right,'%29RANGEEDGES/data.nc')

      message(paste0(mm,'/',yy))

      skip_to_next = FALSE # For data that is not available
      res = tryCatch(suppressWarnings(download.file(filestr, destfile = file.path(root_dir,paste0(yy,'_',mm,'.nc')), method = "auto",quiet = TRUE, mode="wb", cacheOK = TRUE)),
                     error = function(cond)
                       {
                       skip_to_next <<- TRUE # If preliminary data is downloaded or neither preliminary nor non-preliminary data is available, the upscaling part of the loop is skipped.
                       message('Not yet available on the IRI data library. Trying to download preliminary version from UCSB.')
                       res_prelim = tryCatch(suppressWarnings(download_chirps_prelim_aux(years = yy,
                                                                                         months = mm,
                                                                                         extent = extent,
                                                                                         timeout_limit = timeout_limit,
                                                                                         nonprelim_dir = save_dir)),
                                             # NOTE: We call download_chirps_prelim_aux with nonprelim_dir = save_dir,
                                             # which is root_dir/upscaled. This creates the directory root_dir/upscaled/prelim and writes the preliminary data there.
                                             # The download_chirps_prelim_aux-function upscales the preliminary data automatically, to a grid fetched from nonprelim_dir
                                             # (this was implemented because the preliminary data comes on a different grid). Consequently, because the grid is retrieved from
                                             # root_dir/upscaled, the preliminary data is directly upscaled to the grid we want, as part of the download function.
                                             # The upscaling matrix is not saved, but newly derived for every preliminary file, but I guess that's fine, since there should
                                             # be at most 2 preliminary files, and in the vast majority of cases only 1 or 0.
                                             error = function(cond)
                                             {
                                               message('This data is not yet available.')
                                             })
                     })

      if(skip_to_next) next

      # check whether the 'temp.csv' file exists (containing the weights for upscaling), if not create it

      create_temp = !file.exists(paste0(save_dir,'temp.csv'))

      #upscaling with deriving weights for the first file:
      if(create_temp)
      {
        fn = file.path(root_dir,paste0(yy,'_',mm,'.nc'))
        dt_temp = netcdf_to_dt(fn,verbose = 0)
        setnames(dt_temp,c('X','Y'),c('lon','lat'))
        dt_temp = upscale_regular_lon_lat(dt_temp,upscale_grid,'precipitation',
                                          bycols = 'T',save_weights = file.path(save_dir,paste0('temp.csv')))

        nc_out = file.path(save_dir,paste0(yy,'_',mm,'.nc'))

        dt_to_netcdf(dt_temp,'precipitation',
                     units = 'mm/month',
                     dim_vars = c('lon','lat','T'),
                     dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                     nc_out = nc_out,
                     check = 'y')

        invisible(file.remove(fn))
      }
      #upscaling without deriving weights for all others:
      if(!create_temp)
      {
        upscale_weights = fread(file.path(save_dir,paste0('temp.csv')))

        #upscaling
        fn = file.path(root_dir,paste0(yy,'_',mm,'.nc'))
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
        nc_out = file.path(save_dir,paste0(yy,'_',mm,'.nc'))

        # save:
        dt_to_netcdf(dt_temp,'precipitation',
                     units = 'mm/month',
                     dim_vars = c('lon','lat','T'),
                     dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                     nc_out = nc_out,
                     check = 'y')
        # get rid of the big one:
        invisible(file.remove(fn))
      }
    }
    suppressWarnings(invisible(file.remove(file.path(save_dir,'temp.csv'))))
  }
}





#' Upscales the monthly CHIRPS data to a coarser grid and saves the resulting files as netcdf
#'
#' this is mostly auxiliary and called from download_chirps_monthly.
#' Uses the function ForecastTools::upscale_regular_lon_lat, but derives the weights for upscaling only once for efficiency and avoids simultaneous loading of all CHIRPS data.
#'
#' @param update Logical, if TRUE, files that have already been upscaled are skipped
#' @param years,months Which years and months do you want to upscale? NULL upscales everything there is (except if update is TRUE).
#' @param upscale_grid A regular lon/lat grid for upscaling. Defaults to half degrees.
#'
#' @export

upscale_chirps = function(update = TRUE,
                          years = NULL,
                          months = NULL,
                          upscale_grid = data.table(expand.grid(lon = seq(GHA_extent()[1],GHA_extent()[2],0.5),
                                                                lat = seq(GHA_extent()[3],GHA_extent()[4],0.5))),
                          root_dir = file.path(chirps_dir(),'monthly'),
                          us_dir = file.path(root_dir,'upscaled'))
{

  message('upscaling...')

  # get files for upscaling:

  if(is.null(years)) years = 1981:year(Sys.Date())
  if(is.null(months)) months = 1:12

  yys = rep(years,each = length(months))
  mms = rep(months,length(years))

  files_for_us = list.files(root_dir)
  if(length(grep('temp',files_for_us)) > 0 ) files_for_us = files_for_us[-grep('temp',files_for_us)]

  if(update)
  {
    already_upscaled = list.files(us_dir)
    if(length(grep('temp',already_upscaled)) > 0 ) already_upscaled = already_upscaled[-grep('temp',already_upscaled)]

    # subtract from files for upscaling:
    files_for_us = setdiff(files_for_us,already_upscaled)
    # avoid listed directories:
    files_for_us = files_for_us[grep('.nc',files_for_us)]
    if(length(files_for_us) == 0)
    {
      message('Everything is already upscaled.')
      return() # just to skip rest of function.
    }
  }

  # use the first file to temporarily save the weights for upscaling:

  if(length(files_for_us) > 0)
  {
    print(paste0(1,'/',length(files_for_us)))

    fn = file.path(root_dir,files_for_us[1])
    dt_temp = netcdf_to_dt(fn,verbose = 0)
    setnames(dt_temp,c('X','Y'),c('lon','lat'))
    dt_temp = upscale_regular_lon_lat(dt_temp,upscale_grid,'precipitation',
                                      bycols = 'T',save_weights = file.path(us_dir,'temp.csv'))

    nc_out = file.path(us_dir,files_for_us[1])

    dt_to_netcdf(dt_temp,'precipitation',
                 units = 'mm/month',
                 dim_vars = c('lon','lat','T'),
                 dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                 nc_out = nc_out,
                 check = 'y')
  }


  if(length(files_for_us) > 1)
  {
    upscale_weights = fread(file.path(us_dir,'temp.csv'))

    for(i in 2:length(files_for_us))
    {
      print(paste0(i,'/',length(files_for_us)))
      fn = file.path(root_dir,files_for_us[i])
      dt_temp = netcdf_to_dt(fn,verbose = 0)

      # get the fine grid index as in upscale_regular_lon_lat
      setnames(dt_temp,c('X','Y'),c('lon','lat'))
      setkey(dt_temp,lon,lat)
      dt_temp[,fg_index := 1:.N]

      # last bit of the upscaling function:
      dt_temp = merge(dt_temp,upscale_weights,'fg_index',allow.cartesian = TRUE)

      # take the weighted average for upscaling:
      dt_temp= dt_temp[!is.na(precipitation)]
      dt_temp = dt_temp[,lapply(.SD,FUN = function(x) sum(area_contr * x,na.rm = T)),.SDcols = 'precipitation',by = c('T','cg_lon','cg_lat')]

      setnames(dt_temp,c('cg_lon','cg_lat'),c('lon','lat'))

      # get name for save file
      nc_out = file.path(us_dir,files_for_us[i])

      # save:
      dt_to_netcdf(dt_temp,'precipitation',
                   units = 'mm/month',
                   dim_vars = c('lon','lat','T'),
                   dim_var_units = c('degree longitude','degree_latitude','months since 1960-01-01'),
                   nc_out = nc_out,
                   check = 'y')
    }
  }

  file.remove(file.path(us_dir,'temp.csv'))
}







######################################

#' Function for reading in (previously downloaded) CHIRPS (monthly) data.
#'
#' The resulting data table contains precip in unit mm/day.
#'
#' @param years,months Optional subset of years and months you want to load. The default is to load everything that has been downloaded locally.
#' You can update your local CHIRPS download by calling download_chirps_monthly
#' @param us logical. If TRUE, the upscaled version is loaded.
#'
#' @return the derived data table
#'
#' @export

load_chirps = function(years =  NULL, months = NULL, us = T)
{
  ch_dir = ifelse(us, no = file.path(chirps_dir(),'monthly'), yes = file.path(chirps_dir(),'monthly','upscaled'))
  prelim_dir = file.path(ch_dir,'prelim')

  if(is.null(years) & is.null(months) & !us)
  {
    rl = readline(prompt = 'You are trying to load all years and months of the CHIRPS data on the original scale. That is potentially a lot of data (roughly 8GB in memory). do you want to proceed? y/n')
    if(rl == 'n') stop('loading aborted')
  }

  fns = list.files(ch_dir)
  fns=  fns[grep('.nc',fns)]
  fns_prelim = list.files(prelim_dir)

  # subset by years
  if(!is.null(years))
  {
    yys = as.integer(substr(fns,1,4))
    fns = fns[yys %in% years]
  }

  # subset by months
  if(!is.null(months))
  {
    mms = as.integer(substr(fns,6,nchar(fns)-3))
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

  if(length(fns_prelim) >0 )
  {
    # subset by years
    if(!is.null(years))
    {
      yys = as.integer(substr(fns_prelim,1,4))
      fns_prelim = fns_prelim[yys %in% years]
    }

    # subset by months
    if(!is.null(months))
    {
      mms = as.integer(substr(fns_prelim,6,nchar(fns_prelim)-3))
      fns_prelim = fns_prelim[mms %in% months]
    }

    dt_prelim = list()


    for(i in seq_along(fns_prelim))
    {
      ff = fns_prelim[i]
      message(paste0('The file ',ff,' contains preliminary data.'))

      dt_temp = netcdf_to_dt(file.path(prelim_dir,ff),verbose = 0)
      if(!us) setnames(dt_temp,c('X','Y','precipitation'),c('lon','lat','prec'))
      if(us) setnames(dt_temp,c('precipitation'),c('prec'))

      dt_prelim[[i]] = dt_temp
    }

    dt_prelim = rbindlist(dt_prelim)

    dt = rbindlist(list(dt,dt_prelim))
  }

  # convert time to reasonable format and precipitation to mm/day
  dt[,year := floor(get('T')/12) + 1960]
  dt[,month := floor(get('T'))%%12 + 1]
  dt[,T:=NULL]
  dt[,prec := prec/30] # calendar is 360 days, original unit is mm/month

  return(dt)
}

#########################################



#' Auxiliary function for downloading the preliminary CHIRPS monthly data
#'
#' This data becomes available earlier, but it has to be downloaded from UCSB.
#' The function checks whether the non-preliminary version exists and only downloads otherwise.
#' Annoyingly, the grid of UCBS and IRIDL are shifted against each other. Therefore this function also interpolates the UCSB data to the IRIDL grid, which makes it a bit slower.
#' In particular, everything will crash if you have never downloaded a non-preliminary file and try to download a preliminary one.
#'
#' @param years years for which you want to download
#' @param months months for which you want to download
#' @param extent Spatial window for downloading
#' @param timeout_limit How many seconds before download is aborted.
#' @param nonprelim_dir Directory where the non-preliminary CHIRPS data is stored.
#' @param save_dir Directory where the function stores the preliminary data.
#'
#' @export


download_chirps_prelim_aux = function(years,
                                      months,
                                      extent,
                                      timeout_limit = 300,
                                      nonprelim_dir = file.path(chirps_dir(),'monthly'),
                                      save_dir = file.path(nonprelim_dir,'prelim'))
{
  dir.create(save_dir,showWarnings = F)

  options(timeout = timeout_limit)

  for(yy in years)
  {
    for(mm in months)
    {
      fn = file.path(save_dir,paste0(yy,'_',mm,'.nc'))

      if(file.exists(file.path(nonprelim_dir,paste0(yy,'_',mm,'.nc'))) | file.exists(fn)) next

      mstr = ifelse(mm<10,yes = paste0(0,mm),no = mm)
      suppressMessages(download.file(url = paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_monthly/netcdf/chirps-v2.0.',yy,'.',mstr,'.nc'),
                                     destfile = file.path(save_dir,'temp.nc')))


      # full chirps data is downloaded. Chop to size and make it look more like the IRI data.
      # We don't directly define a new netcdf, but take an intermediate step making it a data.table. The reason is that the grids of the preliminary data and of the non-preliminary data are different.
      # So we have to use the upscaling function anyway, and that one takes data tables.

      nc=  nc_open(file.path(save_dir,'temp.nc'))

      lons = ncdf4::ncvar_get(nc, varid = 'longitude')
      rel_inds = which(lons %between% extent[1:2])
      lons_start = rel_inds[1]
      lons_count = length(rel_inds)
      rel_lons = lons[rel_inds]

      lats = ncdf4::ncvar_get(nc, varid = 'latitude')
      rel_inds = which(lats %between% extent[3:4])
      lats_start = rel_inds[1]
      lats_count = length(rel_inds)
      rel_lats=lats[rel_inds]

      times = ncdf4::ncvar_get(nc, varid = 'time')
      times = as.Date(times, origin = '1980-01-01')
      times_new = 12*(year(times)-1960) + month(times) - 0.5# convert to weird format used by IRI data library:

      ncdf4::nc_close(nc)

      dt = data.table(lon = rep(rel_lons,lats_count),lat = rep(rel_lats,each = lons_count),precip = as.vector(precip))
      dt[,T := times_new]

      # upscale to resolution of the other files:
      ref_files = list.files(nonprelim_dir)
      if(length(grep(pattern = 'temp',ref_files))>0)
      {
        ref_files = ref_files[-grep(pattern = 'temp',ref_files)]
      }
      if(length(ref_files == 0))
      {
        file.remove(file.path(save_dir,'temp.nc'))

        stop(paste0("Preliminary data was successfully downloaded, but I didn't find any non-preliminary files in the corresponding directory ",save_dir,".
At least one non-preliminary file is required, such that the preliminary data can be mapped to the non-preliminary grid.
The preliminary data has been removed again."),call. = FALSE)
      }
      ref_file = ref_files[1]

      chirps_iri_grid = netcdf_to_dt(file.path(nonprelim_dir,ref_file),verbose = 0)
      chirps_iri_grid = chirps_iri_grid[,.(X,Y)]
      setnames(chirps_iri_grid,c('lon','lat'))

      dt_new = upscale_regular_lon_lat(dt,chirps_iri_grid,uscols = 'precip',bycols = 'T')

      setnames(dt_new,c('lon','lat'),c('X','Y'))
      dt_to_netcdf(dt_new,vars = 'precip',units = 'mm/month',
                   dim_vars = c('X','Y','T'),
                   dim_var_units = c('degree longitude',
                                     'degree latitude',
                                     'months since 1960-01-01'),
                   nc_out = file.path(save_dir,paste0(yy,'_',mm,'.nc')))

      file.remove(file.path(save_dir,'temp.nc'))

    }
  }
}


#' Auxiliary function cleaning out the directories
delete_redundant_files = function(dir)
{
  temp_files = list.files(dir,pattern = 'temp',recursive = 'TRUE')
  file.remove(file.path(dir,temp_files))

  # check whether a preliminary file has been replaced and can be removed.

  if('prelim' %in% list.files(dir))
  {
    check = intersect(list.files(dir),list.files(file.path(dir,'prelim')))
    if(length(check) > 0) file.remove(file.path(dir,check))
  }

  usdir = file.path(dir,'upscaled')
  if(dir.exists(usdir))
  {
    check = intersect(list.files(usdir),list.files(file.path(usdir,'prelim')))
    if(length(check) > 0) file.remove(file.path(usdir,check))
  }
}

