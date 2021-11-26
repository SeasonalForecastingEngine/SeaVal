

chirps_dir = function(dir = '/nr/project/stat/CONFER/Data/CHIRPS/')
{
    return(dir)
}

GHA_bounding_box

#' Download CHIRPS
#'
#' downloads CHIRPS data and saves it as netcdf. Can download both monthly and daily data. Monthly data is downloaded from http://digilib.icpac.net/SOURCES/.ICPAC/.CHIRPS-BLENDED/.monthly/.rainfall/.precipitation/.
#' Daily data is downloaded from https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/netcdf/p05/
#'
#' There are several sources providing CHIRPS data, all of them with advantages and disadvantages:
#' Sources for the monthly data:
#' \itemize{
#' \item the climate hazard center 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/'. This is the original host (I think), but they only have a single .nc file covering the entire globe
#' \item ICPACs data library, this is the CHIRPS-BLENDED dataset, but it sometimes lags a few months behind
#' \item
#' }
#'
#' @param temp_res temporal resolution. Either 'monthly' or 'daily'.
#' @param years Which years to download. Only used when temp_res == 'daily', for monthly data, everything is downloaded.
#' @param save_dir Where to save the netcdfs.
#'
#' @export

download_chirps = function(temp_res = 'monthly', save_dir = chirps_dir(), update = TRUE, years = NULL, origin = 'chc')
{
  options(timeout = max(300, getOption("timeout")))
  if(temp_res == 'daily')
  {
    for(year in years)
    {
    print(year)
    download.file(url = paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/netcdf/p05/chirps-v2.0.',year,'.days_p05.nc'),
                  destfile = paste0(save_dir,'CHIRPS_daily',year,'.nc'))
    }
  }
  if(temp_res == 'monthly')
  {
    if(origin == 'chc')
    {
      url = 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc'
    }
    urll = 'http://digilib.icpac.net/SOURCES/.ICPAC/.CHIRPS-BLENDED/.monthly/.rainfall/.precipitation/data.nc'
    download.file(urll, destfile = paste0(save_dir,'CHIRPS_monthly.nc'), method = "auto",
                  quiet = FALSE, mode="wb", cacheOK = TRUE)

  }
}


#' Function for reading CHIRPS netcdfs, and turning them into data tables of convenient format
#'
#' The resulting data table contains precip in unit mm/day
#'
#' @param temp_res temporal resolution, either 'monthly' or 'daily'
#' @param chirps_dir directory where the CHIRPS netcdfs are stored
#' @param year only used when temp_res == 'daily'. For which year do you want to load the data? A single year has roughly 6 GB in memory, so you have to process them one by one
#' @param grid optional. A data table containing (a regular grid of) lons and lats. The chirps data is upscaled to this grid.
#' @param save_file If you want to save the resulting data table, provide a filename here (including path and .csv at the end). If nothing is provided, the result is not saved, only returned
#'
#' @return the derived data table
#'
#' @export

process_chirps = function(temp_res,
                          chirps_dir = '/nr/project/stat/CONFER/Data/CHIRPS/',
                          year=  NULL,grid = NULL, save_file = NULL)
{
  if(temp_res == 'monthly')
  {
    dt = netcdf_to_dt(paste0(chirps_dir,'CHIRPS_monthly.nc'),verbose = 0)
    setnames(dt,c('X','Y','precipitation'),c('lon','lat','prec'))

    if(!is.null(grid))
    {
      dt = upscale_nested_griddings(dt,'prec',
                                    coarse_grid = grid,
                                    bycols = 'T',
                                    type = 'within')
    }


    dt[,year := floor(get('T')/12) + 1960]
    dt[,month := floor(get('T'))%%12 + 1]
    dt[,prec := prec/30] # calendar is 360 days, original unit is mm/month


  }
  if(temp_res == 'daily')
  {
    dt = netcdf_to_dt(paste0(chirps_dir,'CHIRPS_daily',year,'.nc'),subset_list = list(latitude = c(-15,25),longitude = c(20,55)),verbose = 0)

    setnames(dt,c('longitude','latitude','precip'),c('lon','lat','prec'))

    if(!is.null(grid))
    {
      dt = upscale_nested_griddings(dt,'prec',
                                    coarse_grid = grid,
                                    bycols = 'time',
                                    type = 'within')
    }

    dt[,date := as.Date(time,origin = '1980-01-01')]
    dt[,time := NULL]

  }

  if(!is.null(save_file)) fwrite(dt,file = save_file)

  return(dt)

}

