

chirps_dir = function(dir = '/nr/project/stat/CONFER/Data/CHIRPS/')
{
    return(dir)
}

GHA_bounding_coords = function()
{
  return(data.table(lon = c(21.5,51.5),lat = c(-12,22.5)))
}


#' Downloads monthly CHIRPS-data
#'
#' downloads CHIRPS monthly data for the GHA-region and saves it as netcdf. The data is downloaded from the IRI data library (see function code for link), because this data library allows to subset before downloading,
#' unlike the original source at UCSB or CHIRPS-blended at ICPAC. As of December 2021, the entire CHIRPS-monthly data for this region is roughly 800MB on disk.
#'
#' @param save_dir where should the data be saved?
#' @param update Logical, if TRUE, files for previous years that already exist are not loaded again.
#' @param years Which years do you want to load? NULL loads everything there is.
#' @param timeout_limit how many seconds (per file, i.e. per year) before the download is aborted?
#'
#' @export


download_chirps_monthly_iridl = function(save_dir = paste0(chirps_dir(),'monthly/iridl/'),
                                         update = TRUE,
                                         years = NULL,
                                         timeout_limit = 300)
{
  options(timeout = max(timeout_limit, getOption("timeout")))

  if(!dir.exists(save_dir))
  {
    yn = readline(prompt="The directory you try saving in does not exist, do you want to create it? y/n:")
    if(yn == 'n') stop()
  }

  dir.create(save_dir,showWarnings = F,recursive = T)

  if(is.null(years)) years = 1981:year(Sys.Date())


  if(update)
  {
    fns = list.files(save_dir)
    fns = fns[grep('.nc',fns)]
    available_years = as.numeric(gsub(pattern = '.nc',replacement = '', x = fns))
    #remove current year: even if that file exists you probably want to reload that, because there might be new months available since last download
    available_years = setdiff(available_years,year(Sys.Date()))
  } else {
    available_years = NULL
  }

  for(yy in years)
  {
    if(!(yy %in% available_years))
    {
      print(yy)

      filestr = paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28Jan%20',yy,'%29%28Dec%20',yy,'%29RANGEEDGES/Y/%2822.5N%29%2812S%29RANGEEDGES/X/%2821.5E%29%2851.5E%29RANGEEDGES/data.nc')

      # get data
      download.file(filestr, destfile = paste0(save_dir,yy,'.nc'), method = "auto",
                    quiet = FALSE, mode="wb", cacheOK = TRUE)
    }

  }
}


fn = 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28Jan%201981%29%28Dec%201981%29RANGEEDGES/Y/%2822.5N%29%2812S%29RANGEEDGES/X/%2821.5E%29%2851.5E%29RANGEEDGES/data.nc'
fn = 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28Jan%202021%29%28Dec%202021%29RANGEEDGES/Y/%2849.875N%29%2849.975S%29RANGEEDGES/X/%28178.5W%29%28179.95E%29RANGEEDGES/data.nc'
download_chirps_monthly_iridl()

nc = nc_open(fn)



######################################

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

