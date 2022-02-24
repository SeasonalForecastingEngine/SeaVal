
### Code for importing geotiffs:



url = 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_monthly/tifs/chirps-v2.0.1981.01.tif.gz'
download.file(url,destfile = paste0(save_dir,'chirps-v2.0.1981.01.01.tif.gz'),method = 'auto')
R.utils::gunzip(paste0(save_dir,"chirps-v2.0.1981.01.01.tif.gz"), remove = FALSE)


library(raster)

tif_to_dt
test = raster(paste0(save_dir,"chirps-v2.0.1981.01.01.tif"))

test = raster(url)

bbox = extent(21.5,51.5,-12,22.5)

ttest = crop(test,bbox)

if(!identical(crs(test),crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
{
  stop('coord. ref. system does not seem to be lon/lat...')
}

ttest[ttest < -100] = NA

coords = xyFromCell(ttest,1:length(ttest))
coords = as.data.table(coords)
setnames(coords,c('lon','lat'))

dt = coords[,'prec' := ttest[1:length(ttest)]]
ggplot_dt(dt)

new_coords = as.data.table(expand.grid(lon = 21.5:51.5, lat = -12:22.5))

dt_new = upscale_regular_lon_lat(dt,new_coords,'prec')


ttest = as(test,'SpatialGridDataFrame')



### Attempts on downloading daily chirps:

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

    urll = 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc'
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
