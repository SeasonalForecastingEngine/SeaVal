#' Function to create a mask of dry regions from CHIRPS
#'
#' @description A gridpoint is masked for a given season (either 'MAM', 'JJAS' or 'OND'), if, on average, less than 10% of the annual total of rainfall
#' occur during the season. This function loads CHIRPS data, and derives this mask as a data table of lon, lat coordinates, only containing
#' the coordinates that shouldn't be masked. You can apply the mask to an existing data table using dt = combine(dt,mask).
#'
#' @param season For which season do you want to calculate the mask? Needs to be either 'MAM', 'JJAS' or 'OND'.
#' @param clim_years Numeric vector of years. Which years should be used to establish the mask?
#' @param version,resolution,us Passed to \code{\link{load_chirps}}. Which CHIRPS version do you want to use and on what resolution?
#'
#'@examples
#'if(interactive()) get_mask('MAM')
#'
#' @export

get_mask = function( season,
                     clim_years = 1990:2020,
                     version = 'UCSB',
                     resolution = 'low',
                     us = (resolution == 'low'))

{
  season_months = prec = fraction = mask = NULL
  ch_dir = file.path(chirps_dir(),version)
  if (us) ch_dir = file.path(ch_dir,'upscaled')
  if (!dir.exists(ch_dir)) stop("I didn't find the directory with the CHIRPS data. Have you downloaded/upscaled CHIRPS? If not, run download_chirps_monthly.")
  ch_dir = file.path(ch_dir,'masks')
  dir.create(ch_dir,showWarnings = FALSE)

  fn = file.path(ch_dir,paste0(season,'_',min(clim_years),'_',max(clim_years),'.csv'))

  if(file.exists(fn)) {
    dt = fread(fn)
    return(dt)
  } else {
    mms = season_months(season)
    dt = load_chirps(years = clim_years,months = NULL,us = us,version = version)
    clim = dt[,.(clim = mean(prec)),.(lon,lat)]
    season_average = dt[month %in% mms,.(season_average = mean(prec)),.(lon,lat)]
    dt = merge(clim,season_average,c('lon','lat'))
    dt[,fraction := (season_average*length(mms))/(clim*12)]
    dt[,mask:= fraction <= 0.1]
    #save:
    fwrite(dt[(!mask),.(lon,lat)],file = fn)
    return(dt[(!mask),.(lon,lat)])
  }
}
