rm(list = ls())

devtools::load_all()


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


### This needs work now:
delete_redundant_files = function(dir)
{
  all_files = list.files(dir)
  possible_high_res = paste0(rep(1981:year(Sys.Date()),each = 12),'_',1:12,'.nc')
  possible_us = paste0(rep(1981:year(Sys.Date()),each = 12),'_',1:12,'_us.nc')

  high_res_files = intersect(all_files,possible_high_res)
  us_files = intersect(all_files,possible_us)
  if(length(grep('prelim',all_files)) > 0)
  {
    prelim_files = all_files[grep('prelim',all_files)]
    for(i in seq_along(prelim_files))
    {
      nonprelim_fn = sub('_prelim','',prelim_files[i])
      if(file.exists(file.path(dir,nonprelim_fn)))
      {
        file.remove(file.path(dir,prelim_files[i]))
        # output message that data has been updated (only if the non-upscaled version is updated, to avoid double messages)
        if(length(grep('us',prelim_files[i]))>0)
        {
          yy = strsplit(nonprelim_fn,'_')[[1]][1]
          mm = strsplit(strsplit(nonprelim_fn,'_')[[1]][2],'.',fixed = T)[[1]][1]
          message('The preliminary data for ',mm,'/',yy,' has now been updated.')
        }
      }
    }
  }

  if(length(grep('prelim',all_files)) > 0)
  {
    temp_files = all_files[grep('temp',all_files)]
    file.remove(file.path(dir,temp_files))
  }
}



