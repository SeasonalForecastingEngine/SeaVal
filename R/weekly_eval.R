
#'@param start_date start date of the forecast as character string in format YYYYMMDD
#'@param weekly_fc_dir directory where weekly forecasts are stored. At ICPAC this should be something like
#'@param obs_file file name of the observations, currently takes .csv, so requires to run netcdf_to_dt before. Needs to be essentially on the same grid.
#'@param nc_out file name (and directory) for the netcdf file the results are written to.
#'
eval_weekly_precip = function(start_date = '20200804',
                              weekly_fc_dir = '/nr/project/stat/CONFER/Data/validation/weekly/',
                              obs_file = '/nr/project/stat/CONFER/Data/chirps_daily_upscaled_2020.csv',
                              nc_out = paste0('/nr/project/stat/CONFER/Data/validation/weekly/',start_date,'/eval.nc'))
{
  dt_pred = netcdf_to_dt(paste0(weekly_fc_dir,start_date,'/PrecDaily.nc'),verbose = 0)
  dt_obs = fread(obs_file)

  # get origin date for time:
  nc = nc_open(paste0('/nr/project/stat/CONFER/Data/validation/weekly/',start_date,'/PrecDaily.nc'))
  check1 = grepl(nc$dim$time$units,pattern = 'hours since ')
  check2 = grepl(nc$dim$time$units,pattern = ' 06:00:00')
  if(check1 & check2)
  {
    od = strsplit(nc$dim$time$units,split = 'hours since ')
    od = strsplit(od[[1]][2],split = ' 06:00:00')[[1]]
  } else {
    od = readline(prompt = paste0('the unit for time is not in the standard format, we get from the netcdf: ',nc$dim$time$units,'\n please enter the origin-date manually:'))
  }

  dt_pred[,date := as.Date(time/24,origin = od)]
  dt_pred[,time:= NULL]

  dt_pred[,lon:= round(lon,1)][,lat:= round(lat,1)]
  dt_obs[,lon:= round(lon,1)][,lat:= round(lat,1)]

  # evaluation:
  dt_ev = merge(dt_pred,dt_obs,by = c('lon','lat','date'))

  dt_ev[,MSE := mean(dailyrain - prec)^2,by = .(lon,lat)]
  dt_ev[,bias := mean(dailyrain - prec),by = .(lon,lat)]

  dt_ev[,mean_obs := mean(prec),by = .(lon,lat)]
  dt_ev[,mean_fc := mean(dailyrain),by = .(lon,lat)]

  dt_ev[,sum_obs := sum(prec),by = .(lon,lat)]
  dt_ev[,sum_fc := sum(dailyrain),by = .(lon,lat)]


  dt_ev_sm = unique(dt_ev[,.(lon,lat,MSE,bias,mean_obs,mean_fc,sum_obs,sum_fc)])
  dt_ev_sm = dt_ev_sm[!is.na(mean_fc)]

  dt_to_netcdf(dt = dt_ev_sm,
               vars = c('MSE','bias','mean_obs','mean_fc'), units = c('(mm/day)^2',rep('mm/day',3)),
               dim_vars = c('lon','lat'),
               nc_out = nc_out)
}

####### eval everything: ########

weekly_dir = '/nr/project/stat/CONFER/Data/validation/weekly/'
start_dates = list.dirs(path = weekly_dir,full.names = FALSE)
start_dates = start_dates[(length(start_dates)-1):length(start_dates)]

for(ii in seq_along(start_dates))
{
  sd = start_dates[ii]
  yy = substring(sd,1,4)
  print(sd)
  eval_weekly_precip(start_date = sd,obs_file = paste0('/nr/project/stat/CONFER/Data/chirps_daily_upscaled_',yy,'.csv'))
}
