library(SeaVal)


library(ncdf4)


### create observation file ###


get_chirps = function(year)
{
  # get arbitrary prediction file for upscaling to prediction grid:
  dt_pred = netcdf_to_dt('/nr/project/stat/CONFER/Data/validation/weekly/20200811/PrecDaily.nc')

  options(timeout = max(300, getOption("timeout")))
  download.file(url = paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/netcdf/p05/chirps-v2.0.',year,'.days_p05.nc'),
                destfile = paste0('/nr/project/stat/CONFER/Data/CHIRPS_daily',year,'.nc'))


  dt_obs = netcdf_to_dt(paste0('/nr/project/stat/CONFER/Data/CHIRPS_daily',year,'.nc'),subset_list = list(latitude = c(-15,25),longitude = c(20,55)))

  setnames(dt_obs,c('longitude','latitude'),c('lon','lat'))

  dt_obs = upscale_nested_griddings(dt_obs,'precip',
                                    coarse_grid = unique(dt_pred[,.(lon,lat)]),
                                    bycols = 'time',
                                    type = 'within')

  ggplot_dt(dt_obs[time == time[1]],'precip')


  dt_obs[,date := as.Date(time,origin = '1980-01-01')]
  dt_obs[,time := NULL]


  setnames(dt_obs,c('precip'),c('prec'))

  fwrite(dt_obs,file = paste0('/nr/project/stat/CONFER/Data/chirps_daily_upscaled_',year,'.csv'))
}

######################

dt_pred = netcdf_to_dt('/nr/project/stat/CONFER/Data/validation/weekly/20200811/PrecDaily.nc')
dt_pred2 = netcdf_to_dt('/nr/project/stat/CONFER/Data/validation/weekly/20210817/Prec30mmMaskExtreme.nc')
dt_pred3 = netcdf_to_dt('/nr/project/stat/CONFER/Data/validation/weekly/20210817/PrecExtreme.nc')
dt_pred4 = netcdf_to_dt('/nr/project/stat/CONFER/Data/validation/weekly/20210817/TavgDaily.nc')



# attention! This function assumes that both for the observation and prediciton we have round(lat,1) = lat and the same for lon (because joining data tables can SUCK!!!)

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

#####


dt_ev = netcdf_to_dt('/nr/project/stat/CONFER/Data/validation/weekly/20210921/eval.nc')


theme_set(theme_bw(base_size = 24))

pp = ggplot_dt(dt_ev,'mean_obs',midpoint = 0,high = 'blue') + ggtitle('20210921 - observed average precip')
ggsave(pp,file = '/nr/project/stat/CONFER/plots/weekly_eval/example_observed_precip.pdf')

pp2 = ggplot_dt(dt_ev,'mean_fc',midpoint = 0,high = 'blue') + ggtitle('20210921 - predicted average precip')
ggsave(pp2,file = '/nr/project/stat/CONFER/plots/weekly_eval/example_predicted_precip.pdf')

pp3 = ggplot_dt(dt_ev,'bias',midpoint = 0) + ggtitle('20210921 - prediction bias')
ggsave(pp3,file = '/nr/project/stat/CONFER/plots/weekly_eval/example_bias.pdf')

pp4 = ggplot_dt(dt_ev,'MSE',midpoint = 0,rr = c(0,10)) + ggtitle('20210921 - prediction MSE')
ggsave(pp4,file = '/nr/project/stat/CONFER/plots/weekly_eval/example_mse.pdf')

