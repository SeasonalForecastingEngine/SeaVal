# prepare and save some of the clunky stuff for the presentation

rm(list = ls())

library(SeaVal)

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202109/'

fns = list.files(data_dir)
#
#
# for(fn in fns)
# {
#   print(fn)
#   dt = netcdf_to_dt(paste0(data_dir,fn),verbose = 0)
#   print(dt)
# }
# dt = netcdf_to_dt(paste0(data_dir,'PredictedProbabilityRain_Oct_Sep2021.nc'),verbose = 0)


setwd('/nr/project/stat/CONFER/Data/validation/example_data/202109/')

prediction = netcdf_to_dt('PredictedProbabilityRain_Oct_Sep2021.nc')
prediction = prediction[!is.na(below)]

print(prediction)



####
# download_chirps(temp_res = 'daily',
#                 year=  2021,
#                 save_dir = '/nr/project/stat/CONFER/Data/CHIRPS/')
#
# obs2021 = process_chirps(temp_res = 'daily',
#                          year=  2021,
#                          grid = prediction) #!!! CHIRPS is automatically upscaled to resolution of the prediction !!!
#
# ### download and process manually ###
#
# download.file( url = 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_monthly/netcdf/chirps-v2.0.2021.10.nc',
#               destfile = '/nr/project/stat/CONFER/Data/CHIRPS/CHIRPS_monthly_2021_10.nc')


## get CHIRPS and process manually ##




chirps_dir = '/nr/project/stat/CONFER/Data/CHIRPS/'
obs_dt = ncdf_to_dt(paste0(chirps_dir,'CHIRPS_monthly.nc'))

setnames(obs_dt,c('lon','lat','month','prec'))

obs_dt[,year := floor(month/12) + 1960]
obs_dt[,month := floor(month)%%12 + 1]
obs_dt[,prec := prec/30] # calendar is 360 days, original unit is mm/month

obs_dt = obs_dt[month == 10]

obs_new = upscale_nested_griddings(obs_dt,uscol = 'prec',bycols = 'year',coarse_grid = prediction[,.(lon,lat)])

fwrite(obs_new,file = '/nr/project/stat/CONFER/Data/CHIRPS/derived/chirps_monthly_10.csv')

obs_dt2021 = netcdf_to_dt(paste0(chirps_dir,'CHIRPS_monthly_2021_10.nc'))

setnames(obs_dt2021,c('lon','lat','time','prec'))
obs_dt2021[,year := 2021][,month := 10]


obs_dt = fread('/nr/project/stat/CONFER/Data/CHIRPS/derived/chirps_monthly_10.csv')
obs_dt2021 = fread('/nr/project/stat/CONFER/Data/CHIRPS/derived/chirps_monthly_10_2021.csv')





