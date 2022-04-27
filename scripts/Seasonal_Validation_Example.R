rm(list=ls())


### set forecast specifications ###
season = "Jun-Sep"
sea = "JJAS"
init   = "May"
year = 2021

data_dir = "/nr/project/stat/CONFER/Data/validation/example_data/202105/"
plot_dir = data_dir # (in case you want to save your plots in a different directory)




#install.packages('devtools')
#library(devtools)
#devtools::install_github('SeasonalForecastingEngine/ForecastTools')
#devtools::install_github('SeasonalForecastingEngine/SeaVal')
setwd(data_dir)
library(SeaVal)


data("chirps_monthly")
print(chirps_monthly)
#
fn = paste0("CorrelationSkillRain_",season,"_",init,"2021.nc")
dt = netcdf_to_dt(paste0(data_dir,fn))
print(dt)
#
pp = ggplot_dt(dt,
          mn = paste0('Corr. skill rain ',season, ', ' ,init, '  initialized'), # title
          rr = c(-1,1), # range of the colorbar
          discrete_cs = TRUE,binwidth = 0.4) # discretize colorbar
pp
# you can save this plot with ggsave(pp, file = <directory-and-filename>)


# get the CV-file:
fn_pred = paste0("CrossValidatedPredictedRain_",season,"_",init,year,".nc")

dt_pred = netcdf_to_dt(paste0(data_dir,fn_pred))

print(dt_pred)

# next, get the observations:
fn_obs = paste0("ObservedRain_",season,"_",init,year,".nc")
dt_obs = netcdf_to_dt(paste0(data_dir,fn_obs))

# Merge observations with forecasts
setnames(dt_pred,'prec','prediction')
setnames(dt_obs,'prec','observation')
#
dt = merge(dt_pred,dt_obs,by = c('lon','lat','time'))
head(dt)
#print(dt)

# remove all rows with missing predictions:
dt = dt[!is.na(prediction)]
head(dt)

# convert time from the 'months since date' (MSD) format to years and months (YM)
dt = MSD_to_YM(dt,origin = '1981-01-01') # (the origin was documented in the netcdf, see above.)
head(dt)
#print(dt)


########### next, let's checkout predicted probabilities ###########

# get predictions:
dt = netcdf_to_dt(paste0(data_dir,'PredictedProbabilityRain_',season,'_',init,year,'.nc'))
head(dt)
#print(dt)

#########################################
#### get observations: Alternative 1 ####
# This is 'the old way', but still works:

# past observations:
dt_obs = netcdf_to_dt(paste0(data_dir,'ObservedRain_',season,'_',init,year,'.nc'))
head(dt_obs)

# 2021 observation:
dt_obs2021 = netcdf_to_dt(paste0(data_dir,'ObservedChirpsRainTotal_',sea,year,'.nc'))
head(dt_obs2021)

# the 2021 observation is named differently...
setnames(dt_obs2021,c('longitude','latitude','precip'),c('lon','lat','prec'))
head(dt_obs2021)

ggplot_dt(dt_obs2021,'prec',high = 'blue',midpoint = 0)

# we can upscale it to the same grid as dt_obs
dt_obs2021 = upscale_regular_lon_lat(dt_obs2021,dt_obs,uscol = 'prec')
ggplot_dt(dt_obs2021,high = 'blue',midpoint = 0)

# the time format is different for the two observation data tables, see netcdf description above.
# For dt_obs the format is month since date, and can be changed to year-month like this:
dt_obs = MSD_to_YM(dt_obs)
# For dt_obs2021 it's the number of days since 1980-01-01, and we can do the following:
dt_obs2021[,year := 2021][,month := 5]
print(dt_obs2021)

# Next, let's restrict 2021-observations to locations that are not blanked out in the past observations:
na_locs = dt_obs[year == year[1],.(lon,lat,is.na(prec))]
na_locs = na_locs[!(V3)] # only keep rows for which V3 is FALSE

# Restrict to region not masked out
dt_obs2021 = merge(dt_obs2021,na_locs[,.(lon,lat)],by = c('lon','lat'))

pp2 = ggplot_dt(dt_obs2021, 'prec',high = 'blue',midpoint = 0)
pp2

# combine with old observations:
dt_obs[,month:=NULL]
dt_obs2021[,'month':=NULL]

dt_obs = rbindlist(list(dt_obs,dt_obs2021),use.names = TRUE)
dt_obs = dt_obs[!is.na(prec)]

### End of Alternative 1 ###########################
####################################################
### Alternative 2: Use the load_chirps function ####
# This makes things much easier, but you need to have run
# download_chirps_monthly previously:

download_chirps_monthly()

dt_obs = load_chirps(months = 6:9)# season was June to September

# take seasonal mean:
dt_obs = dt_obs[,.(prec = mean(prec)), by = .(lon,lat,year)]

### End of Alternative 2 ###########################
####################################################

# in which climatology tercile lies the observation for which year?
dt_obs = add_tercile_cat(dt_obs)
# let's also add climatology and anomaly for later use:
dt_obs[,clim := mean(prec),by = .(lon,lat)]
dt_obs[,anomaly := prec - mean(prec),by = .(lon,lat)]

# plot anomaly:
pp3 = ggplot_dt(dt_obs[year == 2021], 'anomaly',high = 'blue',low = 'red',midpoint = 0)
pp3


pp4 = ggplot_dt(dt_obs[year == 2021],'tercile_cat',low = 'orange',high = 'green')
pp4


# merge prediction and corresponding observation:
dt = merge(dt,dt_obs[year == 2021],by = c('lon','lat'))
# transform percentage prediction to probabilities between zero and one:
dt[,normal := normal/100]
dt[,above := above/100]
dt[,below := below/100]

print(dt)

# get Multicategory Brier Skill Score:
mbss = MBS(dt,o = 'tercile_cat')
pp5 = ggplot_dt(mbss,high = 'darkgreen',low = 'purple',discrete_cs = TRUE,binwidth = 0.2,midpoint = 0, mn = 'MBSS for JJAS tercile forecast 2021')
pp5
