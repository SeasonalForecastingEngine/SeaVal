library(SeaVal)

rm(list=ls())
### set forecast specifications ###
season = "Mar-May"
sea   = "MAM"
mons  = 3:4
#
init   = "Feb"
imon   = 2
fyear  = 2022
#paste0(year,sea)

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202202/'
  #"C:\\Users\\user\\Desktop\\CONFER\\Data\\CHIRPS\\monthly\\"
plot_dir = data_dir # (in case you want to save your plots in a different directory)

#install.packages('devtools')
library(devtools)
#devtools::install_github('SeasonalForecastingEngine/ForecastTools')
#devtools::install_github('SeasonalForecastingEngine/SeaVal')
setwd(data_dir)
library(SeaVal)


data("chirps_monthly")
print(chirps_monthly)
#
fn = paste0("CorrelationSkillRain_",season,"_",init,fyear,".nc") ; fn
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
fn_pred = paste0("CrossValidatedPredictedRain_",season,"_",init,fyear,".nc")

dt_pred = netcdf_to_dt(paste0(data_dir,fn_pred))

print(dt_pred)

# next, get the observations:
fn_obs = paste0("ObservedRain_",season,"_",init,fyear,".nc") ; fn_obs
dt_obs = netcdf_to_dt(paste0(data_dir,fn_obs))               ; dt_obs

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

# convert time from the 'mon ths since date' (MSD) format to years and months (YM)
dt = MSD_to_YM(dt,origin = '1981-01-01') # (the origin was documented in the netcdf, see above.)
head(dt)
#print(dt)

########### next, let's checkout predicted probabilities ###########
# get predictions:
dt = netcdf_to_dt(paste0(data_dir,'PredictedProbabilityRain_',season,'_',init,fyear,'.nc'))
head(dt)
#print(dt)

#########################################
#### get observations: Alternative 1 ####
# This is 'the old way', but still works:

# past observations:
dt_obs = netcdf_to_dt(paste0(data_dir,'ObservedRain_',season,'_',init,fyear,'.nc'))
head(dt_obs)

# 2021 observation:
dt_obs2021 = netcdf_to_dt(paste0(data_dir,'ObservedChirpsRainTotal_',sea,fyear,'.nc'))
head(dt_obs2021)

# the 2021 observation is named differently...
setnames(dt_obs2021,c('longitude','latitude','precip'),c('lon','lat','prec'))
head(dt_obs2021)

ggplot_dt(dt_obs2021,'prec',high = 'blue',midpoint = 0)

# we can upscale it to the same grid as dt_obs
dt_obs2021 = upscale_regular_lon_lat(dt_obs2021,dt_obs,uscol = 'prec')
pp1 = ggplot_dt(dt_obs2021,high = 'blue',midpoint = 0)
pp1

# the time format is different for the two observation data tables, see netcdf description above.
# For dt_obs the format is month since date, and can be changed to year-month like this:
dt_obs = MSD_to_YM(dt_obs)
# For dt_obs2021 it's the number of days since 1980-01-01, and we can do the following:
#dt_obs2021[,year := 2021][,month := 8]
dt_obs2021[,year := paste0(fyear),][,month := paste0(imon)] ; print(dt_obs2021)
#print(dt_obs2021)

# Next, let's restrict 2021-observations to locations that are not blanked out in the past observations:
na_locs = dt_obs[year == year[1],.(lon,lat,is.na(prec))]
na_locs = na_locs[!(V3)] # only keep rows for which V3 is FALSE

# Restrict to region not masked out
dt_obs2021 = merge(dt_obs2021,na_locs[,.(lon,lat)],by = c('lon','lat'))
head(dt_obs2021)
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
dt_obs = load_chirps(months = mons)#paste0(mons))# season is as indicated above
#dt_obs = load_chirps(months = 10:12)         # season was June to September
head(dt_obs)
print(dt_obs)

#plot seasonal mean
pp1 = ggplot_dt(dt_obs[year == paste0(fyear)], 'prec',rr = NULL, high = 'green')#,midpoint = 0)
pp1
### End of Alternative 2 ###########################
####################################################

# take seasonal mean
dt_obs = dt_obs[,.(prec = mean(prec)), by = .(lon,lat,year)]

# in which climatology tercile lies the observation for which year?
dt_obs = add_tercile_cat(dt_obs)

# let's also add climatology and anomaly for later use:
dt_obs[,clim := mean(prec),by = .(lon,lat)]
dt_obs[,anomaly := prec - mean(prec),by = .(lon,lat)]

# plot anomaly:
pp3 = ggplot_dt(dt_obs[year == paste0(fyear)], 'anomaly',high = 'blue',low = 'red',midpoint = 0)
pp3


pp4 = ggplot_dt(dt_obs[year == paste0(fyear)],'tercile_cat',low = 'orange',high = 'green')
pp4

# merge prediction and corresponding observation:
dt = merge(dt,dt_obs[year == paste0(fyear)],by = c('lon','lat'))
# transform percentage prediction to probabilities between zero and one:
dt[,normal := normal/100]
dt[,above := above/100]
dt[,below := below/100]

print(dt)

# get Multicategory Brier Skill Score:
mbss = MBS(dt,o = 'tercile_cat')
pp5 = ggplot_dt(mbss,high = 'darkgreen',low = 'purple',discrete_cs = TRUE,binwidth = 0.2,midpoint = 0,
                mn = paste0('MBSS for ',sea, ' tercile forecast ',fyear))
pp5

#get Ignoorance score
igs = IGSS(dt)
#pp6 = ggplot_dt(igs,'IGSS',midpoint = 0, rr = rr)
pp6 = ggplot_dt(igs,'IGSS',midpoint = 0, rr = c(-1,1), mn = paste0('IGSS for ',sea, ' tercile forecast ',fyear))
pp6

# hit scores spatially averaged:
hs = HS(dt, by = NULL, pool = c('lon','lat'))

# hit skill score map:
hss = HSS(dt)
print(hss)
# note that the hit skill score is just the difference between the hit scores for the categories with the highest and lowest probabilities.
# So if it is only evaluated for one year, and for each location separately, it is either -1,0, or 1

pp7 = ggplot_dt(hss, 'HSS', discrete_cs = T,breaks = c(-1,-0.5,0.5,1))# + ggtitle('Hit skill score')
pp7 = ggplot_dt(igs,'IGSS',midpoint = 0, mn = paste0('IGSS for ',sea, ' tercile forecast ',fyear))
pp7

# ROC curves and reliability diagrams:

ROC_curve(dt) # I've tried this but something seems to be wrong with the function, got to double-check
rel_diag(dt)

## added this section for percentile ##
dt_obs_hr = load_chirps(us = F, month = mons)# paste0(mons)) # us = F means the data is loaded on high spatial resolution, for all Aprils available. This takes a bit of time, so you can set us = T for faster but coarser pictures.
# take seasonal mean
dt_obs_hr = dt_obs_hr[,.(prec = mean(prec)), by = .(lon,lat,year)]
vm  = ver_map(dt_obs_hr, 'prec', climatology_period = 1991:2020, yy = 2022) # generate the plot
plot(vm)
##

