

dt = netcdf_to_dt(paste0(data_dir,'PredictedProbabilityRain_Mar-May_Feb2021_new.nc'))

dt_obs = netcdf_to_dt(paste0(data_dir,'ObservedRain_Mar-May_Feb2021.nc'))
dt_obs2021 = netcdf_to_dt(paste0(data_dir,'ObservedChirpsRainTotal_MAM2021.nc'),vars = 'precip')

# the 2021 observation is on finer scale:
setnames(dt_obs2021,c('longitude','latitude','precip'),c('lon','lat','prec'))
ggplot_dt(dt_obs2021,'prec',high = 'blue',midpoint = 0)
dt_obs2021 = upscale_to_half_degrees(dt_obs2021,uscol = 'prec',bycols = 'time')
ggplot_dt(dt_obs2021,high = 'blue',midpoint = 0)

# get time into same format:
dt_obs = MSD_to_YM(dt_obs)
print(dt_obs)

dt_obs2021[,date := as.Date(time,origin = '1980-01-01')] # see netcdf description above
dt_obs2021[,year := year(date)][,month := month(date)]
print(dt_obs2021)

# restrict 2021-observations to locations that are not blanked out in the past observations
na_locs = dt_obs[year == year[1],.(lon,lat,is.na(prec))]
print(na_locs)
dt_obs2021 = merge(dt_obs2021,na_locs,by = c('lon','lat'))
dt_obs2021 = dt_obs2021[!(V3)]
ggplot_dt(dt_obs2021, 'prec',high = 'blue',midpoint = 0)

#delete what we don't need and bind together:
dt_obs[,month:=NULL]
dt_obs2021[,c('month','time','date','V3'):=NULL]

dt_obs = rbindlist(list(dt_obs,dt_obs2021),use.names = TRUE)
dt_obs = dt_obs[!is.na(prec)]

# in which climatology tercile lies the observation for which year?
dt_obs = add_tercile_cat(dt_obs)

ggplot_dt(dt_obs,'tercile_cat',low = 'red',high = 'blue')

# merge prediction and corresponding observation:

dt = merge(dt,dt_obs[year == 2021],by = c('lon','lat'))
# transform percentage prediction to probabilities between zero and one:
dt[,normal := normal/100]
dt[,above := above/100]
dt[,below := below/100]

# get Multicategory Brier Skill Score:

mbss = MBSS_dt(dt,obs_col = 'tercile_cat')
ggplot_dt(mbss,high = 'darkgreen',low = 'purple',discrete_cs = TRUE,binwidth = 0.2,midpoint = 0, mn = 'MBSS for MAM tercile forecast 2021')

###

dt_obs[,clim:= mean(prec),by = .(lon,lat)]
dt_obs[,anomaly:= prec - clim]

ggplot_dt(dt_obs[year == 2021],'anomaly',high = 'blue',low = 'red',midpoint = 0, mn = 'observed 2021 MAM precip anomaly')

# or, as discrete plot:
pp1 = ggplot_dt(dt_obs[year == 2021],'anomaly',high = 'blue',low = 'red',midpoint = 0,rr = c(-100,100),discrete_cs = TRUE,breaks = seq(-100,100,40), mn = 'observed 2021 MAM precip anomaly')

pp2 = ggplot_dt(dt,'below',midpoint = 0.33,discrete_cs = TRUE,binwidth = 0.05,mn = 'predicted probability below')
pp3 = ggplot_dt(dt,'normal',midpoint = 0.33,discrete_cs = TRUE,binwidth = 0.05,mn = 'predicted probability normal')
pp4 = ggplot_dt(dt,'above',midpoint = 0.33,discrete_cs = TRUE,binwidth = 0.05,mn = 'predicted probability above')

ggpubr::ggarrange(pp1,pp2,pp3,pp4,ncol = 4)

################

fn_fc = 'Ens_Prec_1monLead_MAM_Prob_EnsRegrCPT-avg.nc'

dt_fc = netcdf_to_dt(paste0(data_dir,fn_fc))
dt_fc = dt_fc[!is.na(below) | !is.na(normal) | !is.na (above)]

p1 = ggplot_dt(dt_fc,data_col = 'below', midpoint = dt_fc[,min(below,na.rm = TRUE)])
p2 = ggplot_dt(dt_fc,data_col = 'normal', midpoint = dt_fc[,min(normal,na.rm = TRUE)], high = 'darkgoldenrod') # see https://www.r-graph-gallery.com/ggplot2-color.html for an overview of color names.
p3 = ggplot_dt(dt_fc,data_col = 'above', midpoint = dt_fc[,min(above,na.rm = TRUE)], high = 'darkgreen')

dt_obs2021 = netcdf_to_dt(paste0(data_dir,'ObservedChirpsRainTotal_MAM2021.nc'),vars = 'precip')

# the 2021 observation is named differently...
setnames(dt_obs2021,c('longitude','latitude','precip'),c('lon','lat','prec'))
dt_obs2021[,year := 2021][,time := NULL]

# ... and it is on higher resolution:
ggplot_dt(dt_obs2021,'prec',high = 'blue',midpoint = 0)

chirps_dir = '/nr/project/stat/CONFER/Data/'

chirps_dt = netcdf_to_dt(paste0(chirps_dir,'CHIRPS_new.nc'))
setnames(chirps_dt,c('X','Y','precipitation'),c('lon','lat','prec'))
chirps_dt = MSD_to_YM(chirps_dt,timecol = 'T',origin = '1960-01-01')

chirps_dt = chirps_dt[month %in% 3:5][,.(prec = sum(prec)),by = .(lon,lat,year)]



test = bil_interpol_grid_dt(chirps_dt,expand_variable_cols = c('year'),function_value_cols = 'prec',DT_new = new_grid)

chirps_dt = rbindlist(list(chirps_dt,dt_obs2021),use.names = T)
chirps_dt[,clim:= mean(prec),by = .(lon,lat)]
chirps_dt[,ano:= prec - clim]
ggplot_dt(chirps_dt[year == 2021],'ano',high = 'blue',low = 'red',midpoint = 0)


ggpubr::ggarrange(p1,p2,p3,ncol = 3)

dt_obs

dt = merge(dt,dt_obs[year == 2021,.(lon,lat,tercile_cat)],by = c('lon','lat'))

dt[,below := below / 100]
dt[,normal := normal / 100]
dt[,above := above / 100]

temp = copy(dt)[!is.na(below)|!is.na(normal)|!is.na(above)]

temp[,e_max:= (!is.na(below) & tercile_cat == -1) + (!is.na(normal) & tercile_cat == 0) + (!is.na(above) & tercile_cat == 1)]

get_pmax = function(below,normal,above){
  vout = rep(0,length(below))
  vout[!is.na(below)] = below[!is.na(below)]
  vout[!is.na(normal)] = normal[!is.na(normal)]
  vout[!is.na(above)] = above[!is.na(above)]
  return(vout)
}

temp[,p_max := get_pmax(below,normal,above)]

temp[,cBS_max := p_max^2 - 2*p_max*e_max + 1]
temp[,cBSS_max := 1 - 9/10*cBS_max]
ggplot_dt(temp,'cBSS_max',midpoint = 0)
