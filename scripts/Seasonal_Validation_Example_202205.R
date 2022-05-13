rm(list=ls())

# this script evaluates the MAM probability forecast based on March observations only!!!
# that is not recommended practice, but March-CHIRPS is all I had available.

### set forecast specifications ###
season = "Mar-May"
sea = "MAM"
init   = "Feb"
year = 2022

library(SeaVal)

data_dir = "/nr/project/stat/CONFER/Data/validation/example_data/202202/"

fns = list.files(data_dir)
plot_dir = data_dir # (in case you want to save your plots in a different directory)

### focus on tercile probabilities: ###


########### next, let's checkout predicted probabilities ###########

# get predictions:
dt = netcdf_to_dt(paste0(data_dir,'PredictedProbabilityRain_',season,'_',init,year,'.nc'))

#########################################
#### get observations: Alternative 1 ####
# This is 'the old way', but still works:
download_chirps_monthly()
obs = load_chirps(us = FALSE,months = 3)

obs = upscale_regular_lon_lat(obs,coarse_grid = dt,
                              uscols = 'prec',bycols = c('year','month'))

setnames(obs,'prec','obs')

pp = ver_map(obs,yy = 2022) + ggtitle('   precipitation quantile March')
pp

obs = add_tercile_cat(obs,datacol = 'obs')

obs = obs[year == 2022]

dt = merge(obs,dt,c('lon','lat'))

 dt[,c('below','normal','above') := lapply(.SD,function(x) x/100),.SDcols = c('below','normal','above') ]


rr = c(-0.5,0.5)
mbs = MBS(dt)
pp2 = ggplot_dt(mbs,'MBS',midpoint = 0,rr = rr)
pp2


igs = IGSS(dt)
pp3 = ggplot_dt(igs,'IGSS',midpoint = 0,rr = rr)
pp3


# hit scores spatially averaged:
hs = HS(dt, by = NULL, pool = c('lon','lat'))

# hit skill score map:
hss = HSS(dt)
print(hss)

ggplot_dt(hss, 'HSS', discrete_cs = T,breaks = c(-1,-0.5,0.5,1)) + ggtitle('Hit skill score')

# ROC curves and reliability diagrams:

#ROC_curve(dt) # I've tried this but something seems to be wrong with the function, got to double-check
rel_diag(dt)




