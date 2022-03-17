
# testing script for calculating scores for ensemble forecast

rm(list = ls())
library(ggplot2)
devtools::load_all() # like library(SeaVal), only more appropriate for development mode, where changes to the package are implemented

# get example data set:

fcs = ecmwf_monthly
months = unique(fcs[,month])
years = unique(fcs[,year])

obs = load_chirps(months = months,years = years)
setnames(obs,'prec','obs')

dt = merge(fcs,obs,by = c('lon','lat','year','month'))

#### test scores: ####

timer = Sys.time()
crps = CRPS(dt,f = 'prec',o = 'obs')
timer = Sys.time() - timer
timer

ggplot_dt(crps[month == 12],'CRPS')

# why are the crps-versions so slow? Maybe we need some more sohisticated benchmark...

timer = Sys.time()
mse = MSE(dt,f = 'prec',o = 'obs')
timer = Sys.time() - timer
timer

ggplot_dt(mse[month == 12],'MSE')

# check manually that it does the right thing:
dt[,fc_mean:= mean(prec),by = .(lon,lat,year,month)]
mse2 = dt[,.(MSE = mean((fc_mean - obs)^2)),by = .(lon,lat,month)]
ggplot_dt(mse2[month == 12],'MSE')

#######################
#### skill scores: ####
#######################

crpss = CRPSS(dt,f = 'prec',o = 'obs')
pp = ggplot_dt(crpss,'CRPSS',rr = c(-1,1)) + ggtitle('CRPSS')
plot(pp)

mses = MSES(dt,f = 'prec',o = 'obs')
pp = ggplot_dt(mses,'MSES',rr = c(-1,1))+ ggtitle('MSES')
plot(pp)


test = PCC(dt,f = 'prec',o = 'obs')
ggplot_dt(test,'rho')

cpa = CPA(dt,f = 'prec',o = 'obs')
ggplot_dt(cpa,'cpa')

