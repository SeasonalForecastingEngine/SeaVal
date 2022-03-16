################################ test scores for tercile forecasts #############################

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

# get rid of ensemble forecast structure:
dt_tc = unique(dt[,.(lon,lat,year,month,below,normal,above,obs)])


# set up for tercile forecast evaluation:
add_tercile_cat(dt_tc,'obs',
                by = c('lon','lat','month'))

mbss = MBSS(dt_tc)

ggplot_dt(mbss[month == 10],'MBSS')
