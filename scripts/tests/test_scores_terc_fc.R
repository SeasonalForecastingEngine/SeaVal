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

rps = RPS(dt_tc)

ggplot_dt(rps[month == 10],'RPS')

rpss = RPSS(dt_tc,by=c("lon","lat"),pool=c("year","month"))

ggplot_dt(rpss,'RPSS',midpoint=0)

hs = HS(dt_tc)
ggplot_dt(hs[month == 10],'HS1',midpoint=0.33)
ggplot_dt(hs[month == 10],'HS2',midpoint=0.33)
ggplot_dt(hs[month == 10],'HS3',midpoint=0.33)

hss = HSS(dt_tc)

ggplot_dt(hss[month == 10],'HSS',midpoint=0)

# Arrange forecasts so that no category gets a probability of exactly 0 (but 0.01 instead)
dt_tc2 <- copy(dt_tc)
dt_tc2 <- dt_tc2[below==0, ':=' (below=0.01, normal=normal-c(0.01,0)[max.col(dt_tc2[below==0,c("normal","above")])],
                                above=above-c(0,0.01)[max.col(dt_tc2[below==0,c("normal","above")])])]
dt_tc2 <- dt_tc2[normal==0,':=' (normal=0.01, below=below-c(0.01,0)[max.col(dt_tc2[normal==0,c("below","above")])],
                                 above=above-c(0,0.01)[max.col(dt_tc2[normal==0,c("below","above")])])]
dt_tc2 <- dt_tc2[above==0,':=' (above=0.01, normal=normal-c(0.01,0)[max.col(dt_tc2[above==0,c("normal","below")])],
                                below=below-c(0,0.01)[max.col(dt_tc2[above==0,c("normal","below")])])]

igs = IGS(dt_tc2)

ggplot_dt(igs[month == 10],'IGS')

igss = IGSS(dt_tc2,by=c("lon","lat"),pool=c("year","month"))

ggplot_dt(igss,'IGSS',midpoint=0)

eir = EIR(dt_tc2,by=c("lon","lat"),pool=c("year","month"))

ggplot_dt(eir,'EIR',midpoint=0)
