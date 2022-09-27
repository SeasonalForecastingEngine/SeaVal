
rm(list = ls())

library(SeaVal)

download_chirps_monthly()
download_chirps_monthly(version = 'ICPAC')

ucsb = load_chirps(version = 'UCSB')
icpac = load_chirps(version = 'ICPAC')


#compare:
setnames(ucsb,'prec','chirps')
setnames(icpac,'prec','chirps_blended')

dt = merge(ucsb,icpac,by = c('lon','lat','year','month'))

dt_bias = dt[,.(bias = mean(chirps - chirps_blended)),by = .(lon,lat,month)]

plist = list()
for(mm in 1:12)
{
  pp = ggplot_dt(dt_bias[month == mm],
                 discrete_cs = T,binwidth = 0.2,midpoint = 0,
                 mn = paste0('month = ',mm),
                 rr = c(-2,2))
  plist = c(plist,list(pp))
}

ggpubr::ggarrange(plotlist = plist,ncol = 4,nrow = 3)

#########################################


