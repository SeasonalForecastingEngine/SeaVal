
rm(list = ls())

library(PostProcessing)
library(data.table)
library(ncdf4)

devtools::load_all()

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/'


era_dt = load_era_monthly_data('total_precipitation',months = 2:4,years = 1980:2021,
                             lon_subset = global_confer_lon_subset(), lat_subset = global_confer_lat_subset(),
                             root_dir = claudio_sfe_dir())

chirps_dt = fread('/nr/project/stat/CONFER/Data/CHIRPS_prec_upscaled.csv')


get_terciles = function(dt,SDcols,bycols,copy = T)
{
  if(copy)
  {
    dt_new = copy(dt)
  } else { dt_new = dt  }
  
  terc_fct = function(x){-1*(x <= quantile(x,0.33,na.rm = T)) + 1*(x >= quantile(x,0.67,na.rm = T))}
  
  dt_new[,paste0(SDcols,'_cat') := lapply(.SD,terc_fct),.SDcols = SDcols,by = bycols]
  return(dt_new)
}


era_dt = era_dt[month %in% 2:3,.(prec = mean(total_precipitation)),by = .(year,lon,lat)]

era_cats = get_terciles(era_dt,'prec',c('lon','lat'))
era_cats = era_cats[]


plot_diagnostic(era_cats[year == 2021 ],'prec_cat')

### get example forecast data ###

nc1 = nc_open(paste0(data_dir,'Ens_Prec_1monLead_OND_ProbEnsRegrCPT-avg.nc'))
ncdf_to_dt(nc1)
test = ncvar_get(nc1,varid = 'below')


nc2 = nc_open(paste0(data_dir,'202101/PredictedRainfallProbbability-FMA2021_Jan2021.nc'))

temp = ncvar_get(nc2,varid = 'below')
lons = ncvar_get(nc2,varid = 'lon') 
lats = ncvar_get(nc2,varid = 'lat') 

dt_fc = data.table(lon = rep(lons,length(lats)),
                   lat = rep(lats,each = length(lons)),
                   below = as.vector(ncvar_get(nc2,varid = 'below')),
                   normal = as.vector(ncvar_get(nc2,varid = 'normal')),
                   above = as.vector(ncvar_get(nc2,varid = 'above')))

plot_diagnostic(dt_fc,'below')
plot_diagnostic(dt_fc,'normal')
plot_diagnostic(dt_fc,'above')

dt_fc[,c('below','normal','above') := lapply(.SD,function(x){x/100}),.SDcols = c('below','normal','above')]
dt_fc = dt_fc[! is.na(below)]

lls = dt_fc[,.(lon,lat)]

setkey(lls,lon,lat)
setkey(era_cats,lon,lat)
setkey(era_dt,lon,lat)

era_cats = era_cats[lls,]
era_cats[,mean(prec)]

plot_diagnostic(era_cats[year == 2021],'prec_cat')


dt_fc = merge(dt_fc,era_cats[year == 2021,.(lon,lat,prec_cat)],by = c('lon','lat'))

rps = function(p1_vec,p2_vec,p3_vec,obs_vec)
{
  return((obs_vec == -1)*((1-p1_vec)^2 + p3_vec^2) + (obs_vec == 0)*(p1_vec^2 + p3_vec^2) + (obs_vec == 1)*(p1_vec^2 + p2_vec^2))
}

ibs = function(p1_vec,p2_vec,p3_vec,obs_vec)
{
  return(((obs_vec == -1) - p1_vec)^2 +  ((obs_vec == 0) -  p2_vec)^2 + ((obs_vec == 1) - p3_vec)^2)
}

dt_fc[,rps := rps(below,normal,above,prec_cat)]
dt_fc[,ibs := ibs(below,normal,above,prec_cat)]
plot_diagnostic(dt_fc,'rps')
plot_diagnostic(dt_fc,'ibs')



dt_fc[,c('below_clim','normal_clim','above_clim') := 0.33]
dt_fc[,rps_clim := rps(below_clim,normal_clim,above_clim,prec_cat)]

plot_diagnostic(dt_fc[,.(lon,lat,rps - rps_clim)], rr = c(-0.33,0.33))
plot_diagnostic(dt_fc[,.(lon,lat,ibs - 2/3)], rr = c(-0.33,0.33))
plot_diagnostic(dt_fc, 'prec_cat')

plot_diagnostic(dt_fc,'below')
plot_diagnostic(dt_fc,'normal')
plot_diagnostic(dt_fc,'above')

plot_dir = '/nr/project/stat/CONFER/plots/Examples_RPS_vs_IBS/'

dir.create(plot_dir)

pdf(paste0(plot_dir,'rps.pdf'))
plot_diagnostic(dt_fc,'rps')
dev.off()

pdf(paste0(plot_dir,'ibs.pdf'))
plot_diagnostic(dt_fc,'ibs')
dev.off()

pdf(paste0(plot_dir,'rps_minus_clim.pdf'))
plot_diagnostic(dt_fc[,.(lon,lat,rps - rps_clim)], rr = c(-0.33,0.33),mn = 'rps - rps_clim')
dev.off()

pdf(paste0(plot_dir,'ibs_minus_clim.pdf'))
plot_diagnostic(dt_fc[,.(lon,lat,ibs - 2/3)], rr = c(-0.33,0.33),mn = 'ibs - ibs_clim')
dev.off()

pdf(paste0(plot_dir,'below.pdf'))
plot_diagnostic(dt_fc,'below',rr = c(0,0.67))
dev.off()

pdf(paste0(plot_dir,'normal.pdf'))
plot_diagnostic(dt_fc,'normal',rr = c(0,0.67))
dev.off()

pdf(paste0(plot_dir,'above.pdf'))
plot_diagnostic(dt_fc,'above',rr = c(0,0.67))
dev.off()

pdf(paste0(plot_dir,'prec_cat.pdf'))
plot_diagnostic(dt_fc,'prec_cat')
dev.off()

