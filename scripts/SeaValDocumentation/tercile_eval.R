
rm(list = ls())


### this script works out all the stuff that is contained in the tercile-eval.Rmd ###
#since 

# load the postprocessing package:

library(data.table)
library(ncdf4)
devtools::load_all()
library(PostProcessing)

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/'

nc = nc_open(paste0(data_dir,'202101/PredictedRainfallProbbability-FMA2021_Jan2021.nc'))

### sort all the nc-values into an R data.table

lons = ncvar_get(nc,varid = 'lon') 
lats = ncvar_get(nc,varid = 'lat') 

dt_fc = data.table(lon = rep(lons,length(lats)),
                   lat = rep(lats,each = length(lons)),
                   below = as.vector(ncvar_get(nc,varid = 'below')),
                   normal = as.vector(ncvar_get(nc,varid = 'normal')),
                   above = as.vector(ncvar_get(nc,varid = 'above')))

# remove missing values:
dt_fc = dt_fc[!is.na(below)]

# transform probabilities from percent to probabilities between 0 and 1:
dt_fc = dt_fc[,c('below','normal','above') := lapply(.SD,function(x) x/100),.SDcols = c('below','normal','above')]

# get ERA observation data:
era_dt = load_era_monthly_data('total_precipitation',months = 2:4,years = 1980:2021,
                               lon_subset = global_confer_lon_subset(), lat_subset = global_confer_lat_subset(),
                               root_dir = claudio_sfe_dir())



get_terciles = function(dt,SDcols,
                        bycols,copy = T)
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
era_cats = era_cats[year == 2021]




rmarkdown::render('/nr/user/claudio/pkg/PostProcessing/scripts/CONFER/validation/tercile_eval.Rmd')
rmarkdown::render('/nr/user/claudio/pkg/PostProcessing/scripts/CONFER/validation/tercile_eval_pres.Rmd')
