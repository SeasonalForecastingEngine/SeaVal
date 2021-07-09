rm(list = ls())

library(ForecastTools)
library(SeaVal)
library(ggpubr)


source('~/pkg/ForecastTools/R/ncdf_to_dt.R')
source('~/pkg/ForecastTools/R/plotting.R')
data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202102/'

fns = list.files(path = data_dir,pattern = '*.nc')
  

for(nc in fns[-c(12,15)])
{

  print(nc)
  test = netcdf_to_dt(paste0(data_dir,nc),printunits = FALSE)

}


##### CorrelationSkillRain_Feb-Apr_Feb2021.nc #####

fn = "CorrelationSkillRain_Feb-Apr_Feb2021.nc"

dt = netcdf_to_dt(paste0(data_dir,fn))
dt = netcdf_to_dt(paste0(data_dir,fn),print_nc = FALSE)

ggplot_dt(dt,
          mn = 'Corr. skill rain Feb-Apr, Feb initialized', # title
          rr = c(-1,1), # range of the colorbar
          discrete_cs = TRUE,binwidth = 0.4, # discretize colorbar
          guide = guide_colorbar(barwidth = 0.5, barheight = 10)) # make colorbar longer


##### CorrelationSkillRain_Mar-May_Feb2021.nc #####

fn = "CorrelationSkillRain_Mar-May_Feb2021.nc"

dt = netcdf_to_dt(paste0(data_dir,fn),print_nc = FALSE)

ggplot_dt(dt,
          mn = 'Corr. skill rain Mar-May, Mar initialized', # title
          rr = c(-1,1), # range of the colorbar
          discrete_cs = TRUE,binwidth = 0.4, # discretize colorbar
          guide = guide_colorbar(barwidth = 0.5, barheight = 10)) # make colorbar longer


##### CrossValidatedPredictedRain_Feb-Apr_Feb2021.nc #####

fn_pred = "CrossValidatedPredictedRain_Feb-Apr_Feb2021.nc"

dt_pred = netcdf_to_dt(paste0(data_dir,fn_pred))

# get observations:

fn_obs = "ObservedRain_Feb-Apr_Feb2021.nc"

dt_obs = netcdf_to_dt(paste0(data_dir,fn_obs))

# add prediction to dt_obs:
dt_obs[, prediction := dt_pred[,prec]]

# convert time from the 'months since date' (MSD) format to years and months (YM)
dt = MSD_to_YM(dt_obs)

# kill missing values:
dt = dt[!is.na(prec)][!is.na(prediction)]

# check out local biases 
bias_dt = dt[,.(bias = mean(prediction - prec)), by = .(lon,lat)]
ggplot_dt(bias_dt, midpoint = 0)

msess = MSESS_dt(dt,fc_col = 'prediction', obs_col = 'prec')
ggplot_dt(msess, data_col = 'MSESS', midpoint = 0)

# check out average MSEs and MSESSs per country:
msess = add_country_names(msess)

msess_by_country = msess[,.(MSE = mean(MSE),
                            MSESS = mean(MSESS)), by = country]

print(msess_by_country)

# ACC missing !!!!!!!!!!!!!!!

##### PrecRegPeXcd #####

fn = "PrecRegPeXcd_3monthSeasonal.nc"

dt = netcdf_to_dt(paste0(data_dir,fn))
print(dt)

# Change the model and rthr column to more meaningful values:
models = c('GEM-NEMO','CanCM4i','NASA-GEOSS2S','GFDL-SPEAR','COLA-RSMAS-CCSM4','NCEP-CFSv2','ECMWF','Meteo_France','UKMO')
dt[,model := models[model + 1]]   # the model value in the netcdf ranges from 0 to 8. Assuming they are ordered as in the models vector (which is taken from the netcdf description),
                                  # model + 1 is the index where the models vector stores the corresponding model.
exceedence_thresholds = c(200,300,350,400) # also taken from netcdf description
dt[,rthr := exceedence_thresholds[rthr + 1]]

# kill missing values
dt = dt[!is.na(pexcd)]


##### TrefEnsRegr_monthly.nc #####

fn = 'TrefEnsRegr_monthly.nc'

dt = netcdf_to_dt(paste0(data_dir,fn))

# plot correlations of predictions for all five models at all lead_times:
# create list of plots:
plot_list = list()
for(mod in 1:5)
{
  for(leadtime in 1:3)
  {
      plot_list = c(plot_list,list(ggplot_dt(dt[model == mod & lead == leadtime],
                                        'corr',
                                        rr = c(-1,1),
                                        mn = paste0('model = ',ii,', lead time ',leadtime),
                                        discrete_cs = TRUE,
                                        binwidth = 0.2,
                                        guide = guide_colorbar(title = NULL, barwidth = 75, direction = 'horizontal')))) # adjust the legend/colorbar.
  }
}


#plot as grid:
do.call('ggarrange', c(plot_list,ncol = 5,nrow = 3,common.legend = TRUE,legend = 'bottom'))


################################

fn = fns[2]

dt = netcdf_to_dt(paste0(data_dir,fn))

# plot correlations of predictions for all five models at all lead_times:
# create list of plots:
plot_list = list()
for(mod in 1:5)
{
  for(leadtime in 1:3)
  {
    plot_list = c(plot_list,list(ggplot_dt(dt[model == mod & lead == leadtime],
                                           'corr',
                                           rr = c(-1,1),
                                           mn = paste0('model = ',ii,', lead time ',leadtime),
                                           discrete_cs = TRUE,
                                           binwidth = 0.2,
                                           guide = guide_colorbar(title = NULL, barwidth = 75, direction = 'horizontal')))) # adjust the legend/colorbar.
  }
}

library(ggpubr)

#plot as grid:
do.call('ggarrange', c(plot_list,ncol = 5, nrow = 3, common.legend = TRUE, legend = 'bottom'))
