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
          discrete_cs = TRUE, binwidth = 0.4, # discretize colorbar
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

fn_pred1 = "CrossValidatedPredictedRain_Feb-Apr_Feb2021.nc"
fn_pred2 = "CrossValidatedPredictedRain_Mar-May_Feb2021.nc"

dt_pred1 = netcdf_to_dt(paste0(data_dir,fn_pred1),verbose = 0) # they look the same, we can just look at the information from one of them...
dt_pred2 = netcdf_to_dt(paste0(data_dir,fn_pred2))

# add a column, identifying which is which:
dt_pred1[,season:= 'FMA']
dt_pred2[,season:= 'MAM']

# bind together
dt_pred = rbindlist(list(dt_pred1,dt_pred2))
print(dt_pred)

# get observations:

fn_obs1 = "ObservedRain_Feb-Apr_Feb2021.nc"
fn_obs2 = "ObservedRain_Mar-May_Feb2021_update.nc"
dt_obs1 = netcdf_to_dt(paste0(data_dir,fn_obs1),verbose = 0)
dt_obs2 = netcdf_to_dt(paste0(data_dir,fn_obs2),verbose = 1)

dt_obs1[,season := 'FMA']
dt_obs2[,season := 'MAM']
dt_obs = rbindlist(list(dt_obs1,dt_obs2))

# merge predictions and observations into the same data table:
setnames(dt_pred,'prec','prediction')
setnames(dt_obs,'prec','observation')

dt = merge(dt_pred,dt_obs,by = c('lon','lat','time','season'))
print(dt)

# remove all rows with missing predictions:
dt = dt[!is.na(prediction)]

# convert time from the 'months since date' (MSD) format to years and months (YM)
dt = MSD_to_YM(dt,origin = '1981-01-01') # the origin was documented in the netcdf, see above.
print(dt) 

### check out local biases ###
bias_dt = dt[,.(bias = mean(prediction - observation)), by = .(lon,lat,season)] # grouping by lon,lat, and season means that the mean is taken over all years.
bias_dt[,range(bias)] # get an idea of the range

pp1 = ggplot_dt(bias_dt[season == 'FMA'],
                data_col = 'bias', 
                rr = c(-15,15), # fix range to make it comparable to pp2
                mn = 'bias of FMA prediction',
                midpoint = 0)

pp2 = ggplot_dt(bias_dt[season == 'MAM'],
                data_col = 'bias', 
                rr = c(-15,15),
                mn = 'bias of MAM prediction',
                midpoint = 0)

# show plots:
ggarrange(pp1,pp2)

### analyze mean square error skill scores ###
msess = MSESS_dt(dt,
                 fc_col = 'prediction', 
                 obs_col = 'observation',
                 by_cols = c('lon','lat','season')) # the skill scores should be computed for each location and each season separately

# get useful range:
msess[,range(MSESS)]
rr = c(-0.35,0.35)

pp1 = ggplot_dt(msess[season == 'FMA'], 
                data_col = 'MSESS', 
                rr=rr,
                mn = 'MSE skill score, FMA')

pp2 = ggplot_dt(msess[season == 'MAM'], 
                data_col = 'MSESS', 
                rr=rr,
                mn = 'MSE skill score, MAM')

ggarrange(pp1,pp2)


# check out average MSEs and MSESSs per country:
msess = add_country_names(msess)

msess_by_country = msess[,.(MSE = mean(MSE),
                            MSESS = mean(MSESS)), by = country]

print(msess_by_country)

# ACC missing !!!!!!!!!!!!!!!

##### Ens_Prec_1monLead_MAM_Prob_EnsRegrCPT-avg.nc #####

fn = 'Ens_Prec_1monLead_MAM_Prob_EnsRegrCPT-avg.nc'

dt = netcdf_to_dt(paste0(data_dir,fn))
dt = dt[!is.na(below) | !is.na(normal) | !is.na (above)]

p1 = ggplot_dt(dt,data_col = 'below', midpoint = dt[,min(below,na.rm = TRUE)])
p2 = ggplot_dt(dt,data_col = 'normal', midpoint = dt[,min(normal,na.rm = TRUE)], high = 'darkgoldenrod') # see https://www.r-graph-gallery.com/ggplot2-color.html for an overview of color names.
p3 = ggplot_dt(dt,data_col = 'above', midpoint = dt[,min(above,na.rm = TRUE)], high = 'darkgreen') # see https://www.r-graph-gallery.com/ggplot2-color.html for an overview of color names.

ggarrange(p1,p2,p3,ncol = 3)

##### "Ens_ProbExceedance_Mar-May_Feb2021.nc" #####

fn = "Ens_ProbExceedance_Mar-May_Feb2021.nc" 
dt = netcdf_to_dt(paste0(data_dir,fn))

# get observations:

fn = "PredictedRain_Feb-Apr_Feb2021.nc" 
dt = netcdf_to_dt(paste0(data_dir,fn))

ggplot_dt(dt)

##### "PredictedProbabilityRain_Feb-Apr_Feb2021.nc" #####

fn = "PredictedProbabilityRain_Feb-Apr_Feb2021.nc"
dt = netcdf_to_dt(paste0(data_dir,fn),trymerge = FALSE)

fn = "PredictedProbabilityRain_Mar-May_Feb2021_edit.nc"
dt = netcdf_to_dt(paste0(data_dir,fn))

fn = "PredictedProbabilityRain_Mar-May_Feb2021_new.nc"
dt = netcdf_to_dt(paste0(data_dir,fn))

dt[,normal := normal/100][,above := above/100][,below := below/100]

# get observation:

fn = "ObservedRain_Mar-May_Feb2021_update.nc"  
dt_obs = netcdf_to_dt(paste0(data_dir,fn))

dt_obs = MSD_to_YM(dt_obs)
dt_obs = add_tercile_cat(dt_obs,'prec')

fn = "ObservedRain_Mar-May_Feb2021.nc"
dt_new = netcdf_to_dt(paste0(data_dir,fn))

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

for(leadtime in 1:3)
{
  for(mod in 1:5)
  {
    
      plot_list = c(plot_list,list(ggplot_dt(dt[model == mod & lead == leadtime],
                                        'corr',
                                        rr = c(-1,1),
                                        mn = paste0('model = ',mod,', lead time ',leadtime),
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

##################### Exceedence probs ###############

fn = fns[6]

dt = netcdf_to_dt(paste0(data_dir,fn))
dt

fn = fns[11]

dt = netcdf_to_dt(paste0(data_dir,fn))
dt

