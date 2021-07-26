fns = list.files(data_dir,pattern = '*.nc')

fn = 'PrecRegPeXcd_3monthSeasonal.nc'

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202102/'

dt = netcdf_to_dt(paste0(data_dir,fn))
dt

modelnames = c('GEM-NEMO','CanCM4i','NASA-GEOSS2S','GFDL-SPEAR','COLA-RSMAS-CCSM4','NCEP-CFSv2','ECMWF','Meteo_France','UKMO')
thresholds = c(200,300,350,400)

dt[,model := modelnames[model + 1]][,rthr := thresholds[rthr + 1]]

dt[,month :=lead + 2][,lead:=NULL]


dt_obs = PostProcessing::load_chirps()

dt_obs = dt_obs[year == 2021 & month >=2]
dt_obs[,prec:=30*prec]

dt = merge(dt,dt_obs[,.(lon,lat,month,prec)],by = c('lon','lat','month'))

ggplot_dt(dt[month == 2 & model == 'GEM-NEMO' & rthr == 200],'pexcd')
ggplot_dt(dt[month == 2 & model == 'GEM-NEMO' & rthr == 200],'prec')

dt[,pexcd := pexcd/100]

#' Calculate exceedence score 
#'
#' @param fc_dt Data table containing the predictions.
#' @param fc_col column name of the prediction. Contains predicted probabilities of exceedence
#' @param threshold_col which column contains the exceedence threshold?
#' @param obs_col column name of the observations, that is 
#' @param by_cols column names of grouping variables, all of which need to be columns in fc_dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in fc_dt.
#' @export

ES_dt = function(fc_dt,fc_col,threshold_col,
                  obs_col = 'obs',
                  by_cols = intersect(c('month','season','lon','lat','system','lead_time'),names(fc_dt)),
                  along_cols = c('year'))
{
  
  if(is.logical(fc_dt[,get(obs_col)]))
  {
    ES_dt = fc_dt_new[,.(ES = get(fc_col)^2 - 2*get(fc_col) * get(obs_col)),by = c(by_cols,threshold_col)]# the by-arguments are for keeping these columns only
  }
  if(!is.logical(fc_dt[,get(obs_col)]))
  {
    ES_dt = fc_dt_new[,.(ES = get(fc_col)^2 - 2*get(fc_col) * (get(obs_col) > get(threshold_col))),by = c(by_cols,threshold_col)]
  }
  return(ES_dt)
}


#' Calculate exceedence skill score 
#'
#' @param fc_dt Data table containing the predictions.
#' @param fc_col column name of the prediction. Contains predicted probabilities of exceedence
#' @param threshold_col which column contains the exceedence threshold?
#' @param obs_col column name of the observations, that is 
#' @param by_cols column names of grouping variables, all of which need to be columns in fc_dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in fc_dt.
#' @export

ESS_dt = function(fc_dt,fc_col,threshold_col,
                 obs_col = 'obs',
                 by_cols = intersect(c('month','season','lon','lat','system','lead_time'),names(fc_dt)),
                 along_cols = c('year'))
{
  
  obs_dt = unique(fc_dt[,.SD,.SDcols = intersect(c(obs_col,by_cols,along_cols),c('year','month','season','lon','lat',obs_col))])
  # note that by_cols can contain e.g. different systems, all of which are compared to the same observation, therefore the intersect.
  
  
  obs_by_cols = intersect(by_cols,names(obs_dt))
  
  climatology_prediction = climatology_ens_forecast(obs_dt = obs_dt,
                                                    by_cols = obs_by_cols)
  
  
  
  if(is.logical(fc_dt[,get(obs_col)]))
  {
    ES_dt = fc_dt_new[,.(ES = get(fc_col)^2 - 2*get(fc_col) * get(obs_col)),by = c(by_cols,threshold_col)]# the by-arguments are for keeping these columns only
  }
  if(!is.logical(fc_dt[,get(obs_col)]))
  {
    ES_dt = fc_dt_new[,.(ES = get(fc_col)^2 - 2*get(fc_col) * (get(obs_col) > get(threshold_col))),by = c(by_cols,threshold_col)]
  }
  return(ES_dt)
}



ES_dt = ES_dt(dt,'pexcd','rthr',obs_col = 'prec',by_cols = c('model','month','lon','lat'))

plot_list = list()

for(mod in unique(ES_dt[,model]))
{
  plot_list = c(plot_list,list(ggplot_dt(ES_dt[model == mod& month == 3 & rthr == 300],'ES',mn = mod, high = 'red',midpoint = 0,rr= c(-1,1))))
  
}

ggpubr::ggarrange(plotlist = plot_list,ncol = 3,nrow = 3)
