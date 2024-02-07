
#############################################################
############# Ensemble Forecast Scores ######################
#############################################################
# data tables with ensemble forecasts contain forecasts for real-valued observations.
# They may contain a column 'member', and can contain multiple predictions for each space-time coordinate, namely one prediction per member.
# They are not required to contain multiple members (!), so if you have a data table that contains a single prediction (e.g. the mean of an ensemble),
# you can compute the squared error by using MSE functions.
# The typical data table dropped into these functions should have (some of) the following columns:
# lon   lat   month   year    member    system    lead_time   fc    obs
# There are 5 different sets of columns that require different treatment: grouping variables, pool-variables, the member-column, forecasts and observations.
# - grouping variables generally provide the levels for which you want to evaluate the score separately. E.g. if these contain lon,lat, and month,
#   you'll get an average score for each month and grid point.
# - pool-variables tell you pool which columns is averaged. So if you calculate the MSE and your pool-variable is just year, then the MSE is averaged pool all years in the
#   data table. However, if the pool-variables are year and location, then the MSE is averaged over all years and locations.
# - the optional member column specifies the ensemble member.
# It is absolutely crucial that the data table unique(dt[,.(grouping variables, pool-variables,member column)]) has the same number of entries as the original data.table!!!
# Otherwise, your data contains multiple forecasts/observations for the same coordinate, making it impossible to calculate a score.


#' Coefficients of Predictive Ability
#'
#' @description Function for calculating coefficients of predictive ability (CPAs) of ensemble mean forecasts stored in long data tables:#'
#' Can also handle point forecasts.
#' Warning: This metric always needs several years of data since the ranks on which it is based are calculated across multi-year samples.
#'
#' @param dt Data table containing the predictions.
#' @param f column name of the prediction.
#' @param o column name of the observations.
#' @param by column names of grouping variables, all of which need to be columns in dt. A separate CPA is computed for each value of the grouping variables.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param mem Number of column containing the number of the ensemble member.
#' @param pool column name(s) for the variable(s) along which is averaged. Needs to contain 'year' per warning above.
#' @param dim.check Logical. If True, a simple test whether the dimensions match up is conducted:
#' The data table should only have one row for each level of c(by,pool,mem)
#'
#'@return A data table with the scores
#'
#'@examples
#' dt = data.table(fc = 1:4,obs = c(4,4,7,7),member = c(1,2,1,2),year = c(1999,1999,2000,2000))
#' CPA(dt,f = 'fc')
#' @export
#' @importFrom stats cov na.omit

CPA = function(dt, f, o = 'obs',
               by = by_cols_ens_fc_score(dt),
               pool = 'year',
               mem = 'member',
               dim.check = TRUE)
{
  # for devtools::check:
  fc_mean = fc_midrank = obs_class = obs_midrank = NULL

  by = intersect(by,names(dt))
  pool = intersect(pool,names(dt))
  mem = intersect(mem,names(dt))

  checks_ens_fc_score()

  # get forecast mean (= mean over all ensemble members)
  dt[, fc_mean:=mean(get(f), na.rm=T), by=c(by,pool)]

  # combine data into a single DT and remove rows with missing data
  dt = stats::na.omit(dt[, .SD, .SDcols=c("fc_mean",o,by,pool)])

  # calculate the CPA
  dt[, fc_midrank:=frank(fc_mean,ties.method="average"), by=by]
  dt[, obs_class:=frank(get(o),ties.method="dense"), by=by]
  dt[, obs_midrank:=frank(get(o),ties.method="average"), by=by]
  CPA_dt = dt[, .(cpa=0.5*(1.+stats::cov(obs_class,fc_midrank)/stats::cov(obs_class,obs_midrank))), by=by]

  return(CPA_dt)
}


#' Continuous Ranked Probability Score
#'
#' @description Taking CRPSs of ensemble forecasts stored in long data tables:
#'
#' @param dt Data table containing predictions and observations.
#' @param f column name of the forecasts. May not be called `'f'`
#' @param o column name of the observations.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) over which is averaged. Typically just 'year'.
#' @param mem Name of the column identifying the ensemble member.
#' @param dim.check Logical. If True, a simple test whether the dimensions match up is conducted:
#' The data table should only have one row for each level of c(by,pool,mem)
#' @param ens_size_correction logical. If TRUE, the CRPS is corrected for sample size (see Ferro et al. 2008: 'On the effect of ensemble size on the discrete and continuous
#' ranked probability scores'). This is slower, but you should do it if you compare ensembles of different size.
#'
#'@return A data table with the scores
#'
#'@examples
#' dt = data.table(fc = 1:4,obs = c(4,4,7,7),member = c(1,2,1,2),year = c(1999,1999,2000,2000))
#' CRPS(dt,f = 'fc')
#'
#'
#' @export

CRPS = function(dt, f, o = "obs",
                by = by_cols_ens_fc_score(),
                pool = "year",
                mem = "member",
                dim.check = TRUE,
                ens_size_correction = FALSE)
{
  by = intersect(by, names(dt))
  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]
  checks_ens_fc_score()

  if(!ens_size_correction)  ret_dt = dt[,.(CRPS = crps_aux(get(o),get(f))),by = c(by,pool)]
  if(ens_size_correction)  ret_dt = dt[,.(CRPS = crps_aux_esc(get(o),get(f))),by = c(by,pool)]

  ret_dt = ret_dt[, .(CRPS = mean(CRPS)), by = by]
  return(ret_dt)
}

#' Auxiliary function for calculating crps.
#'
#' @description Mostly copy-paste from `scoringRules:::crps_edf`. Adjusted to the data table format, where the observation is a vector of the same length as the ensemble forecast,
#' but is just repeated (which is why only `y[1]`) is used.
#' @param y vector of length m with m identical entries, the observation
#' @param dat vector of length m containing the m ensemble forecasts

crps_aux = function(y,dat)
{

  c_1n <- 1/length(dat)
  x <- sort(dat)
  a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
  ret <- 2 * c_1n * sum(((y[1] < x) - a) * (x - y[1]))
  return(ret)
}

#' Auxiliary function for calculating crps with ensemble size correction by Ferro et al. 2008.
#'
#' @description Mostly copy-paste from `scoringRules::crps_edf`. Adjusted to the data table format, where the observation is a vector of the same length as the ensemble forecast,
#' but is just repeated (which is why only `y[1]`) is used.
#' @param y vector of length m with m identical entries, the observation
#' @param dat vector of length m containing the m ensemble forecasts

crps_aux_esc = function(y,dat)
{
  c_1n <- 1/length(dat)
  x <- sort(dat)
  a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
  ret <- 2 * c_1n * sum(((y[1] < x) - a) * (x - y[1]))

  #ensemble size correction:
  ens_size = length(dat)
  mean_dist_xx = mean(stats::dist(dat))
  ret = ret - mean_dist_xx/(2*ens_size)
  return(ret)
}


#' Continuous Ranked Probability Skill Score
#'
#' @description Function for taking CRPS skill scores of ensemble forecasts stored in long data tables.
#' The skill score needs a climatological forecast as reference. This is so far always based on the leave-one-year-out climatology.
#'
#' @param dt Data table containing predictions and observations.
#' @param f column name of the prediction.
#' @param o column name of the observations.
#' @param by column names of grouping variables, all of which need to be columns in dt. A separate CRPS is computed for each value of the grouping variables.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged. Needs to contain 'year' since the reference climatology forecast is leave-one-year-out.
#' @param ... passed on to CRPS_ens_fc, in particular mem and dim.check
#'
#'@return A data table with the scores
#'
#'@examples
#' dt = data.table(fc = 1:4,obs = c(4,4,7,7),member = c(1,2,1,2),year = c(1999,1999,2000,2000))
#' CRPSS(dt,f = 'fc')
#'
#'
#' @export

CRPSS = function(dt,f,
                 o = 'obs',
                 by = by_cols_ens_fc_score(),
                 pool = c('year'),...)
{
  # for devtools::check:
  clim_CRPS = NULL

  by = intersect(by,names(dt))

  if(!('year' %in% pool)) stop('skill scores are with respect to leave-one-year-out climatology, so the pool-argument must contain "year".')

  # get climatological loyo-prediction
  obs_dt = unique(dt[,.SD,.SDcols = c(o,obs_dimvars(dt))])
  obs_by = intersect(by,obs_dimvars(dt))
  climatology_prediction = climatology_ens_forecast(obs_dt = obs_dt,
                                                    by = obs_by)

  setnames(climatology_prediction,'obs','clim')

  climatology_prediction = merge(climatology_prediction,obs_dt,by = c(obs_by,'year'))

  climatology_CRPS = CRPS(dt = climatology_prediction,
                                 f = 'clim',
                                 o = o,
                                 by = obs_by)

  setnames(climatology_CRPS,'CRPS','clim_CRPS')

  CRPS_dt = CRPS(dt = dt,
                        f = f,
                        o = o,
                        by = by,
                        pool = pool,...)

  if(length(by) >0) CRPS_dt = merge(CRPS_dt,climatology_CRPS,by = by) else CRPS_dt = cbind(CRPS_dt,climatology_CRPS)
  CRPS_dt[,CRPSS := (clim_CRPS - CRPS)/clim_CRPS][]

  return(CRPS_dt)

}


#' Mean Square Error of ensemble forecasts.
#'
#' Derives the MSE of ensemble forecasts stored in long data tables. Can also handle point forecast.
#'
#' @param dt Data table containing the predictions.
#' @param f column name of the prediction.
#' @param o column name of the observations.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param mem Name of the column identifying the ensemble member. Only used if check_dimension is TRUE. Is NULL for a point forecast.
#' @param dim.check Logical. If True, a simple test whether the dimensions match up is conducted:
#' The data table should only have one row for each level of c(by,pool,mem)
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(fc = 1:4,obs = c(4,4,7,7),member = c(1,2,1,2),year = c(1999,1999,2000,2000))
#' MSE(dt,f = 'fc')
#' @export

MSE = function(dt,
               f, o = 'obs',
               by = by_cols_ens_fc_score(),
               pool = 'year',
               mem = 'member',
               dim.check = TRUE)
{
  # for devtools::check:
  fc_mean = NULL

  by = intersect(by,names(dt))
  pool = intersect(pool,names(dt))
  mem = intersect(mem,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  #checks:
  checks_ens_fc_score()


  # get forecast mean (= mean over all ensemble members)
  dt_new = copy(dt)[,fc_mean := mean(unlist(.SD),na.rm = T),.SDcols = f,by = c(by,pool)]
  #IMPORTANT: in the line above, why do we use .SD and .SDcols rather than simply get(f)?
  # using mean(get(f)) usually works, BUT it doesn't when the forecast column is in fact called 'f',
  # so the function would crash when its called with f = 'f'. This is not so important for the forecast column,
  # but very important for the obs column, which is called 'obs' as default!

  # take MSE:
  MSE_dt = dt_new[,.(MSE = mean((fc_mean - unlist(.SD))^2,na.rm = T)),.SDcols = o,by = by]
  return(MSE_dt)
}


#' Mean Square Error Skill score
#'
#' @description Function for taking MSE skill scores of ensemble forecasts stored in long data tables.
#' Can also handle point forecasts.
#' The skill score needs a climatological forecast as reference. This is so far always based on the leave-one-year-out climatology.
#'
#' @param dt Data table containing the predictions.
#' @param f column name of the prediction.
#' @param o column name of the observations.
#' @param by column names of grouping variables, all of which need to be columns in dt. A separate MSE is computed for each value of the grouping variables.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged. Needs to contain 'year' since the reference climatology forecast is leave-one-year-out.
#' @param ... passed on to MSE
#'
#'@return A data table with the scores
#'
#'@examples
#' dt = data.table(fc = 1:4,obs = c(4,4,7,7),member = c(1,2,1,2),year = c(1999,1999,2000,2000))
#' MSES(dt,f = 'fc')
#'
#' @export

MSES = function(dt,f,
                o = 'obs',
                by = by_cols_ens_fc_score(),
                pool = c('year'),...)
{
  # for devtools::check:
  clim_MSE = NULL

  by = intersect(by,names(dt))
  if(!('year' %in% pool)) stop('skill scores are with respect to leave-one-year-out climatology, so your pool must contain "year".')

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  # get climatological loyo-prediction
  obs_dt = unique(dt[,.SD,.SDcols = c(o,obs_dimvars(dt))])
  obs_by = intersect(by,obs_dimvars(dt))
  climatology_prediction = climatology_ens_forecast(obs_dt = obs_dt,
                                                    by = obs_by)
  setnames(climatology_prediction,o,'clim')

  climatology_prediction = merge(climatology_prediction,obs_dt,by = c(obs_by,'year'))

  climatology_MSE = MSE(dt = climatology_prediction,
                           f = 'clim',
                           o = o,
                           by = obs_by)

  setnames(climatology_MSE,'MSE','clim_MSE')

  MSE_dt = MSE(dt = dt,
                  f = f,
                  o = o,
                  by = by,
                  pool = pool,...)

  if(length(intersect(names(MSE_dt),names(climatology_MSE)))>0) {
    MSE_dt = merge(MSE_dt,climatology_MSE, by = intersect(names(MSE_dt),names(climatology_MSE)))
  } else MSE_dt = cbind(MSE_dt,climatology_MSE)
  MSE_dt[,MSES := (clim_MSE - MSE)/clim_MSE][]

  return(MSE_dt)
}



#' Pearson Correlation Coefficient
#'
#' @description Function for calculating Pearson correlation coefficients (PCCs) of ensemble mean forecasts stored in long data tables.
#' Can also handle point forecasts.
#' This metric always needs several years of data since the means and standard deviations are calculated across time.
#'
#' @param dt Data table containing the predictions.
#' @param f column name of the prediction.
#' @param o column name of the observations.
#' @param by column names of grouping variables, all of which need to be columns in dt. A separate PCC is computed for each value of the grouping variables.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged. Needs to contain 'year' per warning above.
#' @param mem Name of the column identifying the ensemble member. Only used if check_dimension is TRUE. Is NULL for a point forecast.
#' @param dim.check Logical. If True, a simple test whether the dimensions match up is conducted:
#' The data table should only have one row for each level of c(by,pool,mem)
#'
#'@return A data table with the scores
#'
#'@examples
#' dt = data.table(fc = 1:4,obs = c(4,4,7,7),member = c(1,2,1,2),year = c(1999,1999,2000,2000))
#' PCC(dt,f = 'fc')
#'
#' @export
#' @importFrom stats cor

PCC = function(dt, f,
               o = 'obs',
               by = by_cols_ens_fc_score(dt),
               pool = 'year',
               mem = 'member',
               dim.check = TRUE)
{
  # for devtools::check:
  fc_mean = NULL

  by = intersect(by,names(dt))
  pool = intersect(pool,names(dt))
  mem = intersect(mem,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  # checks:
  checks_ens_fc_score()


  # get forecast mean (= mean over all ensemble members)
  dt[, fc_mean:=mean(get(f),na.rm = T), by=c(by,pool)]

  # calculate correlation coefficient
  dt = dt[, .SD, .SDcols=c("fc_mean",o,by,pool)]
  PCC_dt = dt[, .(rho=stats::cor(fc_mean,get(o),use="na.or.complete")), by=by]

  return(PCC_dt)
}



############################################
####### Scores for tercile forecasts #######
############################################



#######################

#' Hit score
#'
#' @description This score is suitable for tercile category forecasts. This score is the frequency at which the highest probability category actually
#' happens. The function also provides the frequency at which the second-highest probability category, and lowest probability category,
#' actually happens.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in `dt` if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' HS(dt)
#' @export

HS = function(dt,f = c('below','normal','above'),
              o = tc_cols(dt),
              by = by_cols_terc_fc_score(),
              pool = 'year',
              dim.check = TRUE)
{
  # for devtools::check:
  HS_min = HS_mid = HS_max = max_cat = min_cat = hit = hit3 = tercile_cat = NULL

  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Hit score:
  ddt = copy(dt)
  ddt = ddt[,max_cat:=c(-1,0,1)[max.col(ddt[,mget(f)],"first")]] #not breaking ties at random
  ddt = ddt[,min_cat:=c(-1,0,1)[max.col(-1*ddt[,mget(f)],"first")]]
  ddt = ddt[,hit:=as.numeric(get(o)==max_cat)]
  ddt = ddt[,hit3:=as.numeric(get(o)==min_cat)]
  ddt[get(f[1])==get(f[3]) & get(f[1])==get(f[2]),':=' (hit=1/3, hit3=1/3)] #tie between b,n,a
  ddt[tercile_cat==-1 & get(f[1])==get(f[2]) & get(f[2])>get(f[3]) ,':=' (hit=1/2, hit3=0)] #tie between b, n
  ddt[tercile_cat==0 & get(f[1])==get(f[2]) & get(f[2])>get(f[3]) ,':=' (hit=1/2, hit3=0)] #tie between b, n
  ddt[tercile_cat==0 & get(f[2])==get(f[3]) & get(f[2])>get(f[1]) ,':=' (hit=1/2, hit3=0)] #tie between n, a
  ddt[tercile_cat==1 & get(f[2])==get(f[3]) & get(f[2])>get(f[1]) ,':=' (hit=1/2, hit3=0)] #tie between n, a
  HS_dt = ddt[,.(HS_max = mean(hit), HS_min = mean(hit3)),by = by]
  HS_dt = HS_dt[,HS_mid:=1-HS_max-HS_min][]
  return(HS_dt)
}

#' Hit Skill Score
#'
#' @description This score is suitable for tercile category forecasts. The skill score is the difference between the hit scores
#' for the categories with the highest and lowest probabilities.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in dt if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1999:2001)
#' print(dt)
#' HSS(dt)
#' @export

HSS = function(dt,f = c('below','normal','above'),
               o = tc_cols(dt),
               by = by_cols_terc_fc_score(),
               pool = 'year',
               dim.check = TRUE)
{
  # for devtools::check:
  HS_max = HS_min = NULL

  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Hit skill score:

  HS_dt = HS(dt,f,o,by,pool)
  HSS_dt = HS_dt[,.(HSS = HS_max-HS_min),by = by]
  return(HSS_dt)
}


#' Effective Interest Rate
#'
#' @description This score is suitable for tercile category forecasts. Using log2 for now (?). According to Mason, the averaging here
#' should be over many years at a single locations and for discrete time-periods (so Mason prefers to take the average after
#' averaging over different locations, but I keep it like this for now).
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in dt if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' EIR(dt)
#' @export

EIR = function(dt,f = c('below','normal','above'),
               o = tc_cols(dt),
               by = by_cols_terc_fc_score(),
               pool = 'year',
               dim.check = TRUE)
{
  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  EIR_dt = dt[,.(EIR =2^(-log2(1/3) + mean(indicator_times_value_aux((get(o) == -1),log2(get(f[1]))) +
                                             indicator_times_value_aux((get(o) == 0),log2(get(f[2]))) +
                                             indicator_times_value_aux((get(o) == 1),log2(get(f[3])))))-1),by = by]

  return(EIR_dt)
}


#' Auxiliary function for multiplying two numbers such that 0 x infty is 0. Needed for the ignorance score: 0log(0) should be 0.
#' @param indicator logical input vector
#' @param value numeric input vector
#' @return indicator x value with 0*infty = 0

indicator_times_value_aux = function(indicator,value)
{
  ret_vec = rep(0, length(indicator))
  ret_vec[indicator] = value[indicator]
  return(ret_vec)
}

#' Ignorance Score
#'
#' @description This score is suitable for tercile category forecasts. Using log2 for now (?).
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in dt if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' IGS(dt)
#' @export

IGS = function(dt,f = c('below','normal','above'),
                o = tc_cols(dt),
                by = by_cols_terc_fc_score(),
                pool = 'year',
                dim.check = TRUE)
{
  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Ignorance score:

  IGS_dt = dt[,.(IGS = - mean((indicator_times_value_aux((get(o) == -1),log2(get(f[1]))) +
                                 indicator_times_value_aux((get(o) == 0),log2(get(f[2]))) +
                                 indicator_times_value_aux((get(o) == 1),log2(get(f[3])))))),
              by = by]

  return(IGS_dt)
}


#' Ignorance Skill score
#'
#' @description This score is suitable for tercile category forecasts. Using log2 for now (?). This is the "usual" skill score
#' (not the effective interest rate).
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in `dt` if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' IGSS(dt)
#' @export

IGSS = function(dt,f = c('below','normal','above'),
                 o = tc_cols(dt),
                 by = by_cols_terc_fc_score(),
                 pool = 'year',
                 dim.check = TRUE)
{
  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Ignorance score:

  IGSS_dt = dt[,.(IGSS = 1 - mean((indicator_times_value_aux((get(o) == -1),log2(get(f[1]))) +
                                     indicator_times_value_aux((get(o) == 0),log2(get(f[2]))) +
                                     indicator_times_value_aux((get(o) == 1),log2(get(f[3])))))/log2(1/3)),
               by = by]

  return(IGSS_dt)
}


#' Multicategory Brier score
#'
#' @description This score is suitable for tercile category forecasts.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in dt if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' MB(dt)
#' @export

MB = function(dt,f = c('below','normal','above'),
              o = tc_cols(dt),
              by = by_cols_terc_fc_score(),
              pool = 'year',
              dim.check = TRUE)
{
  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Multicategory Brier skill score:

  MB_dt = dt[,.(MB = mean((get(f[1]) - (get(o) == -1))^2 + (get(f[2]) - (get(o) == 0))^2 + (get(f[3]) - (get(o) == 1))^2)),by = by]
  return(MB_dt)
}



#' Multicategory Brier Skill score
#'
#' @description This score is suitable for tercile category forecasts.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in `obs_dt`, or in dt if `obs_dt = NULL`). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' MBS(dt)
#' @export

MBS = function(dt,f = c('below','normal','above'),
               o =tc_cols(dt),
               by = by_cols_terc_fc_score(),
               pool = 'year',
               dim.check = TRUE)
{
  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Multicategory Brier skill score:

  MBSS_dt = dt[,.(MBS = 3/2 * (2/3 - mean((get(f[1]) - (get(o) == -1))^2 + (get(f[2]) - (get(o) == 0))^2 + (get(f[3]) - (get(o) == 1))^2))),by = by]
  return(MBSS_dt)
}


#' ROC score (AUC)
#'
#' @description Calculates the area under curve (AUC) or ROC-score from a vector of probabilities and corresponding observations.
#' Formula (1a) from Mason&2018 is used in the calculation, corresponding to trapezoidal interpolation.
#' This is mostly an auxiliary function for the ROCS function, but also used in the ROC-diagram function, where the AUC is added to the diagrams.
#'
#' @param probs vector with probabilities (between 0 and 1)
#' @param obs vector with categorical observations (as TRUE/FALSE)
#'
#'@return numeric. The ROC score.
#'
#'@examples
#'roc_score_vec(probs = c(0.1,0.6,0.3,0.4),
#'              obs = c(FALSE,TRUE,TRUE,FALSE))
#' @export

roc_score_vec = function(probs,obs)
{
  # for devtools::check():
  prob = countzeros = countzeros2 = NULL
  #use data tables fast order:
  temp = data.table(prob = probs,obs = obs)
  setorder(temp,prob,obs) # sort by probability. For multiple entries with equal probabilities, sort the ones with obs == TRUE last.

  n1 = as.numeric(temp[(obs),.N])# as.numeric to prevent integer overflow in n1*n0 when working with large datasets.
  n0 = as.numeric(temp[!(obs),.N])
  temp[,countzeros := cumsum(!obs)] # counts for each entry how many zero-observations have happened previously, including the ones with the same probability as the entry.
  temp[,countzeros2 := 0.5*cumsum(!obs),by = prob] # counts for each entry how many zero-observations have happened with the same probability as the entry.

  ROCscore = temp[(obs),sum(countzeros - countzeros2)/(n1 * n0)]
  return(ROCscore)
}


#' ROC-score/Area Under Curve(AUC)
#'
#' @description This score is not proper, but can be used to assess the resolution of a tercile forecast.
#' The ROC score requires more datapoints to be robust than e.g. the ignorance or Brier score. Therefore the default is to pool the data in space and only calculate one score per season.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 lon = 1:3)
#' print(dt)
#' ROCS(dt)
#'
#' @export


ROCS = function(dt,f = c('below','normal','above'),
                o = tc_cols(dt),
                by = by_cols_terc_fc_score_sp(dt),
                pool = c('year',space_dimvars(dt)),
                dim.check = TRUE)
{
  # for devtools::check:
  ROC_normal = ROC_below = NULL

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # ROC score:
  res_above = dt[,.(ROC_above = roc_score_vec(get(f[3]),get(o) == 1)),by = by]
  res_normal = dt[,.(ROC_normal = roc_score_vec(get(f[2]),get(o) == 0)),by = by]
  res_below = dt[,.(ROC_below = roc_score_vec(get(f[1]),get(o) == -1)),by = by]

  res = res_above[,ROC_normal := res_normal[,ROC_normal]]
  res[,ROC_below := res_below[,ROC_below]][]
  return(res)
}


#' Generalized Discrimination score
#'
#' @description Calculate the Generalized discrimination score from a data.table with data belonging to a single group (as defined by the
#' by variable in the DISS function), for example a single location and month.
#' Formula (5a) from Mason&2018 is used in the calculation. Mostly auxiliary function for the DISS function.
#'
#' @param year  a vector of pool variables, typically year.
#' @param obs   a vector of observations the observation column, needs to contain -1 if it falls into the first category,
#'  0 for the second and 1 for the third category.
#' @param pB    a vector of probabilities for the first category.
#' @param pN    a vector of probabilities for the second category.
#' @param pA    a vector of probabilities for the third category.
#'
#'@return A data table with the scores
#'
#'@examples
#'disc_score_dt(year = 1999:2001,
#'              obs = c(-1,0,0),
#'              pB = c(0.5,0.3,0),
#'              pN = c(0.3,0.3,0.7),
#'              pA = c(0.2,0.4,0.3))
#'
#' @export

disc_score_dt = function(year,obs,pB,pN,pA)
{
  dt = data.table(year=year,tercile_cat=obs,below=pB,normal=pN,above=pA)
  mm <- dim(dt)[1]
  ll <- table(dt$tercile_cat)
  if (length(ll)>1){
    if (length(ll)==3){
      nn <- ll["-1"]*sum(ll[c("0","1")])+ll["0"]*ll["1"]
    }else{
      nn <- ll[1]*ll[2]
    }
    dte <- data.table(matrix(0,nrow = nn, ncol = 5))
    colnames(dte) <- c("year0","year1","k","l","F")
    ii <- 1
    for (i in 1:mm){
      idy <- which(dt$tercile_cat!=dt$tercile_cat[i] & dt$year>dt$year[i])
      if (length(idy)==0) next
      set(dte, i = ii:(ii+length(idy)-1), j ="year0",value = c(rep(dt$year[i],length(idy))))
      set(dte, i = ii:(ii+length(idy)-1), j ="year1",value = dt$year[idy])
      set(dte, i = ii:(ii+length(idy)-1), j ="k",value = c(rep(dt$tercile_cat[i],length(idy))))
      set(dte, i = ii:(ii+length(idy)-1), j ="l",value = dt$tercile_cat[idy])
      p0 <- as.numeric(dt[i,c("below","normal","above")])
      p1s <- as.matrix(dt[idy,c("below","normal","above")])
      ff <- (p0[1]*(p1s[,2]+p1s[,3])+p0[2]*p1s[,3])/(1-(p0[1]*p1s[,1]+p0[2]*p1s[,2]+p0[3]*p1s[,3]))
      set(dte, i = ii:(ii+length(idy)-1), j ="F",value = ff)
      ii <- ii+length(idy)
    }

    dte[,I:=fifelse(F==0.5,0.5,as.numeric(F>0.5))]
    dd <- as.numeric(sum(dte$I)/nn)
  }else{
    dd <- as.numeric(NA)
  }
  return(dd)
}


#' Generalized discrimination score
#'
#' @description A generalisation of the ROC score for more than two categories.
#' This score is not proper, but can be used to assess the discrimination of a tercile forecast.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1:3)
#' print(dt)
#' DISS(dt)
#' @export


DISS = function(dt,f = c('below','normal','above'),
                o = tc_cols(dt),
                by = by_cols_terc_fc_score_sp(),
                pool = 'year',
                dim.check = TRUE)
{
  by = intersect(by,names(dt))

  nas = which(rowSums(dt[,lapply(.SD,is.na),.SDcols = c(o,f)]) > 0)
  if(length(nas) > 0) dt = dt[-nas]

  checks_terc_fc_score()

  res = dt[,.(DIS = disc_score_dt(get(pool),get(o),get(f[1]),get(f[2]),get(f[3]))),by = by]
  return(res)
}


#' Ranked Probability score
#'
#' @description This score is suitable for tercile category forecasts.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1:3)
#' print(dt)
#' RPS(dt)
#' @export

RPS = function(dt,f = c('below','normal','above'),
                o = tc_cols(dt),
                by = by_cols_terc_fc_score(),
                pool = 'year',
                dim.check = TRUE)
{
  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Ranked Probability score:

  RPS_dt = dt[,.(RPS = mean((get(f[1]) - (get(o) == -1))^2  + (get(f[3]) - (get(o) == 1))^2)),by = by]
  return(RPS_dt)
}


#' Ranked Probability skill score
#'
#' @description This score is suitable for tercile category forecasts.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#'  @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1:3)
#' print(dt)
#' RPSS(dt)
#' @export


RPSS = function(dt,f = c('below','normal','above'),
               o = tc_cols(dt),
               by = by_cols_terc_fc_score(),
               pool = 'year',
               dim.check = TRUE)
{
  # for devtools::check:
  RPS_clim = NULL

  by = intersect(by,names(dt))

  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  # Ranked Probability skill score:

  #RPS_dt = RPS(dt,f,o,by,pool)
  RPS_dt = dt[,.(RPS = mean((get(f[1]) - (get(o) == -1))^2  + (get(f[3]) - (get(o) == 1))^2),
                     RPS_clim = mean((1/3 - (get(o) == -1))^2  + (1/3 - (get(o) == 1))^2)),by = by]
  RPSS_dt = RPS_dt[,RPSS := 1-RPS/RPS_clim][]

  return(RPSS_dt)
}


#' Resolution score
#'
#' @description Computes both the resolution component of the Brier score or resolution component of the Ignorance score.
#' Mason claims to prefer the ignorance score version, but this has a very high chance of being NA (much higher
#' than for the full ignorance score itself, I think we should drop it for that reason). Mason writes that the
#' scores are unstable for single locations and that one should pool over many locations.
#' Requires the specification of probability bins. One score for each category (below, normal, above) and
#' also the sum of the scores.
#' Values close to 0 means low resolution. Higher values mean higher resolution.
#'
#' @param dt Data table containing the predictions.
#' @param bins probability bins, defaults to c("<30", "30-35",">35")
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1:3)
#' print(dt)
#' RES(dt)
#' @export


RES = function(dt,bins=c(0.30,0.35001),f = c('below','normal','above'),
               o = tc_cols(dt),
               by = by_cols_terc_fc_score(),
               pool = 'year',
               dim.check = TRUE)
{
  # for devtools::check:
  obs_freqB_gen = obs_freqN_gen = obs_freqA_gen = bs = bsB=pB=pN=pA=obsA=obsN=obsB=count=obs_freqB=prob=obs_freqN=obs_freqA=bsN=bsA=igs=igsN=igsA=igsB=NULL

  by = intersect(by,names(dt))

  nas = which(rowSums(dt[,lapply(.SD,is.na),.SDcols = c(o,f)]) > 0)
  if(length(nas) > 0) dt = dt[-nas]

  checks_terc_fc_score()

  # Determining when each of the 3 probabilities fall into which probability bin (where the bins are numbered 1, 2,...)
  mm <- length(bins)
  dt[,pB:=rep(mm+1,dim(dt)[1])]
  dt[,pN:=rep(mm+1,dim(dt)[1])]
  dt[,pA:=rep(mm+1,dim(dt)[1])]
  for (i in 1:mm){
    dt[get(f[1])<bins[i] & pB==mm+1,pB:=i]
    dt[get(f[2])<bins[i] & pN==mm+1,pN:=i]
    dt[get(f[3])<bins[i] & pA==mm+1,pA:=i]
  }

  dt[,obsA:=fifelse(get(o) == 1,1,0)]
  dt[,obsN:=fifelse(get(o) == 0,1,0)]
  dt[,obsB:=fifelse(get(o) == -1,1,0)]

  RS_dt_B = dt[,.(obs_freqB = mean(obsB),count = .N),by = c(by,"pB")]
  RS_dt_N = dt[,.(obs_freqN = mean(obsN),count = .N),by = c(by,"pN")]
  RS_dt_A = dt[,.(obs_freqA = mean(obsA),count = .N),by = c(by,"pA")]
  RS_dt_Bg = dt[,.(obs_freqB_gen = mean(obsB)),by = by]
  RS_dt_Ng = dt[,.(obs_freqN_gen= mean(obsN)),by = by]
  RS_dt_Ag = dt[,.(obs_freqA_gen= mean(obsA)),by = by]
  if (length(by)==0){
    RS_dt_B = RS_dt_B[,obs_freqB_gen:=as.numeric(RS_dt_Bg)]
    RS_dt_N = RS_dt_N[,obs_freqN_gen:=as.numeric(RS_dt_Ng)]
    RS_dt_A = RS_dt_A[,obs_freqA_gen:=as.numeric(RS_dt_Ag)]
  }else{
    RS_dt_B = merge(RS_dt_B,RS_dt_Bg,by=by)
    RS_dt_N = merge(RS_dt_N,RS_dt_Ng,by=by)
    RS_dt_A = merge(RS_dt_A,RS_dt_Ag,by=by)
  }

  #  Resolution component of the Brier score
  RS_bs_B = RS_dt_B[,.(bsB=sum(count*(obs_freqB-obs_freqB_gen)^2)/sum(count)),by=by]
  RS_bs_N = RS_dt_N[,.(bsN=sum(count*(obs_freqN-obs_freqN_gen)^2)/sum(count)),by=by]
  RS_bs_A = RS_dt_A[,.(bsA=sum(count*(obs_freqA-obs_freqA_gen)^2)/sum(count)),by=by]
  RS_dt = cbind(RS_bs_B,RS_bs_N[,.(bsN)], RS_bs_A[,.(bsA)])
  RS_dt[,bs:=bsB+bsN+bsA]

  #  Resolution component of the Ignorance score
  RS_ig_B = RS_dt_B[,.(igsB=sum(count*(obs_freqB*log2(obs_freqB/obs_freqB_gen) + (1-obs_freqB)*log2((1-obs_freqB)/(1-obs_freqB_gen)))/sum(count))),by=by]
  RS_ig_N = RS_dt_N[,.(igsN=sum(count*(obs_freqN*log2(obs_freqN/obs_freqN_gen) + (1-obs_freqN)*log2((1-obs_freqN)/(1-obs_freqN_gen)))/sum(count))),by=by]
  RS_ig_A = RS_dt_A[,.(igsA=sum(count*(obs_freqA*log2(obs_freqA/obs_freqA_gen) + (1-obs_freqA)*log2((1-obs_freqA)/(1-obs_freqA_gen)))/sum(count))),by=by]
  RS_dt2 = cbind(RS_ig_B,RS_ig_N[,.(igsN)], RS_ig_A[,.(igsA)])
  RS_dt2[,igs:=igsB+igsN+igsA]

  RS_dt = cbind(RS_dt, RS_dt2[,.(igsB,igsN,igsA,igs)])
  dt[, c("pB","pN","pA","obsB","obsN","obsA"):=NULL]
  return(RS_dt)
}

#' Reliability score
#'
#' @description Computes both the reliability component of the Brier score or reliability component of the Ignorance score.
#' Mason claims to prefer the ignorance score version, but this has a very high chance of being NA. Mason writes that the
#' scores are unstable for single locations and that one should pool over many locations.
#' Requires the specification of probability bins. One score for each category (below, normal, above) and
#' also the sum of the scores.
#'
#' Values close to 0 indicate reliable forecasts. Higher values mean less reliable forecasts.
#'
#' @param dt Data table containing the predictions.
#' @param bins probability bins, defaults to ("<30", "30-35",">35") which is given as c(0.30, 0.35001).
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#' @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1:3)
#' print(dt)
#' REL(dt)
#'
#' @export
#'


REL = function(dt,bins=c(0.30,0.35001),f = c('below','normal','above'),
               o = tc_cols(dt),
               by = by_cols_terc_fc_score(),
               pool = 'year',
               dim.check = TRUE)
{
  # for devtools::check:
  bs = bsB=pB=pN=pA=obsA=obsN=obsB=count=obs_freqB=prob=obs_freqN=obs_freqA=bsN=bsA=igs=igsN=igsA=igsB=NULL


  nas = which(rowSums(dt[,lapply(.SD,is.na),.SDcols = c(o,f)]) > 0)
  if(length(nas) > 0) dt = dt[-nas]

  by = intersect(by,names(dt))



  checks_terc_fc_score()

  # Determining when each of the 3 probabilities fall into which probability bin
  # (where the bins are numbered 1, 2,... from the lower probability to higher)
  mm <- length(bins)
  bins_ext <- c(0,bins,1)
  probs <- cumsum(bins_ext)[2:(mm+2)]/2
  bins_dt <- data.table(bin=1:(mm+1),prob=probs)
  dt[,pB:=rep(mm+1,dim(dt)[1])]
  dt[,pN:=rep(mm+1,dim(dt)[1])]
  dt[,pA:=rep(mm+1,dim(dt)[1])]
  for (i in 1:mm){
    dt[get(f[1])<bins[i] & pB==mm+1,pB:=i]
    dt[get(f[2])<bins[i] & pN==mm+1,pN:=i]
    dt[get(f[3])<bins[i] & pA==mm+1,pA:=i]
  }

  dt[,obsA:=fifelse(get(o) == 1,1,0)]
  dt[,obsN:=fifelse(get(o) == 0,1,0)]
  dt[,obsB:=fifelse(get(o) == -1,1,0)]

  RS_dt_B = dt[,.(obs_freqB = mean(obsB),count = .N),by = c(by,"pB")]
  RS_dt_N = dt[,.(obs_freqN = mean(obsN),count = .N),by = c(by,"pN")]
  RS_dt_A = dt[,.(obs_freqA = mean(obsA),count = .N),by = c(by,"pA")]
  # RS_dt_Bg = dt[,.(obs_freqB_gen = mean(obsB)),by = by]
  # RS_dt_Ng = dt[,.(obs_freqN_gen= mean(obsN)),by = by]
  # RS_dt_Ag = dt[,.(obs_freqA_gen= mean(obsA)),by = by]
  # if (length(by)==0){
  #   RS_dt_B = RS_dt_B[,obs_freqB_gen:=as.numeric(RS_dt_Bg)]
  #   RS_dt_N = RS_dt_N[,obs_freqN_gen:=as.numeric(RS_dt_Ng)]
  #   RS_dt_A = RS_dt_A[,obs_freqA_gen:=as.numeric(RS_dt_Ag)]
  # }else{
    RS_dt_B = merge(RS_dt_B,bins_dt,by.x="pB",by.y="bin")
    RS_dt_N = merge(RS_dt_N,bins_dt,by.x="pN",by.y="bin")
    RS_dt_A = merge(RS_dt_A,bins_dt,by.x="pA",by.y="bin")
  #}

  #  Resliability component of the Brier score
  RS_bs_B = RS_dt_B[,.(bsB=sum(count*(obs_freqB-prob)^2)/sum(count)),by=by]
  RS_bs_N = RS_dt_N[,.(bsN=sum(count*(obs_freqN-prob)^2)/sum(count)),by=by]
  RS_bs_A = RS_dt_A[,.(bsA=sum(count*(obs_freqA-prob)^2)/sum(count)),by=by]
  RS_dt = cbind(RS_bs_B,RS_bs_N[,.(bsN)], RS_bs_A[,.(bsA)])
  RS_dt[,bs:=bsB+bsN+bsA]

  #  Resolution component of the Ignorance score
  RS_ig_B = RS_dt_B[,.(igsB=sum(count*(obs_freqB*log2(obs_freqB/prob) + (1-obs_freqB)*log2((1-obs_freqB)/(1-prob)))/sum(count))),by=by]
  RS_ig_N = RS_dt_N[,.(igsN=sum(count*(obs_freqN*log2(obs_freqN/prob) + (1-obs_freqN)*log2((1-obs_freqN)/(1-prob)))/sum(count))),by=by]
  RS_ig_A = RS_dt_A[,.(igsA=sum(count*(obs_freqA*log2(obs_freqA/prob) + (1-obs_freqA)*log2((1-obs_freqA)/(1-prob)))/sum(count))),by=by]
  RS_dt2 = cbind(RS_ig_B,RS_ig_N[,.(igsN)], RS_ig_A[,.(igsA)])
  RS_dt2[,igs:=igsB+igsN+igsA]

  RS_dt = cbind(RS_dt, RS_dt2[,.(igsB,igsN,igsA,igs)])
  dt[, c("pB","pN","pA","obsB","obsN","obsA"):=NULL]
  return(RS_dt)
}

#' Compute the slope of the reliability curve
#'
#' @description Values below 1 indicate a lack of resolution or overconfidence, 1 is perfect, above means underconfident.
#' This score requires more datapoints to be robust than e.g. the ignorance or Brier score. Therefore the default is to pool the data in space and only calculate one score per season.
#'
#' @param dt Data table containing the predictions.
#' @param f column names of the prediction.
#' @param o column name of the observations (either in obs_dt, or in dt if obs_dt = NULL). The observation column needs to
#' contain -1 if it falls into the first category (corresponding to `fcs[1]`), 0 for the second and 1 for the third category.
#' @param by column names of grouping variables, all of which need to be columns in dt.
#' Default is to group by all instances of month, season, lon, lat, system and lead_time that are columns in dt.
#' @param pool column name(s) for the variable(s) along which is averaged, typically just 'year'.
#' @param dim.check Logical. If TRUE, the function tests whether the data table contains only one row per coordinate-level, as should be the case.
#'
#'@return A data table with the scores
#'
#'  @examples
#' dt = data.table(below = c(0.5,0.3,0),
#'                 normal = c(0.3,0.3,0.7),
#'                 above = c(0.2,0.4,0.3),
#'                 tc_cat = c(-1,0,0),
#'                 year = 1:3)
#' print(dt)
#' SRC(dt)
#' @export

SRC = function(dt,f = c('below','normal','above'),
                o = tc_cols(dt),
                by = by_cols_terc_fc_score_sp(dt),
                pool = c('year',space_dimvars(dt)),
                dim.check = TRUE)
{
  # for devtools::check:
  SRC_normal = SRC_below = NULL


  dt = dt[!is.na(get(o)) & !is.na(get(f[1]))]

  checks_terc_fc_score()

  rdv_wrapper = function(discrete_probs, obs)
  {
    ret_val = suppressWarnings(rel_diag_vec(discrete_probs, obs, slope_only = TRUE))
    return(ret_val)
  }


  # score:
  res_above = dt[,.(SRC_above = rdv_wrapper(get(f[3]),get(o) == 1)),by = by]
  res_normal = dt[,.(SRC_normal = rdv_wrapper(get(f[2]),get(o) == 0)),by = by]
  res_below = dt[,.(SRC_below = rdv_wrapper(get(f[1]),get(o) == -1)),by = by]

  res = res_above[,SRC_normal := res_normal[,SRC_normal]]
  res[,SRC_below := res_below[,SRC_below]][]
  return(res)
}
