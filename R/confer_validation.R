#' Assess score uncertainty by bootstrapping
#'
#' @param dt Data table containing scores.
#' @param r how many bootstrap-resamples of the mean score should be considered?
#' @param score_col column name of the scores
#' @param bycols should the mean score be aggregated? E.g. if you have a data table with scores from different systems,
#' putting 'system' here gives you a bootstrap estimate for the mean score of each system, which is usually what you want
#' @param mc_cores Number of cores for parallelization
#'
#' @importFrom boot boot
#' @export

bootstrap_scores_dt = function(dt,r = 100,score_col,bycols = NULL,mc_cores = 1)
{

  bootstrap_samples = NULL
  statistic = function(x,inds){mean(x[inds],na.rm = T)}

  bootstrap_dt = data.table(R = 1:r)

  if(length(bycols) > 0)
  {
    for(i in 1:length(bycols))
    {
      bootstrap_dt = c(bootstrap_dt,unique(dt[,.SD,.SDcols = bycols[i]]))
    }
  }

  bootstrap_dt = as.data.table(expand.grid(bootstrap_dt))

  if(length(bycols) > 0)
  {
    by_dt = unique(dt[,.SD,.SDcols = bycols])
    setkeyv(by_dt,names(by_dt))
    setkeyv(dt,key(by_dt))
    setkeyv(bootstrap_dt,key(by_dt))

    aux_fun = function(row_index)
    {
      sub_dt = by_dt[row_index,]
      data = dt[sub_dt,get(score_col)]
      samples = boot::boot(data,statistic = statistic,R = r)$t

      bootstrap_dt[sub_dt,bootstrap_samples := samples]
    }

    if(mc_cores == 1)
    {
      for(ri in 1:by_dt[,.N])
      {
        aux_fun(ri)
      }
    } else { # using more cores doesn't speed up things here, so some bad coding happened?
      parallel::mclapply(1:by_dt[,.N],aux_fun,mc.cores = mc_cores)
    }
  } else {
    bootstrap_dt[,bootstrap_samples := boot::boot(dt[,get(score_col)],statistic = statistic,R = r)$t]
  }

  return(bootstrap_dt)
}


#' composite analysis for teleconnections
#'
#'@param var_dt Data table containing the weather variables for which you want to conduct the composite analysis, e.g. monthly mean precip for a range of years, months, and locations.
#'@param TC_dt Data table containing the values of the teleconnection index (such as IOD), typically indexed by year and month.
#'@param TC_name Character string containing the column name of TC_dt where the values of the teleconnection index are stored. Default is third column.
#'@param by_cols Character vector containing the column names of the grouping variables by which the composite analysis is supposed to be conducted. Default is \code{c('month','lon','lat')}.
#'@param average_along_cols Along which columns is averaged.
#'@param var_name Column name of the weather variable in var_dt. Default is first column not contained in \code{by_cols} and not named 'year'.
#'
#'@return A data table containing the composites x_plus and x_minus for each value of by_cols
#'@author Claudio
#'@export
#'
#'@importFrom stats quantile


composite_analysis = function(var_dt,TC_dt,
                              TC_name = names(TC_dt)[3],
                              by_cols = c('month','lon','lat'),
                              average_along_cols = 'year',
                              var_name = setdiff(names(var_dt),c(average_along_cols,by_cols))[1])
{

  # get three categories: -1 is low TC, 0 is normal TC, 1 is high TC
  TC_by = intersect(by_cols,names(TC_dt))
  TC_dt[,cat := -1*(get(TC_name) <= stats::quantile(get(TC_name),0.33)) + 1*(get(TC_name) >= stats::quantile(get(TC_name),0.67)),by = TC_by]

  TCcols = intersect(c('cat',by_cols,average_along_cols),names(TC_dt))
  var_dt = merge(var_dt,TC_dt[,.SD,.SDcols = TCcols],by = c(average_along_cols,TC_by))

  CA_dt = var_dt[,mean(get(var_name)),by = c(by_cols,'cat')]
  setnames(CA_dt,'V1',var_name)
  setkeyv(CA_dt,c(by_cols,'cat'))

  ret_dt = unique(CA_dt[,.SD,.SDcols = by_cols])
  x_plus = CA_dt[ cat == 1,get(var_name)] - CA_dt[ cat == 0,get(var_name)]
  x_minus = CA_dt[ cat == -1,get(var_name)] - CA_dt[ cat == 0,get(var_name)]

  ret_dt[,x_plus := x_plus][,x_minus := x_minus]

  return(ret_dt)
}


#' Auxiliary function for computing CRPS on ensemble forecasts
#' Essentially wraps scoringRules::crps_sample, but can deal with missing values.
#' That's important when your ensemble switches size between hind- and forecasts, and
#' when you're computing crps for several systems at once and are too lazy to peel them apart.
#'
#' @param obs vector of observations.
#' @param pred either vector of predictions when a single observation is provided, or a matrix with nrow = length(obs)
#' where the different columns are the different predictions.
#' @param ens_size_correction logical. If TRUE, the correction for ensemble size proposed in Ferro et al. 2008: 'On the effect of ensemble size on the discrete and continuous
#' ranked probability scores' is employed. This makes calculations slower, but should be done when you compare ensembles with different sizes!
#'
#' @importFrom scoringRules crps_sample
#' @export

crps_sample_na = function(obs, pred, ens_size_correction = FALSE)
{
  if(ens_size_correction)
  {
    crps_sample_na_single_obs = function(single_obs,vector_pred)
    {
      vector_pred2 = vector_pred[!(is.na(vector_pred) | is.nan(vector_pred))]

      ens_size = length(vector_pred2)
      mean_dist_xx = mean(stats::dist(vector_pred2))

      crps = scoringRules::crps_sample(single_obs,vector_pred2) - mean_dist_xx/(2*ens_size)
      return(crps)
    }
  } else {
    crps_sample_na_single_obs = function(single_obs,vector_pred)
    {
      vector_pred2 = vector_pred[!(is.na(vector_pred) | is.nan(vector_pred))]
      crps = scoringRules::crps_sample(single_obs,vector_pred2)
      return(crps)
    }
  }

  if(length(obs) == 1)
  {
    crps = crps_sample_na_single_obs(obs,pred)
  } else{
    crps = sapply(seq_along(obs), function(i) crps_sample_na_single_obs(obs[i], pred[i,]))
  }


  return(crps)
}
