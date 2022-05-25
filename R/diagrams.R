
# This file contains functions to create diagrams for evaluation, such as ROC curves and reliability diagrams,
# plus auxiliary functions directly used for these functions.

#' auxiliary function for rounding probabilities
#'
#' takes a vector of probabilities (between 0 and 1) and rounds them to the scale specified by binwidth. This is used for reliability diagrams,
#' where one point is drawn for each bin. 0 is always at the center of the first interval for rounding:
#' E.g. if binwidth = 0.05 (the default), then probabilities up to 0.025 are rounded to 0, probs between 0.025 and 0.075 are rounded to 0.05, etc.
#'
#' @param probs vector of probabilities (between 0 and 1, not percent)
#' @param binwidth width of the bins for rounding.
#' @export

round_probs = function(probs,binwidth = 0.05)
{
  bins = seq(0 - binwidth/2,1 + binwidth/2,by = binwidth) # typical binwidth is 0.05 which means you want the probabilities rounded to 5%, meaning that the intervals needs to be shifted
  rounded_probs = (bins[1:(length(bins)-1)] + binwidth/2 )  # display in percent
  return( rounded_probs[as.integer(cut(probs,breaks = bins))])
}


#' Create a reliability diagram from a vector of probabilities and observations. The probabilities have to be rounded beforehand
#' (see \code{round_probs}), because the diagram draws a point for each level of the probabilities. The diagram includes a histogram indicating
#' the forecast relative frequency for each probability bin. The diagram shows the reliability curve and the diagonal for reference.
#' Moreover, it shows a regression line fitted by weighted linear regression where the forecast relative frequencies are used as weights.
#' A horizontal and vertical line indicate the frequency of observation = TRUE over the entire dataset.
#'
#' @param discrete_probs Vector of (rounded) probabilites.
#' @param obs Vector of logical observations.

rel_diag_vec = function(discrete_probs,obs)
{
  temp = data.table(prob = 100*discrete_probs,obs = obs)
  rel_diag_dt = temp[,.(obs_freq = mean(obs) * 100,
                        count = .N,
                        obs_count = sum(obs)),by = prob]

  # reduce to bins with more than 1 data point:
  rel_diag_dt = rel_diag_dt[count > 1]

  # warning if you have to few points for the discretization used:
  if(rel_diag_dt[,mean(count)] <= 5)
  {
    percentage = 100/(rel_diag_dt[,.N] - 1)
    warning(paste0('You are trying to derive a diagram with probabilities discretized to a ',percentage,'%-level.\n
                   On average you have only ',rel_diag_dt[,mean(count)],' observations per category. Consider coarser discretization.'))
  }

  rel_diag_dt[,frequency := count/sum(count) * 100]

  total_freq = 100*mean(obs)

  # add linear regression line:

  model = lm(obs_freq ~ prob,data = rel_diag_dt,weights = frequency)


  pp = ggplot(rel_diag_dt) +
    geom_vline(xintercept = total_freq,color = 'gray') +
    geom_hline(yintercept = total_freq,color = 'gray') +
    geom_line(aes(x = prob,y = obs_freq),color = 'blue',size = 1) +
    geom_abline(intercept = model$coefficients[1], slope  = model$coefficients[2],color = 'blue',linetype = 'dashed') +
    geom_point(aes(x = prob,y = obs_freq),color = 'blue')+
    geom_col(aes(x = prob,y = frequency),width = 2) +
    scale_x_continuous(name = 'Forecast Probability (%)') +
    scale_y_continuous(name = 'Observed Relative Frequency (%)') +
    geom_abline(slope = 1)+
    theme_bw() + theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(0,100),
                    ylim = c(0,100),
                    expand = FALSE)
  return(pp)
}



#' Creates reliability diagrams from a data table containing tercile forecasts. It wraps \code{rel_diag_vec}, see \code{?rel_diag_vec} for details
#' about the output diagrams. The output format is very much inspired by Figure 5 of Mason&2018. By default, 4 diagrams are drawn,
#' one for each the prediction of above-, normal- and below-values, plus one for  all forecasts together.
#' You can provide a 'by' argument to obtain separate reliability diagrams for different values of the by-columns. E.g., when you data table contains
#' a column named 'season', you can set by = 'season'. Then, the function will output a list of 16 diagrams, 4 for each season.
#'
#' @param dt Data table containing tercile forecasts
#' @param f column names of the prediction columns
#' @param o column name of the observation column
#' @param by column names of grouping variables. Default is to not group.
#' @param pool column names of pooling variables (used for the dimension check). Default is all dimvars.
#' @param dim.check Logical. If TRUE, the function checks whether the columns in by and pool span the entire data table.
#'
#' @return A list of gg objects which can be plotted by ggpubr::ggarrange (for example)
#'
#' @importFrom ggpubr ggarrange
#' @export

rel_diag = function(dt,
                    f = c('below','normal','above'),
                    o = 'tercile_cat',
                    by = NULL,
                    pool = dimvars(dt),
                    binwidth = 0.05,
                    dim.check = TRUE)
{

  dt = dt[!is.na(get(o)) & !is.na(get(f))]
  # check for correct naming of columns etc.
  checks_terc_fc_score()

  # a crude check whether the provided probabilities are discrete. If they are, they are used directly for the reliability diagram, that is
  # each point in the RD corresponds to one probability on the discrete scale. If they are not, probabilities are rounded to a scale defined by binwidth.
  check_discrete_probs = (length(unique(dt[,get(f[1])])) <= min(50,1/5 * dt[,.N]))

  if(is.null(by))
  {
    #same order of plots as suggested in the WMO guidance
    if(check_discrete_probs)
    {
      rps1 = dt[,get(f[3])]
      rps2 = dt[,get(f[2])]
      rps3 = dt[,get(f[1])]
    } else {
      rps1 = round_probs(dt[,get(f[3])],binwidth = binwidth)
      rps2 = round_probs(dt[,get(f[2])],binwidth = binwidth)
      rps3 = round_probs(dt[,get(f[1])],binwidth = binwidth)
    }


    obs1 = dt[,get(o) == 1]
    obs2 = dt[,get(o) == 0]
    obs3 = dt[,get(o) == -1]

    rps4 = c(rps1,rps2,rps3)
    obs4 = c(obs1,obs2,obs3)

    pp1 = rel_diag_vec(rps1,obs1) + ggtitle('Above')
    pp2 = rel_diag_vec(rps2,obs2) + ggtitle('Normal')
    pp3 = rel_diag_vec(rps3,obs3)  + ggtitle('Below')
    pp4 = rel_diag_vec(rps4,obs4) + ggtitle('All')
    ppl = list(pp1,pp2,pp3,pp4)
  }

  return_plot = ggpubr::ggarrange(plotlist = ppl,ncol = 2,nrow = 2)
  plot(return_plot)
  return(return_plot) # do not return return_plot, which is a single gg object, with facets generated by ggpubr::ggarrange.
              # It is way easier to work with ppl, and this make the behavior consistent with the case !is.null(by)

  if(!is.null(by))
  {
    by_dt = unique(dt[,.SD,.SDcols = by])
    nby = by_dt[,.N]
    if(nby >= 12)
    {
      mm = menu(choices = c('yes','no'),
                title = paste0("Your choice of 'by' would result in ",4*nby," plots.\nDo you want to proceed?"))
      if(mm == 2)
      {
        #stop without error:
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }
    }
    plot_list = list()
    for(row_ind in 1:by_dt[,.N])
    {
      dt_sub = merge(dt,by_dt[row_ind],by = colnames(by_dt))
      title_temp = c()
      for(ii in 1:ncol(by_dt))
      {
        title_temp = c(title_temp,paste0(names(by_dt)[ii],' = ',by_dt[row_ind,..ii]))
      }
      title_temp = paste(title_temp,collapse = ', ')

      ## copy-paste from above ##

      #same order of plots as suggested in the WMO guidance:
      if(check_discrete_probs)
      {
        rps1 = dt_sub[,get(f[3])]
        rps2 = dt_sub[,get(f[2])]
        rps3 = dt_sub[,get(f[1])]
      } else {
        rps1 = round_probs(dt_sub[,get(f[3])],binwidth = bindwidth)
        rps2 = round_probs(dt_sub[,get(f[2])],binwidth = bindwidth)
        rps3 = round_probs(dt_sub[,get(f[1])],binwidth = bindwidth)
      }


      obs1 = dt_sub[,get(o) == 1]
      obs2 = dt_sub[,get(o) == 0]
      obs3 = dt_sub[,get(o) == -1]

      rps4 = c(rps1,rps2,rps3)
      obs4 = c(obs1,obs2,obs3)

      pp1 = rel_diag_vec(rps1,obs1) + ggtitle(paste0('Above, ',title_temp))
      pp2 = rel_diag_vec(rps2,obs2) + ggtitle(paste0('Normal, ',title_temp))
      pp3 = rel_diag_vec(rps3,obs3)  + ggtitle(paste0('Below, ',title_temp))
      pp4 = rel_diag_vec(rps4,obs4) + ggtitle(paste0('All, ',title_temp))
      ppl = list(pp1,pp2,pp3,pp4)
      plot_list = c(plot_list,ppl)
    }
    return(plot_list)
  }
}

##################
### ROC curves ###
##################


#' Plot the ROC-curve for a vector of probabilities and corresponding observations
#'
#' @param probs vector with probabilities (between 0 and 1)
#' @param obs vector with categorical observations
#' @param interpolate logical. If TRUE the ROC-curve is interpolated and drawn as a continuous function. Otherwise it is drawn as a step function.
#'
#' @export


roc_curve_vec = function(probs,obs,interpolate = TRUE)
{

  temp = data.table(prob = probs,obs = obs)
  setorder(temp,-prob,obs)

  levels = unique(temp[,prob])

  n1 = temp[(obs),.N]
  n0 = temp[!(obs),.N]

  temp[,level := prob]
  temp[,hit_rate := cumsum(obs)/n1]
  temp[,false_alarm_rate := cumsum(!obs)/n0]


  AUC = roc_score_vec(probs,obs)


  ROC_dt = temp[,.(x = max(false_alarm_rate), y = max(hit_rate)), level]
  plot_dt = data.table(x = 100*c(0,ROC_dt[,x]),
                       y = 100*c(0,ROC_dt[,y]))

  if(!interpolate) pp = ggplot(plot_dt) + geom_step(aes(x=x,y=y))
  if(interpolate)
  {
    pp = ggplot(plot_dt) + geom_line(aes(x=x,y=y))
    # highlight discretization if it's at a 2%-resolution or coarser:
    if(plot_dt[,.N] <= 51)
    {
      pp = pp + geom_point(aes(x=x,y=y))
    }
  }

  pp = pp + scale_x_continuous(name = 'False Alarm Rate (%)') +
    scale_y_continuous(name = 'Hit Rate (%)') +
    geom_abline(slope = 1,intercept = 0,linetype = 'dashed') +
    theme_bw() +
    coord_cartesian(xlim = c(0,100),ylim = c(0,100),expand = FALSE) +
    geom_label(aes(x = x,y = y,label = label),data = data.table(x = 50,y = 5,label = paste0('Area Under Curve: ',round(AUC,3))))

  return(pp)
}


#' Creates ROC curves from a data table containing tercile forecasts. It wraps \code{roc_curve_vec}.
#' By default, 4 ROC-curves are drawn, one for each the prediction of above-, normal- and below-values, plus one for all forecasts together.
#' You can provide a 'by' argument to obtain separate ROC-curves for different values of the by-columns. E.g., when your data table contains
#' a column named 'season', you can set by = 'season'. Then, the function will output a list of 16 ROC-curvess, 4 for each season.
#'
#' @param dt Data table containing tercile forecasts
#' @param f column names of the prediction columns
#' @param o column name of the observation column
#' @param by column names of grouping variables. Default is to not group.
#' @param pool column names of pooling variables (used for the dimension check). Default is all dimvars.
#' @param dim.check Logical. If TRUE, the function checks whether the columns in by and pool span the entire data table.
#'
#' @return A list of gg objects which can be plotted by ggpubr::ggarrange (for example)
#'
#' @importFrom ggpubr ggarrange
#' @export

ROC_curve = function(dt,
                     f = c('below','normal','above'),
                     o = 'tercile_cat',
                     by = NULL,
                     pool = dimvars(dt),
                     interpolate = TRUE,
                     dim.check = TRUE)
{
  dt = dt[!is.na(get(o)) & !is.na(get(f))]
  # check for correct naming of columns etc.
  checks_terc_fc_score()

  if(is.null(by))
  {
    #same order of plots as for reliability diagrams
    prs1 = dt[,get(f[3])]
    prs2 = dt[,get(f[2])]
    prs3 = dt[,get(f[1])]

    obs1 = dt[,get(o) == 1]
    obs2 = dt[,get(o) == 0]
    obs3 = dt[,get(o) == -1]

    prs4 = c(prs1,prs2,prs3)
    obs4 = c(obs1,obs2,obs3)

    pp1 = roc_curve_vec(prs1,obs1,interpolate = interpolate) + ggtitle('Above')
    pp2 = roc_curve_vec(prs2,obs2,interpolate = interpolate) + ggtitle('Normal')
    pp3 = roc_curve_vec(prs3,obs3,interpolate = interpolate) + ggtitle('Below')
    pp4 = roc_curve_vec(prs4,obs4,interpolate = interpolate) + ggtitle('All')
    ppl = list(pp1,pp2,pp3,pp4)
  }

  # plot results:
  return_plot = ggpubr::ggarrange(plotlist = ppl,ncol = 2,nrow = 2)
  return_plot
  return(return_plot)

  if(!is.null(by))
  {
    by_dt = unique(dt[,.SD,.SDcols = by])
    nby = by_dt[,.N]
    if(nby >= 12)
    {
      mm = menu(choices = c('yes','no'),
                title = paste0("Your choice of 'by' would result in ",4*nby," plots.\nDo you want to proceed?"))
      if(mm == 2)
      {
        #stop without error:
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }
    }
    plot_list = list()
    for(row_ind in 1:by_dt[,.N])
    {
      dt_sub = merge(dt,by_dt[row_ind],by = colnames(by_dt))
      title_temp = c()
      for(ii in 1:ncol(by_dt))
      {
        title_temp = c(title_temp,paste0(names(by_dt)[ii],' = ',by_dt[row_ind,..ii]))
      }
      title_temp = paste(title_temp,collapse = ', ')

      ## copy-paste from above ##
      #same order of plots as for reliability diagrams
      prs1 = dt_sub[,get(f[3])]
      prs2 = dt_sub[,get(f[2])]
      prs3 = dt_sub[,get(f[1])]

      obs1 = dt_sub[,get(o) == 1]
      obs2 = dt_sub[,get(o) == 0]
      obs3 = dt_sub[,get(o) == -1]

      prs4 = c(prs1,prs2,prs3)
      obs4 = c(obs1,obs2,obs3)

      pp1 = roc_curve_vec(prs1,obs1,interpolate = interpolate) + ggtitle('Above')
      pp2 = roc_curve_vec(prs2,obs2,interpolate = interpolate) + ggtitle('Normal')
      pp3 = roc_curve_vec(prs3,obs3,interpolate = interpolate) + ggtitle('Below')
      pp4 = roc_curve_vec(prs4,obs4,interpolate = interpolate) + ggtitle('All')
      ppl = list(pp1,pp2,pp3,pp4)
      plot_list = c(plot_list,ppl)
    }
    return(plot_list)
  }
}


#' Plot a verification map of percentiles
#'
#' For each location, the map shows whether the observed value was normal, below, or above. This makes it possible to visually compare to the usual tercile forecsst
#'
#' @param dt input data table. This has to contain the observations for the year to plot, as well as for many other years (which are used to calculate the climatological reference).
#' The data table should have coumns named `lon`, `lat`, `year`, and an observation column, the name of which is passed as value of `o` to the function, see below.
#' For each level of `lon`, `lat`, and `year`, the table should only contain one row (this is checked by the function).
#' @param o name of the column containing the observation.
#' @param yy The year for which to show the verification map. Defaults to the last year available in dt
#' @param climatology_period which years should the climatology be calculated on? Defaults to all years (except `yy`) in `dt`
#' @param out_file optional path and file name (including valid filetype, like .pdf or .png) for saving the file. If not provided, the function just shows the plot in the running R session.
#'
#' @importFrom ggplotify as.ggplot
#' @importFrom RColorBrewer brewer.pal
#'

ver_map = function(dt,o = 'obs',yy = dt[,max(year)],
                   climatology_period = unique(dt[,year]),
                   out_file = NULL)

{
  # get data in shape
  dt_temp = copy(dt[year %in% climatology_period])
  if(!(yy %in% climatology_period))
  {
    dt_temp = rbindlist(list(dt_temp,dt[year == yy]))
  }

  # remove missing gridpoints:
  dt_temp = dt_temp[!is.na(get(o))]

  # check that there is only one level per coordinate
  if(dt_temp[,.N] != unique(dt_temp[,.(lon,lat,year)])[,.N])
  {
    stop('Your data table seems to have multiple levels (rows) per year, lon, lat.\n
Please subselect before. E.g., if your data contains multiple months, you could run ver_map(dt[month == 10]).')
  }

  # get climatology percentile

  # put yy last, for resolving ties, see below:
  dt_temp[,is_yy := (year == yy)]
  setorderv(dt_temp,c('lon','lat',o, 'is_yy')) # in case of ties (equal o), yy is sorted last
  ny = length(unique(dt_temp[,year]))
  dt_temp[,sample_quantile := 100 * seq(1/ny,1,length.out = ny), by = c('lon','lat')]
  # What about ties? Well, if you have ties in your observations, the function outputs the
  # right limit of the ECDF (since we included is_yy in the ordering).
  # This is consistent with statistical practice, but not necessary what you want:
  # E.g. a location where precip always was exactly zero and is also zero in yy
  # would get a sample quantile of 100%, and would be shown as above normal.
  # I think in this context you want to resolve the ties as the center of the jump,
  # i.e. (F(obs) - F_-(obs))/2. So lets do that:

  dt_temp[,how_many_ties := .N, by = c('lon','lat',o)]
  # jump size is 100% x (how_many_ties)/(.N), where .N is the number of observations for your gridpoint, which is the sample size
  # for your ECDF-calculation. Not that how_many_ties is probably almost always 1/.N, which is the discretization level of the ECDF.
  # Since we sorted yy last in case of ties, we need to subtract half a jump:
  dt_temp = dt_temp[year == yy]
  dt_temp[how_many_ties > 1,sample_quantile := sample_quantile - 50*(how_many_ties)/ny, by = c('lon','lat')]

  # get plot:
  pp = ggplot_dt(dt_temp,'sample_quantile')

  # fix levels and colors for colorscale:
  qqs = c(10,20,33,67,80,90)

  my_colors1 <- RColorBrewer::brewer.pal(9, "BrBG")[c(1,2,3)] # a scale with several browns for dry regions, see RColorBrewer::display.brewer.all()
  my_colors2 = "#B2EBF2" # This is cyan200 for normal, checkout https://www.r-bloggers.com/2018/12/having-bit-of-party-with-material-colour-palette/
  my_colors3 <- RColorBrewer::brewer.pal(9, "Greens")[c(5,7,9)] # a scale with greens

  my_colors = c(my_colors1,my_colors2,my_colors3)

  # create the scale you want:
  sc = scale_fill_stepsn(breaks = qqs,colors = my_colors,name = '',guide = guide_colorsteps(even.steps = FALSE))
  # overwrite scale without returning the usual warning:
  suppressMessages(eval(parse(text = "pp = pp + sc + theme(legend.position = 'bottom',legend.direction = 'horizontal')")))

  # now the messy part begins: We want the legend to be horizontal and as wide as the plot (!!!)
  # turns out that's difficult in ggplot. The following solution is inspired by the first answer posted here: (which does not work for scale_fill_stepsn)
  # https://stackoverflow.com/questions/71073338/set-legend-width-to-be-100-plot-width
  # It's still missing legend section labels for 'normal', 'above', 'below'. An alternative way to fit the legend might be the second answer in stackexchange,
  # using ggh4x::force_panelsizes()


  gt <- ggplotGrob(pp)

  # Extract legend
  is_legend <- which(gt$layout$name == "guide-box")
  legend <- gt$grobs[is_legend][[1]]
  legend <- legend$grobs[legend$layout$name == "guides"][[1]]
  # Set widths in guide gtable
  width <- as.numeric(legend$widths[4]) # save bar width (assumes 'cm' unit)
  legend$widths[4] <- unit(1, "null") # replace bar width

  # Set width/x of bar/labels/ticks. Assumes everything is 'cm' unit.
  width2 = max(legend$grobs[[2]]$x) + legend$grobs[[2]]$width#!!!! legend$grobs[[2]]$width is NOT what you're looking for here
  rescale2 = function(x) unit(as.numeric(x) / as.numeric(width2) , 'npc')

  legend$grobs[[2]]$width <- rescale2(legend$grobs[[2]]$width)
  legend$grobs[[2]]$x = rescale2(legend$grobs[[2]]$x)


  legend$grobs[[3]]$children[[1]]$x <- unit(
    as.numeric(legend$grobs[[3]]$children[[1]]$x) / width, "npc"
  )
  #legend$grobs[[5]]$x0 <- unit(as.numeric(legend$grobs[[5]]$x0) / width, "npc")
  #legend$grobs[[5]]$x1 <- unit(as.numeric(legend$grobs[[5]]$x1) / width, "npc")

  # Replace legend
  gt$grobs[[is_legend]] <- legend

  # Draw new plot
  grid::grid.newpage()
  grid::grid.draw(gt)
  pp = ggplotify::as.ggplot(gt)

  if(!is.null(out_file))
  {
    ggsave(pp,filename = out_file)
  }
  return(pp)
}

