#' plotting function for spatial data
#'
#' @description Plots spatial data from a data.table. The data table needs to contain columns named 'lon' and 'lat'. The grid needs to be regular.
#' If spatial data is contained for several levels (e.g. mutliple times or multiple ensemble members), only the data for the first level is plotted.
#' By default, the first column that is not recognized as a dimension variable is plotted, see \code{data_col}. For the most common data-columns, reasonable
#' color scales are selected automatically.
#'
#' @param dt Data table containing the data for plotting.
#' @param data_col The name of the column in dt containing the data for plotting. If nothing is provided (the default), the first column that is not a dimension variable or 'member' is selected.
#' @param mn optional plot title
#' @param discrete_cs Logical. Should the color scale be discretized?
#' @param midpoint midpoint of the color scale, passed to \code{scale_fill_gradient2} or \code{scale_fill_steps2} (depending on whether \code{discrete_cs == TRUE}).
#' If you set it to NULL (the default), the midpoint is set to the center of the data range (or the center of rr, if provided), such that the entire color scale is used.
#' Note that this default differs from the default behavior of \code{scale_fill_gradient2} or \code{scale_fill_steps2}.
#' Specifying the midpoint can often be a convenient way to force a color scale with only two colors (for example, by setting it
#' to the minimum or maximum of your data).
#' @param rr,low,mid,high,name,breaks,na.value,oob,guide,... Arguments for the color scale, passed to scale_fill_gradient2 or scale_fill_steps2 (depending on whether discrete_cs == TRUE).
#' rr replaces limits (specifying the range of the color scale) for consistency with the older plotting functions from the PostProcessing package.
#' \code{na.value} specifies the color of missing values. \code{oob} specifies the treatment of out-of-boundary values, i.e. values beyond the
#' limits.
#' The ... argument can in particular be used to customize the legend/colorbar using the function \code{guide_colorbar},
#' see https://ggplot2.tidyverse.org/reference/guide_colourbar.html. Moreover, when \code{discrete_cs == TRUE} you can pass the arguments \code{n.breaks,breaks} to customize the scale.
#' If you use n.breaks you might also want to set nice.breaks = FALSE, see ?scale_fill_steps2.
#' @param binwidth,bin_midpoint only used when \code{discrete_cs == TRUE}. Normally, the breaks for the discrete colorscale are specified by n.breaks (which is not reliable,
#' since they're adjusted to be 'nice'), or by specifying the breaks explicitly (which is often tedious). This gives you a third option, namely specifying how far the breaks
#' should be apart, and specifying the centerpoint for one of the bins (default is midpoint, or the center of rr if midpoint is not provided). For example, if your color scale shows
#' percentages and you'd like 4 categories, ranging from white to red, this is easiest achieved by \code{binwidth = 25, midpoint = 12.5}.
#' @param add_map logical. Set to FALSE to remove borders (e.g. if you want to add them yourself from a shapefile).
#' @param extent An optional four-element vector in the order xmin,xmax,ymin,ymax for specifying the spatial extent of the plot. Default is to fit the extent to the data.
#' @param expand.x,expand.y vectors with two entries to be added to xlims/ylims of the plot. E.g. expand.x = c(-0.5,0.5)
#' expands the plot by half a longitude both on the right and left hand side
#'
#' @return a ggplot object.
#'
#' @import data.table
#' @import ggplot2
#'
#' @examples
#' ex_dt = chirps_monthly[lat <0 & month == 12 & year == 2020]
#' pp = ggplot_dt(ex_dt)
#' if(interactive()) plot(pp)
#'
#' @export
#'
#' @author Claudio Heinrich


ggplot_dt = function(dt,
                     data_col = NULL,
                     mn = NULL,
                     discrete_cs = FALSE,
                     rr = NULL,
                     low = NULL,
                     mid = NULL,
                     high = NULL,
                     name = data_col,
                     midpoint = NULL,
                     breaks = NULL,
                     na.value = 'gray75',
                     oob = NULL,
                     guide = guide_colorbar(barwidth = 0.5, barheight = 10),
                     ...,
                     binwidth = NULL,bin_midpoint = midpoint,
                     add_map = TRUE,
                     extent = NULL,
                     expand.x = c(0,0),
                     expand.y = c(0,0))
{
  # for devtools::check():
  long = group = NULL

  if(is.null(oob)) oob = scales::squish # declaring this in the function-defaults results in a NOTE during check(), because scales is imported but never used

  if(!('lon' %in% names(dt) & 'lat' %in% names(dt))) stop('The data table has to contain columns called "lon" and "lat".')

  # detect the columns that indicate that there might be multiple levels to plot:
  level_cols = intersect(c(dimvars(),'member'),names(dt))
  # so far, level_cols includes lon and lat. This will be changed in a sec, but let's first find the column to plot if its not given:
  if(is.null(data_col))
  {
    data_col = ifelse(test = (length(level_cols) < ncol(dt)),
                      no = names(dt)[ncol(dt)], # last column if every column is level-column
                      yes = setdiff(names(dt),c(level_cols,'country'))[1])
  }

  if(data_col %in% tc_cols()) message('Check out the function tercile_cat() for plotting tercile-categories.')

  # now remove lon lat from level_cols and check whether dt contains multiple levels:
  level_cols = setdiff(level_cols,space_dimvars(dt))

  if(length(level_cols) > 0)
  {
    level_dt = unique(dt[,.SD,.SDcols = level_cols])
    if(level_dt[,.N] > 1)
    {
      output_string = 'The data table contains multiple levels per lon/lat. The following selection is plotted:\n'
      multiple_list = list()
      for(nn in names(level_dt))
      {
        temp = unique(level_dt[,get(nn)])
        if(length(temp) > 1)
        {
          output_string = paste0(output_string,nn,' = ',temp[1],'\n')
        }
      }
      warning(output_string)
      dt = merge(dt,level_dt[1],by = names(level_dt))
    }
  }

  # Plotting should still work for logicals (e.g. masks):
  if (is.logical(dt[,get(data_col)])) dt[,(data_col) := as.numeric(get(data_col))]

  # if color scale details are not provided, guess based on column name:

  High = Mid = Low = Midpoint = NULL

  if(data_col %in% c('prec','precipitation','anomaly','climatology','clim','obs','pred','fc','forecast'))
  {
    High = 'blue'
    Mid = 'white'
    Low = 'red'
    Midpoint = 0
  }

  if(data_col %in% c('terc_cat','tercile_cat','tercile_category'))
  {
    # select similar colors as for the verification map:
    High = 'forestgreen'
    Mid = "#B2EBF2" # This is cyan200 for the normal category
    Low = 'brown'
    Midpoint = 0
  }

  if(data_col == 'above')
  {
    High = 'forestgreen'
    Mid = 'white' # This is cyan200 for normal
    Low = 'brown'
    Midpoint = 0.333
  }

  if(data_col == 'below')
  {
    High = 'brown'
    Mid = 'white' # This is cyan200 for normal
    Low = 'forestgreen'
    Midpoint = 0.333
  }

  if(data_col == 'normal')
  {
    High = "cyan3"
    Mid = 'white' # This is cyan200 for normal
    Low = 'gold2'
    Midpoint = 0.333
  }

  # set defaults if not provided in the function:

  if(is.null(high))
  {
    if(is.null(High))
    {
      high = 'blue'
    } else {
      high = High
    }
  }

  if(is.null(mid))
  {
    if(is.null(Mid))
    {
      mid = 'white'
    } else {
      mid = Mid
    }
  }

  if(is.null(low))
  {
    if(is.null(Low))
    {
      low = 'red'
    } else {
      low = Low
    }
  }

  if(is.null(midpoint))
  {
    midpoint = Midpoint
  }


  #### get map: ####

  if(add_map) world_map <- ggplot2::map_data(map = 'world',resolution = 0)
  # better maps are available with the rnaturalearth package and can be plotted using geom_sf.
  # However, this approach requires gdal, so it's not exactly easily accessible.

  #### fix range and set values outside of range to the range border ####

  if(is.null(rr))
  {
    rr = range(dt[,get(data_col)],na.rm = T)
  }

  # set midpoint:
  if(is.null(midpoint))
  {
    midpoint = rr[1] + (rr[2]-rr[1])/2
    if(is.null(bin_midpoint))
    {
      bin_midpoint = midpoint
    }

  }

  # set colorscale:
  if(!discrete_cs)
  {
    colorscale = scale_fill_gradient2(low = low, mid = mid, high = high,
                                      name = name, limits = rr, midpoint = midpoint,
                                      na.value = na.value,oob = oob,
                                      guide = guide,...)
  }
  if(discrete_cs)
  {
    # select breaks:
    if(is.null(breaks) & is.null(binwidth)){
      breaks = waiver() # ggplot2-default:
    } else if(is.null(breaks) & !is.null(binwidth))
    {
      nbinapprox = floor((rr[2] - rr[1])/binwidth)
      bins1 = binwidth*(1/2 + (0:nbinapprox)) + bin_midpoint
      bins2 = -binwidth*(1/2 + (0:nbinapprox)) + bin_midpoint
      bins=  sort(unique(c(bins2,bins1)))
      breaks = round(bins[bins %between% rr],2)
    }
    # for discrete scales there used to be an issue where the boundary bins are shown wider in the legend,
    # see https://github.com/tidyverse/ggplot2/issues/4019. This was resolved in ggplot2 version 2.3.4.
    colorscale = scale_fill_steps2(low = low, mid = mid, high = high,
                                   name = name, limits = rr, midpoint = midpoint,
                                   breaks = breaks,
                                   na.value = na.value, oob = oob, guide = guide,...)

  }


  ### plotting ###

  if(is.null(extent))
  {
    xlim = range(dt[,lon],na.rm = T) + expand.x
    ylim = range(dt[,lat],na.rm = T) + expand.y
  } else {
    xlim = extent[1:2] + expand.x
    ylim = extent[3:4] + expand.y
  }

  pp = ggplot(data = dt) +
    geom_tile(aes(x = lon,y = lat, fill = get(data_col))) +
    colorscale +  # colorscale is specified above
    coord_fixed(xlim = xlim,
                ylim = ylim,
                expand = FALSE) + # restricts the plot to exactly the considered area to avoid weird borders
    #coord_sf(xlim = lon_range,ylim = lat_range,expand = FALSE) +       # restricts the plot to exactly the considered area to avoid weird borders
       xlab('lon') + ylab('lat') +
    theme(panel.background = element_rect(fill =na.value), # this is required in case a data table is passed that has 'truely' missing locations, i.e. that is not rectangular
          panel.grid = element_blank(),
          axis.ticks = element_line(), # add ticks...
          axis.text = element_text(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

  if(!is.null(mn)) pp = pp + ggtitle(mn)                               # add title, if given

  if(add_map)
  {
  pp = pp + geom_polygon(data = world_map,
                         mapping = aes(x = long,y = lat,group = group),
                         color = 'black',fill = NA,linewidth=0.25)                # add map
  }
  return(pp)
}

#' Plotting function with different map for Greater Horn of Africa
#'
#' @description This essentially wraps \code{\link{ggplot_dt}}, but uses a different map for borders.
#' The map is part of the package and is the one currently used during GHACOFs at ICPAC.
#'
#' @param ...,expand.x,expand.y passed to \code{\link{ggplot_dt}}
#'
#'@examples
#'dt = chirps_monthly[lon %between% c(30,40) & lat < 0 & month == 11 & year == 2020]
#'pp = gha_plot(dt)
#'if(interactive()) plot(pp)
#'
#'@export

gha_plot = function(...,expand.x = c(-0.5,0.5),expand.y = c(-0.5,2))
{
  long = group = NULL
  fn =  system.file("extdata", "GHA_map.csv", package="SeaVal")

  pp = ggplot_dt(...,add_map = FALSE,expand.x = expand.x,expand.y = expand.y)
  map = fread(fn)
  pp = pp + geom_polygon(data = map,
                         mapping = aes(x = long,y = lat,group = group),
                         color = 'black',fill = NA,linewidth=0.25)
  return(pp)
}


#'@rdname gha_plot
ggplot_dt_shf = function(...)
{
  lifecycle::deprecate_warn("1.1.1", "ggplot_dt_shf()", "gha_plot()")
  gha_plot(...)
}


#'@rdname gha_plot
ggplot_dt_gha_map = function(...)
{
  lifecycle::deprecate_warn("1.1.1", "ggplot_dt_gha_map()", "gha_plot()")
  gha_plot(...)
}



#' Function for plotting terciles
#'
#' @param dt data table
#' @param data_col Name of the column containing the observed tercile category
#' @param mn optional title for the plot.
#' @param low,mid,high colors for the three categories
#' @param name optional title for the colorscale
#' @param labels How to label the three categories
#' @param na.value How to color missing values
#' @param extent Optional vector of length 4 specifying the plotting borders in order xmin, xmax, ymin, ymax.
#' @param expand.x,expand.y How far should the plotting borders be extended (beyond the data range)?
#'
#' @examples
#' \donttest{
#' dt = combine(chirps_monthly[month == 12],tfc_from_efc(ecmwf_monthly[month == 12]))
#' p = tercile_plot(dt)
#' if(interactive()) plot(p)
#' }
#'
#' @export

tercile_plot = function(dt,
                        data_col = tc_cols(dt),
                        mn = NULL,
                        low = 'orange',
                        mid = 'cyan',
                        high = 'green1',
                        name = '',
                        labels = c('Wetter','Average','Drier'),
                        na.value = 'white',
                        extent = NULL,
                        expand.x = c(-0.5,0.5),expand.y = c(-0.5,2))
{
  # for devtools::check():
  long = group = dummy_col = NULL


  if(!('lon' %in% names(dt) & 'lat' %in% names(dt))) stop('The data table has to contain columns called "lon" and "lat".')

  # detect the columns that indicate that there might be multiple levels to plot:
  level_cols = intersect(c(dimvars(),'member'),names(dt))
  # so far, level_cols includes lon and lat. This will be changed in a sec, but let's first find the column to plot if its not given:
  if(is.null(data_col))
  {
    data_col = ifelse(test = (length(level_cols) < ncol(dt)),
                      no = names(dt)[ncol(dt)], # last column if every column is level-column
                      yes = setdiff(names(dt),level_cols)[1])
  }
  # now remove lon lat from level_cols:
  level_cols = setdiff(level_cols,space_dimvars(dt))

  if(length(level_cols) > 0)
  {
    level_dt = unique(dt[,.SD,.SDcols = level_cols])
    if(level_dt[,.N] > 1)
    {
      output_string = 'The data table contains multiple levels per lon/lat. The following selection is plotted:\n'
      multiple_list = list()
      for(nn in names(level_dt))
      {
        temp = unique(level_dt[,get(nn)])
        if(length(temp) > 1)
        {
          output_string = paste0(output_string,nn,' = ',level_dt[.N,get(nn)],'\n')
        }
      }
      warning(output_string)
      dt = merge(dt,level_dt[.N],by = names(level_dt))
    }
  }

  #### get map: ####

  fn =  system.file("extdata", "GHA_map.csv", package="SeaVal")

  map = fread(fn)

  # better maps are available with the rnaturalearth package and can be plotted using geom_sf.
  # However, this approach requires gdal, so it's not exactly easily accessible.

  #### fix range and set values outside of range to the range border ####

  cols = c(high,mid,low)
  names(cols) = c('1','0','-1')

  colorscale = scale_fill_manual(values = cols,name = name,labels = labels)

  # a bit hacked...
  dt[,dummy_col := factor(get(data_col),levels = c(1,0,-1))]
  dt[,(data_col) := NULL]
  setnames(dt,'dummy_col',data_col)

  ### plotting ###

  if(is.null(extent))
  {
    xlim = range(dt[,lon],na.rm = T) + expand.x
    ylim = range(dt[,lat],na.rm = T) + expand.y
  } else {
    xlim = extent[1:2] + expand.x
    ylim = extent[3:4] + expand.y
  }

  pp = ggplot(data = dt) +
    geom_tile(aes(x = lon,y = lat, fill = get(data_col))) +
    colorscale +
    geom_polygon(data = map,
                 mapping = aes(x = long,y = lat,group = group),
                 color = 'black',fill = NA,linewidth=0.25) +
    coord_fixed(xlim = xlim,
                ylim = ylim,
                expand = FALSE) + # restricts the plot to exactly the considered area to avoid weird borders
    #coord_sf(xlim = lon_range,ylim = lat_range,expand = FALSE) +       # restricts the plot to exactly the considered area to avoid weird borders
    xlab('lon') + ylab('lat') +                                              # remove default labels and background grid...
    theme(panel.background = element_rect(fill =na.value), # this is required in case a data table is passed that has 'truely' missing locations, i.e. that is not rectangular
          panel.grid = element_blank(),
          axis.ticks = element_line(), # add ticks...
          axis.text = element_text(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

  if(!is.null(mn)) pp = pp + ggtitle(mn)


  return(pp)
}

