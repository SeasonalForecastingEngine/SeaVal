#' plotting function for spatial data
#'
#' @description Plots spatial data from a data.table. The data table needs to contain columns named 'lon' and 'lat'. The grid needs to be regular.
#' If spatial data is contained for several levels (e.g. mutliple times or multiple ensemble members), only the first is plotted.
#' By default, the first column that is not recognized as a dimension variable is plotted, see \code{data_col}. If the function recognizes the name
#' of the column containing the plotting data, it sets a fitting default color scale.
#'
#' @param dt Data table containing the data for plotting.
#' @param data_col The name of the column in dt containing the data for plotting. If nothing is provided (the default), the first column that is not a dimension variable or 'member' is selected.
#' @param mn optional plot title
#' @param discrete_cs Logical. Should the color scale be discretized?
#' @param midpoint midpoint of the color scale, passed to scale_fill_gradient2 or scale_fill_steps2 (depending on whether discrete_cs == TRUE).
#' This is 0 by default. If you set it to NULL, the midpoint is set to the center of the data range (or the center of rr, if provided), such that the entire color scale is utilized.
#' Specifying the midpoint can often be a convenient way to force a color scale with only two colors (for example, by setting it
#' to the minimum or maximum of your data).
#' @param rr,low,mid,high,name,midpoint,breaks,na.value,oob,guide,... Arguments for the color scale, passed to scale_fill_gradient2 or scale_fill_steps2 (depending on whether discrete_cs == TRUE).
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
#'
#' @return a ggplot object.
#'
#' @import data.table
#' @import ggplot2
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
                     oob = scales::squish,
                     guide = guide_colorbar(barwidth = 0.5, barheight = 10),
                     ...,
                     binwidth = NULL,bin_midpoint = midpoint,
                     add_map = TRUE)
{
  # for devtools::check():
  long = group = NULL

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
          output_string = paste0(output_string,nn,' = ',temp[1],'\n')
        }
      }
      warning(output_string)
      dt = merge(dt,level_dt[1],by = names(level_dt))
    }
  }

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
    Mid = "#B2EBF2" # This is cyan200 for normal
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
    if(is.null(Midpoint))
    {
      midpoint = 0
    } else {
      midpoint = Midpoint
    }
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
    if(!is.null(binwidth))
    {
      nbinapprox = floor((rr[2] - rr[1])/binwidth)
      bins1 = binwidth*(1/2 + (0:nbinapprox)) + bin_midpoint
      bins2 = -binwidth*(1/2 + (0:nbinapprox)) + bin_midpoint
      bins=  sort(unique(c(bins2,bins1)))
      bins = round(bins[bins %between% rr],2)

      # for discrete scales there used to be an issue where the boundary bins are shown wider in the legend,
      # see https://github.com/tidyverse/ggplot2/issues/4019. This was resolved in ggplot2 version 2.3.4.

      colorscale = scale_fill_steps2(low = low, mid = mid, high = high,
                                     name = name, limits = rr, midpoint = midpoint,
                                     breaks = bins,
                                     na.value = na.value, oob = oob, guide = guide,...)
    }
    if(is.null(binwidth))
    {
      colorscale = scale_fill_steps2(low = low, mid = mid, high = high,
                                     name = name, limits = rr, midpoint = midpoint,
                                     na.value = na.value, oob = oob,
                                     guide = guide, ...)
    }
  }


  ### plotting ###

  pp = ggplot(data = dt) +
    geom_tile(aes(x = lon,y = lat, fill = get(data_col))) +
    colorscale +  # colorscale is specified above
    coord_fixed(xlim = range(dt[,lon],na.rm = T),
                ylim = range(dt[,lat],na.rm = T),
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


