
#' Auxiliary function for checking dimensions for map-plotting
#'
#' @param dt Data table containing the data for plotting
#' @param data_col Name of column containing the data for plotting

modify_dt_map_plotting = function(dt, data_col){
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
      if(! unique(dt[,.(lon,lat)])[,.N] == dt[,.N]) warning("After this selection there are still multiple values per lon/lat.")
    } else if(! unique(dt[,.(lon,lat)])[,.N] == dt[,.N]) warning("Multiple values per lon/lat detected.")

  } else if(! unique(dt[,.(lon,lat)])[,.N] == dt[,.N]) warning("Multiple values per lon/lat detected.")

  # Plotting should still work for logicals (e.g. masks):
  if (is.logical(dt[,get(data_col)])) dt[,(data_col) := as.numeric(get(data_col))]

  return(list(dt,data_col))
}


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
#' @param add_map logical, defaults to `TRUE`, mostly for internal use. Set to `FALSE` to remove borders (e.g. if you want to add them yourself from a shapefile).
#' @param extent An optional four-element vector in the order xmin,xmax,ymin,ymax for specifying the spatial extent of the plot. Default is to fit the extent to the data.
#' @param expand.x,expand.y vectors with two entries to be added to xlims/ylims of the plot. E.g. expand.x = c(-0.5,0.5)
#' expands the plot by half a longitude both on the right and left hand side
#' @param dimension_check Logical. By default the function checks that there are not multiple values per coordinate
#' (and subsets to the first level if there are several, e.g. to the first year and month (by appearance in `dt`) if `dt` contains data for several years and months).
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
                     expand.y = c(0,0),
                     dimension_check = TRUE)
{
  # for devtools::check():
  long = group = NULL

  if(dimension_check) {
    temp = modify_dt_map_plotting(dt = dt, data_col = data_col)
    dt = temp[[1]]
    data_col = temp[[2]]
  }

  if(data_col %in% tc_cols()) message('Check out the function tercile_plot() for plotting tercile-categories.')
  if(data_col %in% c("above","below","normal")) message('Check out the function tfc_plot() for plotting tercile-forecasts.')

  if(is.null(oob)) oob = scales::squish # declaring this in the function-defaults results in a NOTE during check(), because scales is imported but never used

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
#' @param dimension_check Logical. By default the function checks that there are not multiple values per coordinate
#' (and subsets to the first level if there are several, e.g. to the first year and month (by appearance in `dt`) if `dt` contains data for several years and months).
#'
#' @examples
#' dt = chirps_monthly[month == 12 & lat <0 & year == 2018]
#' p = tercile_plot(dt = dt)
#' if(interactive()) plot(p)
#'
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
                        expand.x = c(-0.5,0.5),expand.y = c(-0.5,2),
                        dimension_check = TRUE)
{
  # for devtools::check():
  long = group = dummy_col = NULL


  if(dimension_check) {
    temp = modify_dt_map_plotting(dt = dt, data_col = data_col)
    dt = temp[[1]]
    data_col = temp[[2]]
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


#' plotting function for tercile forecasts
#'
#' @description Plots spatial maps of tercile forecasts. Requires a data table with three columns `"below"`, `"normal"`, `"above"`
#' which sum to 1. For each gridpoint only the highest of the three values is plotted, so there are three colorscales.
#'
#' @param dt Data table containing the data for plotting.
#' @param discrete_cs Logical. Do you want to use discrete color scales (default) or not.
#' @param below,normal,above Colors to use for the three categories. Default is `'brown'`, `'gold'`, `'forestgreen'`.
#' @param na.value Color to use for missing value. Default is `'gray75'`.
#' @param cs_names Character vector of length three giving the legend titles for the below-, normal-, and above category.
#' @param oob Behavior for data above `r_max`. Passed to [ggplot2::scale_fill_continuous()] if `discrete_cs == FALSE` or else to [ggplot2::scale_fill_steps()].
#' @param rmax Optional value to fix the range of the colorscale (lower limit is always 0.33).
#' @param guide_barwidth,guide_barheight value to specify the width and height of the color guide. Are flipped if `legend_horizontal` is `TRUE`. Use `units(...,"npc")` to make it work across all output devices.
#' @param legend_horizontal Logical. Set to `TRUE` to show the legend horizontally underneath the plot.
#' @param binwidth Width of the steps when a discrete colorscale is used.
#' @param add_map logical, defaults to `TRUE`, mostly for internal use. Set to `FALSE` to remove borders (e.g. if you want to add them yourself from a shapefile).
#' @param extent An optional four-element vector in the order xmin,xmax,ymin,ymax for specifying the spatial extent of the plot. Default is to fit the extent to the data.
#' @param expand.x,expand.y vectors with two entries to be added to xlims/ylims of the plot. E.g. expand.x = c(-0.5,0.5)
#' expands the plot by half a longitude both on the right and left hand side.
#' @param showplot Logical. Should the plot be displayed at the end?
#' @param dimension_check Logical. By default the function checks that there are not multiple values per coordinate
#' (and subsets to the first level if there are several, e.g. to the first year and month (by appearance in `dt`) if `dt` contains data for several years and months).
#'
#' @return a ggplot object.
#'
#' @import data.table
#'
#' @examples
#' #dt = tfc_from_efc(ecmwf_monthly[month == 11 & lat < 0])
#' #pp = tfc_plot(dt[year == 2018])
#' #if(interactive()) plot(pp)
#'
#' @export
#'
#' @author Claudio Heinrich
#' @export

tfc_plot = function(dt,
                    discrete_cs = TRUE,
                    rmax = NULL,
                    below = 'brown',
                    normal = 'gold',
                    above = 'forestgreen',
                    na.value = 'gray75',
                    cs_names = c('below','normal','above'),
                    oob = NULL,
                    guide_barwidth = grid::unit(0.01, units = "npc"),
                    guide_barheight = grid::unit(0.15, units = "npc"),
                    legend_horizontal = FALSE,
                    binwidth = 'auto',
                    add_map = TRUE,
                    extent = NULL,
                    expand.x = c(0,0),
                    expand.y = c(0,0),
                    showplot = TRUE,
                    dimension_check = TRUE)
{
  if(is.null(oob)) oob = scales::squish # declaring this in the function-defaults results in a NOTE during check(), because scales is imported but never used

  #required colnames:
  req_cns = c('lon','lat','below','normal','above')
  if(any(!(req_cns %in% names(dt)))) stop('The data table has to contain columns named ',paste(req_cns,collapse = ', '),'.')


  if(dimension_check) dt = modify_dt_map_plotting(dt = dt, data_col = "below")[[1]] # second page contains 'data_col' which we don't need here...


  # only keep largest value between below, normal and above:

  dt[below <= pmax(normal,above),below:= NA_real_]
  dt[above <= pmax(below,normal,na.rm = TRUE),above:= NA_real_]
  dt[normal <= pmax(below,above,na.rm = TRUE),normal:= NA_real_]


  #### get map: ####

  if(add_map) world_map <- ggplot2::map_data(map = 'world',resolution = 0)
  # better maps are available with the rnaturalearth package and can be plotted using geom_sf.
  # However, this approach requires gdal, so it's not exactly easily accessible.

  #### fix range and set values outside of range to the range border ####

  if(is.null(rmax))
  {
    rmax = max(dt[,below],dt[,normal],dt[,above],na.rm = TRUE)
  }

  rmin = ifelse(discrete_cs,yes = 0.3, no = 0.33)

  if(legend_horizontal){
    temp = guide_barheight
    guide_barheight = guide_barwidth
    guide_barwidth = temp
  }

  # set colorscales:
  if(!discrete_cs)
  {
    guide = guide_colorbar(barwidth = guide_barwidth, barheight = guide_barheight)
    cs_below = scale_fill_gradient(low = 'white', high = below,
                                   name = cs_names[1],
                                   limits = c(rmin,rmax),
                                   na.value = NA,
                                   oob = oob,
                                   guide = guide)
    cs_normal = scale_fill_gradient(low = 'white', high = normal,
                                    name = cs_names[2],
                                    limits = c(rmin,rmax),
                                    na.value = NA,
                                    oob = oob,
                                    guide = guide)
    cs_above = scale_fill_gradient(low = 'white', high = above,
                                   name = cs_names[3],
                                   limits = c(rmin,rmax),
                                   na.value = NA,
                                   oob = oob,
                                   guide = guide)
  }
  if(discrete_cs)
  {
    if(identical(binwidth,'auto')) binwidth = ifelse(rmax >=0.7,yes = 0.1,no = 0.05)
    breaks = seq(rmin,rmax,by = binwidth)
    guide = guide_colorsteps(barwidth = guide_barwidth, barheight = guide_barheight)

    cs_below = scale_fill_steps(low = 'white', high = below,
                                name = cs_names[1],
                                breaks = breaks,
                                limits = c(rmin,rmax),
                                na.value = NA,
                                oob = oob,
                                guide = guide)
    cs_normal = scale_fill_steps(low = 'white', high = normal,
                                 name = cs_names[2],
                                 breaks = breaks,
                                 limits = c(rmin,rmax),
                                 na.value = NA,
                                 oob = oob,
                                 guide = guide)
    cs_above = scale_fill_steps(low = 'white', high = above,
                                name = cs_names[3],
                                breaks = breaks,
                                limits = c(rmin,rmax),
                                na.value = NA,
                                oob = oob,
                                guide = guide)

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
    geom_tile(aes(x = lon,y = lat, fill = below)) +
    cs_below +
    ggnewscale::new_scale_fill() +
    geom_tile(aes(x = lon,y = lat, fill = normal)) +
    cs_normal +
    ggnewscale::new_scale_fill() +
    geom_tile(aes(x = lon,y = lat, fill = above)) +
    cs_above +
    coord_fixed(xlim = xlim,
                ylim = ylim,
                expand = FALSE) + # restricts the plot to exactly the considered area to avoid weird borders
    #coord_sf(xlim = lon_range,ylim = lat_range,expand = FALSE) +       # restricts the plot to exactly the considered area to avoid weird borders
    xlab('lon') + ylab('lat')

  if(legend_horizontal){
    pp = pp +
      theme(panel.background = element_rect(fill =na.value), # this is required in case a data table is passed that has 'truely' missing locations, i.e. that is not rectangular
            panel.grid = element_blank(),
            axis.ticks = element_line(), # add ticks...
            axis.text = element_text(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
            legend.position = 'bottom',legend.direction = 'horizontal')
  } else {
    pp = pp +
      theme(panel.background = element_rect(fill =na.value), # this is required in case a data table is passed that has 'truely' missing locations, i.e. that is not rectangular
            panel.grid = element_blank(),
            axis.ticks = element_line(), # add ticks...
            axis.text = element_text(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
  }


  if(add_map)
  {
    pp = pp + geom_polygon(data = world_map,
                           mapping = aes(x = long,y = lat,group = group),
                           color = 'black',fill = NA,linewidth=0.25)                # add map
  }
  if(interactive() & showplot) plot(pp)
  return(pp)
}



#' Plotting function with different map for Greater Horn of Africa
#'
#' @description This function wraps [tfc_plot()], but uses a different map for borders.
#' The map is part of the package and is the one currently used during GHACOFs at ICPAC.
#'
#' @param ...,expand.x,expand.y,showplot passed to [tfc_plot()].
#'
#'@examples
#' dt = tfc_from_efc(ecmwf_monthly[month == 11 & lat < 0])
#' pp = tfc_gha_plot(dt[year == 2018], expand.y = c(0.5,0.5))
#' if(interactive()) plot(pp)
#'
#'@export
tfc_gha_plot = function(...,expand.x = c(-0.5,0.5),expand.y = c(-0.5,2),showplot = TRUE)
{
  fn =  system.file("extdata", "GHA_map.csv", package="SeaVal")

  pp = tfc_plot(...,add_map = FALSE,expand.x = expand.x,expand.y = expand.y,showplot = FALSE)
  map = fread(fn)
  pp = pp + geom_polygon(data = map,
                         mapping = aes(x = long,y = lat,group = group),
                         color = 'black',fill = NA,linewidth=0.25)

  if(interactive() & showplot) plot(pp)
  return(pp)
}


###########################################################
##################  Verification Maps  ####################
###########################################################

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
#' @return a gg object
#'
#' @importFrom ggplotify as.ggplot
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \donttest{
#' # takes some time:
#' pp = ver_map(chirps_monthly[month == 11],yy = 2018)
#' if(interactive()) plot(pp)
#'}
#' @export

ver_map = function(dt,o = obs_cols(dt),yy = dt[,max(year)],
                   climatology_period = unique(dt[,year]),
                   out_file = NULL)

{

  # for devtools::check()
  N = dummy_var = lon = lat = is_yy = sample_quantile = how_many_ties = NULL

  if(!('sample_quantile' %in% names(dt))) # this allows us to send in a more prepared data table instead, with precalculated sample quantiles.
  {
    if(length(intersect( names(dt),o)) == 0)
    {
      stop('Please specify which column contains your observation.')
    }

    o = o[min(which(o %in% names(dt)))]

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

    # check whether there are locations for which not every year has data:
    check_dt = dt_temp[,.N,by = .(lon,lat)][N!=max(N),]
    if(check_dt[,.N] > 0)
    {
      warning(paste0(check_dt[,.N],' locations only have data for some years and have been removed.'))
      # a bit hacked:
      locs = check_dt[,.(lon,lat)][,dummy_var := 1]
      dt_temp = merge(dt_temp,locs,by = c('lon','lat'),all = T)
      dt_temp = dt_temp[is.na(dummy_var)][,dummy_var := NULL]
    }

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
  } else {dt_temp = dt}


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
    is_legend <- which(gt$layout$name == "guide-box-bottom")
    legend <- gt$grobs[is_legend][[1]]
    legend <- legend$grobs[legend$layout$name == "guides"][[1]]
    # Set widths in guide gtable
    width <- as.numeric(legend$widths[3]) # save bar width (assumes 'cm' unit)
    legend$widths[3] <- unit(1, "null") # replace bar width


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

#' Plot a verification map of percentiles based on precomputed CHIRPS quantiles.
#'
#' The quantiles should be computed and saved by the function \code{chirps_ver_map_quantiles}.
#'
#' @param yy,mm The year and month for which to show the verification map. Defaults to the month 60 days ago (in order to avoid using preliminary data).
#' @param version which CHIRPS version to use.
#' @param resolution Spatial resolution, 'high' or 'low'
#' @param ... passed on to ver_map.
#'
#' @return A gg object
#'
#' @importFrom ggplotify as.ggplot
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \donttest{ # takes a while:
#' if(interactive()) ver_map_chirps(mm = 12,yy = 2022)
#' }
#'
#' @export

ver_map_chirps = function(mm = month(Sys.Date()-60),
                          yy = year(Sys.Date()-60),
                          version = 'UCSB',resolution = 'low',...)
{
  res = sample_quantile = prec = q0.1 = q0.2 = q0.33 = q0.67 = q0.8 = q0.9 = NULL
  dt = load_chirps(years = yy,
                   months = mm,
                   version = version,
                   resolution = resolution)

  # get directory where quantiles are stored:
  quantile_dir = file.path(chirps_dir(),version)
  if(resolution == 'low') quantile_dir = file.path(quantile_dir,'upscaled')
  quantile_dir = file.path(quantile_dir,'quantiles')

  fn = list.files(quantile_dir,pattern = 'ver_map_quantiles')
  # for now exclude the seasonal file:
  fn = grep(fn, pattern='seasonal', invert=TRUE, value=TRUE)

  if(length(fn)>1) warning(paste0('There are multiple quantile files for verification map in the target directory, I am picking\n',fn[1],'\nThe target directory is ',quantile_dir))
  fn = fn[1]

  load(file.path(quantile_dir,fn))
  qdt = res$dt[month == mm]

  dt = merge(dt,qdt,by = c('lon','lat','month'))


  levels = c(0.1,0.2,0.33,0.67,0.8,0.9,1)
  level_diffs = levels[2:7]-levels[1:6]
  sample_quant = function(prec,q0.1,q0.2,q0.33,q0.67,q0.8,q0.9)
  {
    res = rep(0.099,length(prec))# just under 0.1
    res = res + level_diffs[1]*(prec>=q0.1) +
      level_diffs[2]*(prec>=q0.2) +
      level_diffs[3]*(prec>=q0.33) +
      level_diffs[4]*(prec>=q0.67) +
      level_diffs[5]*(prec>=q0.8) +
      level_diffs[6]*(prec>=q0.9)
    return(res)
  }

  dt[,sample_quantile:=sample_quant(prec,q0.1,q0.2,q0.33,q0.67,q0.8,q0.9)]
  dt[,sample_quantile := 100*sample_quantile]
  dt[sample_quantile == 9.9,sample_quantile:=0]

  begin_year = as.numeric(strsplit(fn,'_')[[1]][4])
  end_year = as.numeric(strsplit(strsplit(fn,'_')[[1]][6],'\\.')[[1]][1])

  message('plotting verification map for ',yy,'/',mm,'\nClimatology reference is ',begin_year,'-',end_year)
  vm = ver_map(dt,...) + ggtitle(paste0('     ',mm,'/',yy))
  return(vm)
}
