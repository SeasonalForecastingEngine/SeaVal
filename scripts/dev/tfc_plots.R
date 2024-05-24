devtools::load_all()


chirps = load_chirps()
pred = ecmwf_monthly |> tfc_from_efc()

dt = combine(chirps,pred)

dt = add_tercile_cat(dt)




tfc_plot = function(dt,
                    discrete_cs = TRUE,
                    rmax = NULL,
                    below = 'brown',
                    normal = 'gold',
                    above = 'forestgreen',
                    na.value = 'gray75',
                    cs_names = c('below','normal','above'),
                    oob = NULL,
                    guide_barwidth = 0.5,
                    guide_barheight = 10,
                    legend_horizontal = FALSE,
                    binwidth = 'auto',
                    add_map = TRUE,
                    extent = NULL,
                    expand.x = c(0,0),
                    expand.y = c(0,0),
                    showplot = TRUE)
{
  if(is.null(oob)) oob = scales::squish # declaring this in the function-defaults results in a NOTE during check(), because scales is imported but never used



  #required colnames:
  req_cns = c('lon','lat','below','normal','above')
  if(any(!(req_cns %in% names(dt)))) stop('The data table has to contain columns named ',paste(req_cns,collapse = ', '),'.')

  # detect the columns that indicate that there might be multiple levels to plot:
  level_cols = intersect(c(dimvars(),'member'),names(dt))

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

pp = tfc_gha_plot(dt)
pp = tfc_gha_plot(dt,legend_horizontal = TRUE)
