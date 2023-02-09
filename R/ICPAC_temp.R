#' Function for plotting terciles
#'
#' @param dt data table
#' @param data_col Name of the column containing the observed tercile category
#' @param mn optional title for the plot.
#' @param low,mid,high colors for the three categories
#' @param name optional title for the colorscale
#' @param labels How to label the three categories
#' @param na.value How to color missing values
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
                        expand.x = c(-0.5,0.5),expand.y = c(-0.5,2))
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
          output_string = paste0(output_string,nn,' = ',level_dt[.N,get(nn)],'\n')
        }
      }
      warning(output_string)
      dt = merge(dt,level_dt[.N],by = names(level_dt))
    }
  }

  #### get map: ####

  fn = file.path(data_dir(),'GHA_map.csv')
  if(!file.exists(fn)) stop(paste0('For using this function, you need a file GHA_map.csv located in ',data_dir()))

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

  pp = ggplot(data = dt) +
    geom_tile(aes(x = lon,y = lat, fill = get(data_col))) +
    colorscale +
    geom_polygon(data = map,
                 mapping = aes(x = long,y = lat,group = group),
                 color = 'black',fill = NA,linewidth=0.25) +
    coord_fixed(xlim = range(dt[,lon] ,na.rm = T)+ expand.x,
                ylim = range(dt[,lat],na.rm = T) + expand.y,
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

#' Plotting function with different GHA-borders
#'
#' @description This loads a (pre-processed) shapefile from ICPAC containing the GHA-countries.
#'
#' @param ... passed to \link{\code ggplot_dt}
#'
#'@export

ggplot_dt_shf = function(...,expand.x = c(-0.5,0.5),expand.y = c(-0.5,2))
{
  fn = file.path(data_dir(),'GHA_map.csv')
  if(!file.exists(fn)) stop(paste0('For using this function, you need a file GHA_map.csv located in ',data_dir()))

  pp = ggplot_dt(...,add_map = FALSE,expand.x = expand.x,expand.y = expand.y)
  map = fread(fn)
  pp = pp + geom_polygon(data = map,
                         mapping = aes(x = long,y = lat,group = group),
                         color = 'black',fill = NA,linewidth=0.25)
  return(pp)
}


#' Function to create a mask of dry regions from CHIRPS
#'
#' @description A gridpoint is masked for a given season (either 'MAM', 'JJAS' or 'OND'), if, on average, less than 10% of the annual total of rainfall
#' occur during the season. This function loads CHIRPS data, and derives this mask as a data table of lon, lat coordinates, only containing
#' the coordinates that shouldn't be masked. You can apply the mask to an existing data table using dt = combine(dt,mask).
#'
#' @param season For which season do you want to calculate the mask? Needs to be either 'MAM', 'JJAS' or 'OND'.
#' @param clim_years Which years should be used to establish the mask?
#' @param version,resolution,us Passed to \link{\code{load_chirps}}. Which CHIRPS version do you want to use and on what resolution?
#'
#' @export
#'

get_mask = function( season,
                     clim_years = 1990:2020,
                     version = 'UCSB',
                     resolution = 'low',
                     us = (resolution == 'low'))

{
  ch_dir = file.path(chirps_dir(),version)
  if (us) ch_dir = file.path(ch_dir,'upscaled')
  if (!dir.exists(ch_dir)) stop("I didn't find the directory with the CHIRPS data. Have you downloaded/upscaled CHIRPS? If not, run download_chirps_monthly.")
  ch_dir = file.path(ch_dir,'masks')
  dir.create(ch_dir,showWarnings = FALSE)

  fn = file.path(ch_dir,paste0(season,'_',min(clim_years),'_',max(clim_years),'.csv'))

  if(file.exists(fn)) {
    dt = fread(fn)
    return(dt)
  } else {
    mms = season_months(season)
    dt = load_chirps(years = clim_years,months = NULL,us = us,version = version)
    clim = dt[,.(clim = mean(prec)),.(lon,lat)]
    season_average = dt[month %in% mms,.(season_average = mean(prec)),.(lon,lat)]
    dt = merge(clim,season_average,c('lon','lat'))
    dt[,fraction := (season_average*length(mms))/(clim*12)]
    dt[,mask:= fraction <= 0.1]
    #save:
    fwrite(dt[(!mask),.(lon,lat)],file = fn)
    return(dt[(!mask),.(lon,lat)])
  }
}


