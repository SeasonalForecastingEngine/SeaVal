#' Function for matching data between different grids
#'
#' @description
#' Upscales data from one regular lon-lat grid to another lon-lat grid that is coarser or of the same resolution.
#' It uses conservative interpolation (rather than bilinear interpolation) which is the better choice for upscaling, see details below.
#' If the fine grid and coarse grid are of the same resolution but shifted, results are (almost) identical to bilinear interpolation
#' (almost because bilinear interpolation does not account for the fact that grid cells get smaller towards the pole, which this function does).
#'
#' The function addresses the following major challenges:
#' \itemize{
#' \item The fine grid does not need to be nested in the coarse grid, creating different partial overlap scenarios.
#' Therefore, the value of each fine grid cell may contribute to multiple (up to four) coarse grid cells.
#' \item Grid cell area varies with latitude, grid cells at the equator are much larger than at the poles.
#' This affects the contribution of grid cells (grid cells closer to the pole contribute less to the coarse grid cell average).
#' \item Frequently, it is required to upscale \emph{repeated} data between the same grids, for example when you want to upscale observations for many different years.
#' In this case, the calculation of grid cell overlaps is only done once, and not repeated every time.
#' \item For coarse grid cells that are only partially covered, a minimal required fraction of coverage can be specified.
#' \item It is memory efficient: Naive merging of data tables or distance-based matching of grid cells is avoided, since it results in
#' unnecessary large lookup tables that may not fit into memory when both your fine and your coarse grid are high-resolution.
#' }
#'
#' @details Bilinear interpolation is generally not appropriate for mapping data from finer to coarser grids.
#' The reason is that in BI, the value of a coarse grid cell only depends on the four fine grid cells surrounding its center coordinate,
#' even though many fine grid cells may overlap the coarse grid cell).
#' Conservative interpolation calculates the coarse grid cell value by averaging all fine grid cells overlapping with it, weighted by
#' the fraction of overlap. This is the appropriate way of upscaling when predictions and observations constitute grid point averages, which is usually the case (Göber et al. 2008).
#'
#' The grids are assumed to be *regular*, but are not required to be *complete* (see \code{\link{set_spatial_grid}}).
#' The function is faster when missing-data grid points are not contained in `dt` (then fewer grid points need to be matched).
#'
#'
#' @param dt data table containing the data you want to upscale.
#' @param coarse_grid data table containing lons/lats of the grid you want to upscale to.
#' @param uscols column name(s) of the data you want to upscale (can take multiple columns at once, but assumes that the different columns have missing values at the same position).
#' @param bycols optional column names for grouping if you have repeated data on the same grid, e.g. use bycols = 'date' if your data table contains observations for many dates on the same grid (and the column specifying the date is in fact called 'date').
#' @param save_weights optional file name for saving the weights for upscaling. Used for the CHIRPS data.
#' @param req_frac_of_coverage Numeric value between 0 and 1. All coarse grid cells with less coverage than this value get assigned a missing value. In particular, setting this to 0 (the default) means a value is assigned to each coarse grid cell
#' that overlaps with at least one fine grid cell. Setting this to 1 means only coarse grid cells are kept for which we have full coverage.
#'
#' @return A data table with the upscaled values.
#'
#' @references Göber, M., Ervin Z., and Richardson, D.S. (2008): *"Could a perfect model ever satisfy a naïve forecaster? On grid box mean versus point verification."* Meteorological Applications: A journal of forecasting, practical applications, training techniques and modelling 15, no. 3 (2008): 359-365.
#' @export

upscale_regular_lon_lat = function(dt,
                                   coarse_grid,
                                   uscols,
                                   bycols = setdiff(dimvars(dt),c('lon','lat')),
                                   save_weights = NULL,
                                   req_frac_of_coverage = 0)
{
  # for devtools::check():
  lon = lat = fg_lon = fg_lat = fg_lon_min = fg_lon_max = NULL
  fg_lat_min = fg_lat_max = fg_index = NULL
  index = overlap_type = cg_lon_min = cg_lon_max = cg_lat_min = NULL
  cg_lat_max = mt = area = area_contr = NULL
  cg_area = foc = NULL

  set_spatial_grid(dt)
  set_spatial_grid(coarse_grid)

  # kick out all rows that have a missing value in one of the uscols
  na_inds = is.na(rowSums(dt[,.SD,.SDcols = uscols]))
  dt = dt[(!na_inds)]

  # checks for regularity and correct column names:

  if(!identical(attr(dt,'grid')$coor_cns,c('lon','lat'))) stop('The data you want to upscale does not seem to be on a lon/lat grid. Potentially you need to change column names to "lon", "lat".')
  if(!attr(dt,'grid')$regular) stop('The data you provided does not seem to be on a regular grid, see ?set_spatial_grid for details.')


  if(!attr(coarse_grid,'grid')$regular) stop('The upscale-grid you provided does not seem to be regular grid, see ?set_spatial_grid for details.')
  if(!identical(attr(coarse_grid,'grid')$coor_cns,c('lon','lat'))) {

    warning_str = paste0('The grid I am supposed to upscale to has column names ',paste(attr(coarse_grid,'grid')$coord_cns,collapse = ', '),' rather than "lon", "lat".
  \nUpscaling is only allowed to lon/lat grids.')
    if(!interactive()){
      stop(warning_str)
    } else {
      menu_title = paste0(warning_str,' Should I change column names of the coarse grid to "lon","lat"?')
      check = menu(choices = c('yes','no'),title = menu_title)
      if(check == 1){
        grid = attr(coarse_grid,'grid')$grid
        setnames(coarse_grid,
                 old = grid$coor_cns,
                 new = c('lon','lat'))
        #update grid attribute:
        grid$coor_cns = c('lon','lat')
        setattr(coarse_grid,'grid',grid)
      }
      if(check == 2)stop('aborted. Please run again with vaid coarse grid.')
    }
  }

  if((attr(dt,'grid')$dx > attr(coarse_grid,'grid')$dx) |
     (attr(dt,'grid')$dy > attr(coarse_grid,'grid')$dy)) stop('"Upscaling" from coarser to finer grids not allowed.')

  ### let's get started: ###

  save_key_dt = key(dt)

  setkeyv(dt,c(bycols,'lon','lat'))

  # check whether for each coordinate and each instance of bycols only one value is provided, otherwise the user likely forgot to specify bycols correctly:
  if(length(bycols) == 0)
  {
    if(dt[,.N] != unique(dt[,.(lon,lat)])[,.N]) stop('The data table contains multiple values per coordinate. Did you forget to specify bycols?')
  } else {
    if(dt[,.N] != unique(dt[,.SD,.SDcols = c('lon','lat',bycols)])[,.N]) stop(paste('The data table contains multiple values per coordinate and level of bycols.',
                                                                                    paste0('Your bycols are ',paste(bycols,collapse = ', '),'.'),
                                                                                    'Maybe you forgot to include a grouping variables in there?',sep = '\n'))
  }


  fine_grid = unique(dt[,.(lon,lat)])
  coarse_grid = unique(coarse_grid[,.(lon,lat)])

  setkey(fine_grid,lon,lat)
  setkey(coarse_grid,lon,lat)

  cg_lons = sort(unique(coarse_grid[,lon]))
  cg_lats = sort(unique(coarse_grid[,lat]))
  fg_lons = sort(unique(fine_grid[,lon]))
  fg_lats = sort(unique(fine_grid[,lat]))

  # get grid size of fine grid
  d_lons = fg_lons[2:length(fg_lons)] - fg_lons[1:(length(fg_lons)-1)]
  d_lats = fg_lats[2:length(fg_lats)] - fg_lats[1:(length(fg_lats)-1)]

  d_lon = min(d_lons)
  d_lat = min(d_lats)

  # corner point coordinates for the fine grid:
  fg_lon_mins = fg_lons - d_lon/2
  fg_lon_maxs = fg_lons + d_lon/2
  fg_lat_mins = fg_lats - d_lat/2
  fg_lat_maxs = fg_lats + d_lat/2

  # same for coarse grid:

  D_lon = min(cg_lons[2:length(cg_lons)] - cg_lons[1:(length(cg_lons) - 1)])
  D_lat = min(cg_lats[2:length(cg_lats)] - cg_lats[1:(length(cg_lats) - 1)])

  cg_lon_mins = cg_lons - D_lon/2
  cg_lon_maxs = cg_lons + D_lon/2
  cg_lat_mins = cg_lats - D_lat/2
  cg_lat_maxs = cg_lats + D_lat/2


  #### For each corner-point of the fine grid, find in which grid-cell of the coarse grid it is contained (if any). ###

  find_index_lon = function(fine_lons)
  {
    ret_list = lapply(fine_lons,FUN = function(x) which(x >= cg_lon_mins & x <= cg_lon_maxs))

    # if you just unlist this, the coordinates that don't match any coarse grid cell are getting lost, so we need to do the following:
    is_null = unlist(lapply(ret_list,length)) == 0
    ret_list[is_null] = 0
    ret_list= lapply(ret_list, `[[`, 1) # some fine-grid-corner-coordinates may be assigned to multiple coarse grid coordinates due to rounding errors, and the tol-expansion.
    # In this case it does not matter which of the grid cell we assign it to, this hack assigns it to the first one.

    return(unlist(ret_list))
  }

  find_index_lat = function(fine_lats)
  {
    ret_list = lapply(fine_lats,FUN = function(x) which(x >= cg_lat_mins & x <= cg_lat_maxs))

    # if you just unlist this, the coordinates that don't match any coarse grid cell are getting lost, so we need to do the following:
    is_null = unlist(lapply(ret_list,length)) == 0
    ret_list[is_null] = 0
    ret_list  =lapply(ret_list, `[[`, 1) # see above

    return(unlist(ret_list))
  }



  lon_min_index = find_index_lon(fg_lon_mins)
  lon_max_index = find_index_lon(fg_lon_maxs)
  lat_min_index = find_index_lat(fg_lat_mins)
  lat_max_index = find_index_lat(fg_lat_maxs)

  ####
  #Now, a small grid cell overlaps with the i-th large grid cell, if (and only if) at least one of its corner points lies in that grid cell.

  fg_full = as.data.table(expand.grid(fg_lon = fg_lons,fg_lat = fg_lats))
  setkey(fg_full,fg_lon,fg_lat)

  # add cornerpoints:
  fg_full[, fg_lon_min := rep(fg_lon_mins,each = length(fg_lats))]
  fg_full[, fg_lon_max := rep(fg_lon_maxs,each = length(fg_lats))]
  fg_full[, fg_lat_min := rep(fg_lat_mins,length(fg_lons))]
  fg_full[, fg_lat_max := rep(fg_lat_maxs,length(fg_lons))]


  fg_full[, lon_min_index := rep(lon_min_index,each = length(fg_lats))]
  fg_full[, lon_max_index := rep(lon_max_index,each = length(fg_lats))]
  fg_full[, lat_min_index := rep(lat_min_index,length(fg_lons))]
  fg_full[, lat_max_index := rep(lat_max_index,length(fg_lons))]

  fg_full[,fg_index := 1:.N]

  # now, we may only keep working the fine grid points for which we actually have data:
  setnames(fine_grid,c('lon','lat'),c('fg_lon','fg_lat'))
  fg_full = merge(fg_full,fine_grid,by = c('fg_lon','fg_lat'))


  # get the row-index of the cg_full (see below) associated with the coarse grid point in which the lower left corner point lies:
  fg_full_lower_left = copy(fg_full)[,index := (lon_min_index != 0 & lat_min_index != 0 )*(lat_min_index + (lon_min_index-1)*(length(cg_lats)))]
  fg_full_lower_left[,overlap_type := 1]
  # the overlap_type helps us keeping track of 'duplicates', i.e. fine grid cells for which multiple corner points lie in the same coarse grid cell
  # (but they still should only contribute once...).

  # lower right corner point:
  fg_full_lower_right = copy(fg_full)[,index := (lon_max_index != 0 & lat_min_index != 0 )*(lat_min_index + (lon_max_index-1)*(length(cg_lats)))]
  fg_full_lower_right[,overlap_type := 2]
  # upper left:
  fg_full_upper_left = copy(fg_full)[,index := (lon_min_index != 0 & lat_max_index != 0 )*(lat_max_index + (lon_min_index-1)*(length(cg_lats)))]
  fg_full_upper_left[,overlap_type := 3]
  # upper right:
  fg_full_upper_right = copy(fg_full)[,index := (lon_max_index != 0 & lat_max_index != 0 )*(lat_max_index + (lon_max_index-1)*(length(cg_lats)))]
  fg_full_upper_right[,overlap_type := 4]

  matched_grids = rbindlist(list(fg_full_lower_left,
                                 fg_full_lower_right,
                                 fg_full_upper_left,
                                 fg_full_upper_right))

  # get full coarse grid:
  cg_full = as.data.table(expand.grid(lon = cg_lons,lat = cg_lats))
  setkey(cg_full,lon,lat)
  # add cornerpoints to calculate area of overlap
  cg_full[, cg_lon_min := rep(cg_lon_mins,each = length(cg_lats))]
  cg_full[, cg_lon_max := rep(cg_lon_maxs,each = length(cg_lats))]
  cg_full[, cg_lat_min := rep(cg_lat_mins,length(cg_lons))]
  cg_full[, cg_lat_max := rep(cg_lat_maxs,length(cg_lons))]

  cg_full[,index := 1:.N] # this index is what matches with the index in matched_grids:
  matched_grids = merge(matched_grids,cg_full,by = 'index')

  # Each row in matched_grids resembles a corner point of the fine grid and matches it to the grid cell in coarse grid in which it is contained.
  # We want a data table that show us which fine cells overlap with which coarse cells. Overlapping means that at least one fine-grid-corner-point
  # is contained in the coarse grid cell, but there are coarse grid cells containing multiple corners of the same fine grid cell, so we need to do the following:

  matched_grids = matched_grids[,mt := min(overlap_type), by = .(index,fg_index)][overlap_type == mt][,mt := NULL]

  #### calculate area overlaps: ####

  # for a rectangle with corner coordinates lon_min, lon_max,lat_min,lat_max the area is (lon_max - lon_min)*(sin(lat_max) - sin(lat_min)), when lat is given in radians.
  # We thus just may convert lats into sin(lats), and then calculate areas of rectangles. (This is a one-to-one mapping, since the angle is between -90 and +90 degree, where sin is increasing).
  # Also note that we do not need dx and dy to be compatible (which would require to convert dx into proper angle distances...),
  # because we ultimately do not care about the AREA of overlap, but only about the FRACTION of overlap, like, (area of overlap)/(unit area), see below.

  # convert lats to sin(lats):
  matched_grids[,c('fg_lat_min','fg_lat_max','cg_lat_min','cg_lat_max') := lapply(.SD,function(x) sin(pi*x/180)),.SDcols = c('fg_lat_min','fg_lat_max','cg_lat_min','cg_lat_max')]
  # calculate area of overlap:
  matched_grids[,area := (pmin(cg_lon_max,fg_lon_max) - pmax(cg_lon_min,fg_lon_min)) * (pmin(cg_lat_max,fg_lat_max) - pmax(cg_lat_min,fg_lat_min))]
  # area contribution per coarse grid cell:
  matched_grids[,area_contr:= area/sum(area),by = index] # this is why we don't need to care about the absolute unit of area, we only need it for our weights for averaging.

  # for dealing with partially covered gridcells, calculate the fraction of each coarse grid cell that is covered:
  matched_grids[,cg_area := (cg_lon_max - cg_lon_min) * (cg_lat_max - cg_lat_min),by = index]
  matched_grids[,foc := sum(area)/cg_area,by = index]

  matched_grids = matched_grids[,.(index,lon,lat,fg_index,fg_lon,fg_lat,area_contr,foc)]

  # now, matched grids contains the weights for the weighted averages, and we can merge with the original data table:
  setnames(matched_grids,c('cg_index','cg_lon','cg_lat','fg_index','lon','lat','area_contr','foc'))

  if(!is.null(save_weights)) fwrite(matched_grids, file = save_weights)

  ret_dt = merge(dt,matched_grids,c('lon','lat'),allow.cartesian = TRUE)

  # return dt to its original key
  setkeyv(dt,save_key_dt)

  # take the weighted average for upscaling:
  ret_dt= ret_dt[!is.na(get(uscols[1]))] # remove missing values before taking weighted means: The function stats::weighted.means has three if-queries, making it pretty slow.
  # Using sum with na.rm=TRUE is the fast alternative, but if you do that without removing the missing values first,
  # coarse grid cells that do not overlap with a single fine grid cells get the value 0 rather than NA.

  # subset to coarse grid cells with required fraction of coverage
  req_frac_of_coverage = min(0.9999,req_frac_of_coverage) # dealing with precision errors
  ret_dt = ret_dt[foc >= req_frac_of_coverage][,foc:=NULL][]

  ret_dt = ret_dt[,lapply(.SD,FUN = function(x) sum(area_contr*x,na.rm = T)),.SDcols = uscols,by = c(bycols,'cg_lon','cg_lat')]

  setnames(ret_dt,c('cg_lon','cg_lat'),c('lon','lat'))
  return(ret_dt)
}
