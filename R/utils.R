#' adds a column with the countryname to a data table
#' Only the following 10 countries are added: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tansania, Uganda, Rwanda, Burundi
#' @param dt the data table.
#'
#' @export
#' @importFrom data.table as.data.table

add_country_names = function(dt)
{
  data(countries)
  cs = as.data.table(countries)
  return(merge(dt,cs,by = c('lon','lat')))
}

#' adds a column with the tercile category to a data table
#' @param dt the data table.
#' @param datacol Name of the column where the data is stored
#' @param bycols names of columns to group by
#'
#' @export
#' @importFrom data.table as.data.table

add_tercile_cat = function(dt,datacol = 'prec',by = intersect(c('month','lon','lat'),names(dt)))
{
  dt[!(is.na(get(datacol))),tercile_cat := -1*(get(datacol) <= quantile(get(datacol),0.33)) + 1 *(get(datacol) >= quantile(get(datacol),0.67)),by = by]
  return(dt)
}

#' Returns a leave-one-year-out climatology-based ensemble forecast
#'
#' for a given year, the ensemble forecast simply consists of the observations in all other years.
#' This is essentially an auxiliary function for computing skill scores relative to climatology.
#'
#' @param obs_dt Data table containing observations, must contain a column 'year'.
#' @param by_col character vector containing the column names of the grouping variables, e.g. \code{c('month','lon','lat')}.
#'
#' @return Long data table with the typical ensemble-forecast looks, i.e. containing a column 'member'.
#' @export

climatology_ens_forecast = function(obs_dt,
                                    by)
{
  years = unique(obs_dt[,year])

  ret_dt = data.table()
  for(yy in years)
  {
    dt_temp = obs_dt[year != yy][,member := 1:.N,by = by][,year:=yy]
    ret_dt = rbindlist(list(ret_dt,dt_temp))
  }
  return(ret_dt)
}

#' Get climatological prediction for exceedence probabilities.
#'
#' The climatological prediction for exceedence probabilities is the fraction of observed years where the observation exceeded the threshold.
#' It's calculated from leave-one-year-out climatology.
#'
#' @param obs_dt Data table containing observations.
#' @param obs_col column name of the observation. Mostly observed precipitation in mm.
#' @param by By which columns should be grouped?
#' @param thresholds vector of thresholds for which the exceedence probabilities should be derived.
#'
#' @export

climatology_threshold_exceedence = function(obs_dt,
                                            obs_col = 'prec',
                                            by = intersect(c('lon','lat','month','season'),names(obs_dt)),
                                            thresholds = c(200,300,350,400))
{
  clim_dt = climatology_ens_forecast(obs_dt,by = by)
  ret_dt = data.table()
  for(thr in thresholds)
  {
    thr_dt = clim_dt[,.(pexcd = mean(get(obs_col) > thr)),by = c(by,'year')]
    thr_dt[,threshold := thr]
    ret_dt = rbindlist(list(ret_dt,thr_dt))
  }
  return(ret_dt)
}


#' Converts time given as 'months since date' (MSD) into years and months (YM)
#' @param dt a data table.
#' @param timecol name of the column containing the time.
#' @param origin The time column contains time in the format month since which date?
#'
#' @return data table with two new columns 'month' and 'year', the timecol is deleted.
#'
#' @export
#' @importFrom data.table as.data.table


MSD_to_YM = function(dt,timecol = 'time',origin = '1981-01-01')
{
  or_mon = month(as.Date(origin))
  or_year = year(as.Date(origin))

  mons = (dt[,get(timecol)] + or_mon ) %% 12

  # in case the middle of the month is supplied (i.e. non-integer times):
  mons = floor(mons)

  mons[mons == 0] = 12

  years = (dt[,get(timecol)] + or_mon ) %/% 12 + or_year
  years[mons == 12] = years[mons == 12] - 1

  dt[,c('year','month') := list(years,mons)]
  dt[,(timecol) := NULL]

  return(dt)
}


#' restricts data to a specified country
#' Only the following 10 countries can be used in this function: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tansania, Uganda, Rwanda, Burundi.
#' This function assumes the lon/lat grid to be a regular half degree or full degree grid.
#' @param dt the data table.
#' @param ct name of the country, or vector containing multiple country names
#' @param rectangle logical. If FALSE (default), the data is restricted to the gridcells for which the centerpoint lies within the selected country (e.g. for computing mean scores for a country).
#' If TRUE, the data is kept for a rectangle containing the entire country, therefore also containing gridpoints outside the country. This is the preferred option for plotting data
#' for a specific country.
#' @param tol Only used when \code{rectangle == TRUE}. A tolerance value for widening the plotting window, making things look a bit nicer.
#'
#' @export
#' @importFrom data.table as.data.table


restrict_to_country = function(dt,ct,rectangle = FALSE,tol = 0.5)
{
  data(countries)
  cs = as.data.table(countries)[country %in% ct]
  if(!rectangle)
  {
    return(merge(dt,cs,by = c('lon','lat'))[,country := NULL])
  }
  if(rectangle)
  {
    lon_range = range(cs[,lon]) + c(-tol,tol)
    lat_range = range(cs[,lat]) + c(-tol,tol)

    return(dt[lon %between% lon_range & lat %between% lat_range])
  }
}


#' restricts data to CONFER region
#' Only the following 10 countries can be used in this function: Sudan, South Sudan, Somalia, Eritrea, Ethiopia, Somalia, Kenya, Tanzania, Uganda, Rwanda, Burundi
#' @param dt the data table.
#' @param ct name of the country, or vector containing multiple country names
#' @param rectangle logical. If FALSE (default), the data is restricted to the gridcells for which the centerpoint lies within the selected country (e.g. for computing mean scores for a country).
#' If TRUE, the data is kept for a rectangle containing the entire country, therefore also containing gridpoints outside the country. This is the preferred option for plotting data
#' for a specific country.
#' @param tol Only used when \code{rectangle == TRUE}. A tolerance value for widening the plotting window, making things look a bit nicer.
#'
#' @export
#' @importFrom data.table as.data.table


restrict_to_confer_region = function(dt,rectangle = FALSE,tol = 0.5)
{
  data(countries)
  cs = as.data.table(countries)
  if(!rectangle)
  {
    return(merge(dt,cs,by = c('lon','lat'))[,country := NULL])
  }
  if(rectangle)
  {
    lon_range = range(cs[,lon]) + c(-tol,tol)
    lat_range = range(cs[,lat]) + c(-tol,tol)

    return(dt[lon %between% lon_range & lat %between% lat_range])
  }
}




#'This function is deprecated! Please use ForecastTools::upscale_regular_lon_lat instead.
#'
#'upscaling data from a finer to a coarser grid (by averaging) assumes the finer grid to be nested within the larger (only averages )
#'Does not account for error by projection (lon/lat squares are treated as actual squares), and assumes lon/lat gridcells to all have equal size,
#'so only use this near the equator.
#'
#'@param dt Data table containing data for upscaling
#'@param uscol The column name of the column to be upscaled by averaging
#'@param coarse_grid The coarse grid as data table with colnames lon,lat.
#'@param bycols column names of dt for grouping (usually time- or system-columns)
#'
#'
#'@export

upscale_nested_griddings = function(dt,
                                    uscol,
                                    coarse_grid,
                                    bycols = intersect(c('month','year'),colnames(dt)))
{
  save_key_dt = key(dt)

  tol = 1e-3 # used for all kinds of tests whether stuff is equal...

  fine_grid = unique(dt[!is.na(get(uscol)),.(lon,lat)])
  setkey(fine_grid,lon,lat)


  # get grid size of fine grid
  fine_lons = sort(unique(fine_grid[,lon]))
  d_lon = fine_lons[2] - fine_lons[1]

  fine_lats = sort(unique(fine_grid[,lat]))
  d_lat = fine_lats[2] - fine_lats[1]

  dt[,min_lon := lon-d_lon/2][,max_lon := lon + d_lon/2][,min_lat := lat-d_lat/2][,max_lat := lat + d_lat/2]
  fine_grid[,min_lon := lon-d_lon/2][,max_lon := lon + d_lon/2][,min_lat := lat-d_lat/2][,max_lat := lat + d_lat/2]

  # get grid size of coarse grid
  coarse_lons = sort(unique(coarse_grid[,lon]))
  D_lon = coarse_lons[2] - coarse_lons[1]

  coarse_lats = sort(unique(coarse_grid[,lat]))
  D_lat = coarse_lats[2] - coarse_lats[1]

  coarse_grid[,min_lon := lon-D_lon/2 - tol][,max_lon := lon + D_lon/2 + tol][,min_lat := lat - D_lat/2 - tol][,max_lat := lat + D_lat/2 + tol] # tolerance is added because we do fjoin within:

  # overlapjoin in lon:

  setkey(dt,min_lon,max_lon)
  setkey(fine_grid,min_lon,max_lon)
  setkey(coarse_grid,min_lon,max_lon)

  # using foverlaps on the entire dt does not fit in memory for big data tables:

  dt_sub = foverlaps(fine_grid,coarse_grid,type = 'within')

  # restrict to coarse cells that actually contain at least one fine cell:
  dt_sub = dt_sub[!is.na(lon)&!is.na(lat)]
  # restrict to coordinates within coarse gridcells
  dt_sub = dt_sub[ i.min_lat >=min_lat & i.max_lat <=  max_lat]

  setnames(dt_sub,c('lon','lat','i.lon','i.lat'),c('new_lon','new_lat','lon','lat'))

  dt = merge(dt,dt_sub[,.(lon,lat,new_lon,new_lat)],by = c('lon','lat'))


  dt = dt[,lapply(.SD,mean,na.rm = T),.SDcols = uscol,by = c(bycols,'new_lon','new_lat')]

  setnames(dt,c('new_lon','new_lat'),c('lon','lat'))

  return(dt)
}


#'This function is deprecated! Please use ForecastTools::upscale_regular_lon_lat instead.
#'
#' @importFrom Matrix sparseMatrix rowSums
#' @export

upscale_to_half_degrees = function(dt,
                                   uscol,
                                   bycols = intersect(c('month','year'),colnames(dt)))
{
  fine_grid = unique(dt[!is.na(get(uscol)),.(lon,lat)])
  setkey(fine_grid,lon,lat)

  # get the grid box extend
  min_lon = floor(2*fine_grid[,min(lon)])/2
  min_lat = floor(2*fine_grid[,min(lat)])/2
  max_lon = ceiling(2*fine_grid[,max(lon)])/2
  max_lat = ceiling(2*fine_grid[,max(lat)])/2

  # get the four 'closest' coarse grid box centerpoints:

  fine_grid[,clon1 := floor(2*lon)/2][,clat1 := floor(2*lat)/2]
  fine_grid[,clon2 := floor(2*lon)/2 + 0.5][,clat2 := floor(2*lat)/2]
  fine_grid[,clon3 := floor(2*lon)/2 + 0.5][,clat3 := floor(2*lat)/2 + 0.5]
  fine_grid[,clon4 := floor(2*lon)/2][,clat4 := floor(2*lat)/2 + 0.5]

  # get rectangle overlap:
  rect_intersect = function(center_x1,center_y1,center_x2,center_y2,l1,l2)
  {
    x_overlap = pmin(center_x1 + l1/2, center_x2 + l2/2) - pmax(center_x1 - l1/2,center_x2 - l2/2)
    x_overlap = pmax(x_overlap,0)
    y_overlap = pmin(center_y1 + l1/2, center_y2 + l2/2) - pmax(center_y1 - l1/2,center_y2 - l2/2)
    y_overlap = pmax(y_overlap,0)

    return(x_overlap * y_overlap)
  }

  # get grid size for fine grid (assuming that grid cells are squares.)
  temp = sort(unique(fine_grid[,lon]))
  grid_size_fine_grid = round(temp[2] - temp[1],5) # round, because of instabilities

  for(ii in 1:4)
  {

    fine_grid[,paste0('ol',ii) := rect_intersect(center_x1 = lon,
                                                 center_y1 = lat,
                                                 center_x2 = get(paste0('clon',ii)),
                                                 center_y2 = get(paste0('clat',ii)),
                                                 l1 = grid_size_fine_grid,
                                                 l2 = 0.5)]
  }

  coarse_grid = as.data.table(expand.grid(lon = seq(min_lon,max_lon,0.5), lat = seq(min_lat,max_lat,0.5)))
  setkey(coarse_grid,lon,lat)

  fine_grid[,index_fine:= 1:.N]
  coarse_grid[,index_coarse:= 1:.N]


  ### get weight matrices ###
  for (ind in 1:4) {
    temp = copy(coarse_grid)
    setnames(temp, c(paste0("c", c("lon", "lat"), ind), paste0("ic",
                                                               ind)))
    fine_grid = merge(fine_grid, temp, by = paste0("c", c("lon",
                                                          "lat"), ind))
  }
  setkey(fine_grid, lon, lat)
  for(ind in 1:4)
  {
    assign(paste0("Mat", ind), value = Matrix::sparseMatrix(i = fine_grid[,
                                                                          get(paste0("ic", ind))], j = fine_grid[, index_fine],
                                                            x = fine_grid[, get(paste0("ol", ind))], dims = c(coarse_grid[,
                                                                                                                          .N], fine_grid[, .N])))
  }

  multipl_matrix = Mat1 + Mat2 + Mat3 + Mat4
  rs = Matrix::rowSums(multipl_matrix) # Matrix:: is required because they are sparse matrices
  weight_inverter = rep(0,length(rs))
  weight_inverter[abs(rs) > 1e-5] = 1/rs[abs(rs) > 1e-5]

  weight_mat =   multipl_matrix * weight_inverter

  coarse_grid[,inGHA := abs(rs) > 1e-5]

  dt = dt[!is.na(get(uscol))]

  dt_new = data.table()

  setkeyv(dt,c(bycols,'lon','lat'))
  setkey(fine_grid,lon,lat)

  setkey(coarse_grid,lon,lat)

  levels = dt[lon == lon[1] & lat == lat[1],.SD,.SDcols = bycols]


  for(ll in 1:levels[,.N])
  {
    ll_sub = levels[ll]

    print(paste0(ll,'/',levels[,.N]))

    temp = dt[ll_sub]
    setkey(temp,lon,lat)

    new_vals = weight_mat %*% temp[,get(uscol)]

    temp2 = copy(coarse_grid)
    temp2[,(uscol):= as.vector(new_vals)]
    temp2 = cbind(temp2,ll_sub)


    dt_new = rbindlist(list(dt_new,temp2))
  }

  dt_new[!(inGHA),(uscol) := NA]
  dt_new[,inGHA := NULL][,index_coarse:=NULL]

  return(dt_new)
}

