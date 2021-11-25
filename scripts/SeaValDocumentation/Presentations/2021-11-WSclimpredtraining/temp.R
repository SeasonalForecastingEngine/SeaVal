rm(list = ls())

library(SeaVal)

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202109/'

fns = list.files(data_dir)
#
#
# for(fn in fns)
# {
#   print(fn)
#   dt = netcdf_to_dt(paste0(data_dir,fn),verbose = 0)
#   print(dt)
# }
# dt = netcdf_to_dt(paste0(data_dir,'PredictedProbabilityRain_Oct_Sep2021.nc'),verbose = 0)


setwd('/nr/project/stat/CONFER/Data/validation/example_data/202109/')

prediction = netcdf_to_dt('PredictedProbabilityRain_Oct_Sep2021.nc')
prediction = prediction[!is.na(below)]

print(prediction)



chirps_dir = '/nr/project/stat/CONFER/Data/CHIRPS/'
obs_dt = ncdf_to_dt(paste0(chirps_dir,'CHIRPS_monthly.nc'))

setnames(obs_dt,c('lon','lat','month','prec'))

obs_dt[,year := floor(month/12) + 1960]
obs_dt[,month := floor(month)%%12 + 1]
obs_dt[,prec := prec/30] # calendar is 360 days, original unit is mm/month

obs_dt = obs_dt[month == 10]

obs_dt2021 = netcdf_to_dt(paste0(chirps_dir,'CHIRPS_monthly_2021_10.nc'))

setnames(obs_dt2021,c('lon','lat','time','prec'))
obs_dt2021[,year := 2021][,month := 10]

?upscale_to_half_degrees

obs_dt_new = upscale_to_half_degrees(obs_dt, uscol = 'prec',
                                   bycols = 'year')

obs_dt2021_new = upscale_to_half_degrees(obs_dt2021,
                                         uscol = 'prec',
                                         bycols = 'year')

obs_dt2021_new[,prec := prec/30] # calendar is 360 days, original unit is mm/month


obs_dt_new = rbindlist(list(obs_dt_new,obs_dt2021_new),use.names = TRUE)

pred_below = upscale_to_half_degrees(prediction,
                               uscol = c('below'))


### upscale prediction to half degrees ###

  dt = copy(prediction)

  uscol = 'below'


  fine_grid = unique(dt[, .(lon, lat)])
  setkey(fine_grid, lon, lat)
  min_lon = floor(2 * fine_grid[, min(lon)])/2
  min_lat = floor(2 * fine_grid[, min(lat)])/2
  max_lon = ceiling(2 * fine_grid[, max(lon)])/2
  max_lat = ceiling(2 * fine_grid[, max(lat)])/2
  fine_grid[, `:=`(clon1, floor(2 * lon)/2)][, `:=`(clat1,
                                                    floor(2 * lat)/2)]
  fine_grid[, `:=`(clon2, floor(2 * lon)/2 + 0.5)][, `:=`(clat2,
                                                          floor(2 * lat)/2)]
  fine_grid[, `:=`(clon3, floor(2 * lon)/2 + 0.5)][, `:=`(clat3,
                                                          floor(2 * lat)/2 + 0.5)]
  fine_grid[, `:=`(clon4, floor(2 * lon)/2)][, `:=`(clat4,
                                                    floor(2 * lat)/2 + 0.5)]
  rect_intersect = function(center_x1, center_y1, center_x2,
                            center_y2, l1, l2) {
    x_overlap = pmin(center_x1 + l1/2, center_x2 + l2/2) -
      pmax(center_x1 - l1/2, center_x2 - l2/2)
    x_overlap = pmax(x_overlap, 0)
    y_overlap = pmin(center_y1 + l1/2, center_y2 + l2/2) -
      pmax(center_y1 - l1/2, center_y2 - l2/2)
    y_overlap = pmax(y_overlap, 0)
    return(x_overlap * y_overlap)
  }
  temp = sort(unique(fine_grid[, lon]))
  grid_size_fine_grid = round(temp[2] - temp[1], 5)
  for (ii in 1:4) {
    fine_grid[, `:=`(paste0("ol", ii), rect_intersect(center_x1 = lon,
                                                      center_y1 = lat, center_x2 = get(paste0("clon", ii)),
                                                      center_y2 = get(paste0("clat", ii)), l1 = grid_size_fine_grid,
                                                      l2 = 0.5))]
  }
  coarse_grid = as.data.table(expand.grid(lon = seq(min_lon,
                                                    max_lon, 0.5), lat = seq(min_lat, max_lat, 0.5)))
  setkey(coarse_grid, lon, lat)
  fine_grid[, `:=`(index_fine, 1:.N)]
  coarse_grid[, `:=`(index_coarse, 1:.N)]
  fine_grid = fine_grid[1:(.N-1)]
  for (ind in 1:4) {
    temp = copy(coarse_grid)
    setnames(temp, c(paste0("c", c("lon", "lat"), ind), paste0("ic",
                                                               ind)))
    fine_grid = merge(fine_grid, temp, by = paste0("c", c("lon",
                                                          "lat"), ind))
    assign(paste0("Mat", ind), value = Matrix::sparseMatrix(i = fine_grid[,
                                                                          get(paste0("ic", ind))], j = fine_grid[, index_fine],
                                                            x = fine_grid[, get(paste0("ol", ind))], dims = c(coarse_grid[,
                                                                                                                          .N], fine_grid[, .N])))
  }
  multipl_matrix = Mat1 + Mat2 + Mat3 + Mat4
  rs = Matrix::rowSums(multipl_matrix)
  weight_inverter = rep(0, length(rs))
  weight_inverter[abs(rs) > 1e-05] = 1/rs[abs(rs) > 1e-05]
  weight_mat = multipl_matrix * weight_inverter
  coarse_grid[, `:=`(inGHA, abs(rs) > 1e-05)]
  dt = dt[!is.na(get(uscol))]
  dt_new = data.table()
  setkeyv(dt, c(bycols, "lon", "lat"))
  setkey(fine_grid, lon, lat)
  setkey(coarse_grid, lon, lat)

  dt = dt[1:(.N-1)]

  below_vals = weight_mat %*% dt[, below]
  normal_vals = weight_mat %*% dt[, normal]
  above_vals = weight_mat %*% dt[, above]

  dt_new = copy(coarse_grid)
  dt_new[, `:=`(c('below','normal','above'), list(as.vector(below_vals),as.vector(normal_vals),as.vector(above_vals)))]

  dt_new[,inGHA:=NULL]

  dt_new[abs(below + normal +above) <1 ,c('below','normal','above'):=NA_real_]

  ### save!!! ###

  fwrite(dt_new,file = '/nr/project/stat/CONFER/Data/pres2021_11_15/prediction.csv')

fwrite(obs_dt_new,file = '/nr/project/stat/CONFER/Data/pres2021_11_15/obs.csv')
fwrite(obs_dt2021_new,file = '/nr/project/stat/CONFER/Data/pres2021_11_15/obs2021.csv')


obs_dt = fread('/nr/project/stat/CONFER/Data/pres2021_11_15/obs.csv')

small_grid = unique(obs_dt[year == 1981 & !is.na(prec) ,.(lon,lat)])
setkey(small_grid,lon,lat)
setkey(obs_dt,lon,lat)
obs_dt = obs_dt[small_grid,]

setkey(obs_dt,year,lon,lat)


fwrite(obs_dt,file = '/nr/project/stat/CONFER/Data/pres2021_11_15/obs.csv')
