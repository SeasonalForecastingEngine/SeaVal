
rm(list = ls())

devtools::load_all()
library(ncdf4)

source('~/pkg/SeaVal/scripts/dev/chirps_preliminary_data.R')

download_chirps_monthly(resolution = 'high')
