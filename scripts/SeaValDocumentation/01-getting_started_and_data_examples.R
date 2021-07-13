### get bookdown ###

install.packages('bookdown')

library

rm(list = ls())

library(PostProcessing)
devtools::load_all()

data("chirps_monthly")
print(chirps_monthly)


?chirps_monthly

# getting the climatology

clim_dt = chirps_monthly[,mean(prec),by = .(lon,lat)]
ggplot_dt(clim_dt)



#subsetting
chirps_monthly[month == 10] # extract the data for October
chirps_monthly[year %between% c(1990,1999)] # extract the data for 1990 - 1999
chirps_monthly[1000:2000] # extract rows 1000 - 2000

# computing
chirps_monthly[,prec]




rmarkdown::render('/nr/user/claudio/pkg/PostProcessing/scripts/CONFER/validation/01-getting_started_and_data_examples.Rmd')
