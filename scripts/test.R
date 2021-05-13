
# These lines you only need when you don't have the package installed, so you only need to do this once:
install.packages('devtools')

devtools::install_github('SeasonalForecastingEngine/SeaVal')


# setup:
library(SeaVal)
library(ForecastTools)

# set this to the data directory containing the cross-validation data,
# at ICPAC that should be something like '~SharedData/gcm/seasonal/202101/'. If you are using windows, you might need to replace '/' by '\' instead...

data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202101/'

### get cross-validation results ###

cv_dt = cv_to_dt(data_dir)

print(cv_dt)

### derive MSE skill scores ###

mse_dt = MSESS_dt(cv_dt,fc_col = 'prec',obs_col = 'obs')

print(mse_dt)

### plot results ###

ggplot_dt(mse_dt,'MSESS',rr = c(-0.5,0.5)) # rr is the range of the color scale

### get result by country ###

mse_dt = add_countries(mse_dt) # adds a country column to the data.table

mse_country_averaged = mse_dt[,.(MSE = mean(MSE), # averages the specified columns by country
                                 clim_MSE = mean(clim_MSE),
                                 MSESS = mean(MSESS)),
                              by = country]
# show results:
print(mse_country_averaged)

### some more example plots, here of 2016 predicted and observed anomalies, and of model bias: ###

# derive forecast- and observed anomalies:
cv_dt[,prec_ano := prec - mean(prec), by = .(lon,lat)] # forecast anomaly
cv_dt[,obs_ano := obs - mean(obs), by = .(lon,lat)] # observed anomaly

rr = c(-25,25) # specify range of color scale
ggplot_dt(cv_dt[year == 2016],'prec_ano',rr = rr)
ggplot_dt(cv_dt[year == 2016],'obs_ano',rr = rr)

#derive and plot model bias:
cv_dt[,bias := mean(prec) - mean(obs), by = .(lon,lat)]
ggplot_dt(cv_dt,'bias') # note that here we left out the rr argument for the range of the color scale, so it is determined automatically. It is by default centered at 0
