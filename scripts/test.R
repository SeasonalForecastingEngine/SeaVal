
install.packages('devtools')
install.packages('data.table')

devtools::install_github('SeasonalForecastingEngine/ForecastTools')
devtools::install_github('SeasonalForecastingEngine/SeaVal')

library(data.table)

library(ForecastTools)
library(SeaVal)


data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202101/'

cv_dt = cv_to_dt(data_dir)

print(cv_dt)

mse_dt = MSESS_dt(cv_dt,fc_col = 'prec',obs_col = 'obs')

print(mse_dt)


ggplot_dt(mse_dt,'MSESS',rr = c(-0.5,0.5))


mse_dt = add_countries(mse_dt)

mse_country_averaged = mse_dt[,.(MSE = mean(MSE),
                                 clim_MSE = mean(clim_MSE),
                                 MSESS = mean(MSESS)),
                              by = country]
print(mse_country_averaged)

cv_dt[,prec_ano := prec - mean(prec), by = .(lon,lat)] # derive anomaly for each gridpoint
ggplot_dt(cv_dt,'prec_ano',rr = c(-10,10))



And we can (visually) compare to observed climatology:

```{r}
cv_dt[,obs_ano := obs - mean(obs), by = .(lon,lat)] # derive anomaly for each gridpoint
ggplot_dt(cv_dt,'obs_ano',rr = c(-10,10))

```


## The way going forward...

We will publish this code on github in form of an R package. It is then easy to install and get access to all the functions shown here. Moreover, we can continue to add functionality. These functions already now come with more flexibility than shown here: for example the MSESS_dt-function can also handle ensemble forecasts, and the plotting function allows for a large variety of adjustments (plot title, colorscale, etc.).


