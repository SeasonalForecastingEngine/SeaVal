
install.packages('devtools')
devtools::install_github('SeasonalForecastingEngine/ForecastTools')
devtools::install_github('SeasonalForecastingEngine/SeaVal')

library(data.table)
library(ncdf4)
library(ForecastTools)
library(SeaVal)


data_dir = '/nr/project/stat/CONFER/Data/validation/example_data/202101/' 

cv_dt = cv_to_dt(data_dir)

print(cv_dt)

```

In this R data.table, the column 'prec' contains the predicted precip and the column 'obs' contains the corresponding observations. The function is based on the data structure and naming system of the files in 

~SharedData/gcm/seasonal/202101/.

In particular it makes the following assumptions:
  
  1. The cross-validation file is named according to the following scheme:
  'CrossVal\*\*\*-MMtarYYtar_\*\*\*'
+ MMtar are the target months (capital letters describing consecutive months, e.g. 'OND' or 'JJAS')
+ YYtar is the target year
+ \*\*\* is a placeholder for any sequence of characters
2. The file with the corresponding observations is located in the same folder and is named as the cross-validation file, just beginning with 'ObservedRainfall' rather than 'CrossVal\*\*\*'.
3. Both of these netcdf files are formatted as in the 202101 example.

Since the predictions are point forecasts (rather than probabilities), we can use the mean square error (MSE) to assess the skill of our forecast. The following function computes the MSE and MSE-skill-score:
  
  ```{r}
mse_dt = MSESS_dt(cv_dt,fc_col = 'prec',obs_col = 'obs')

print(mse_dt)

```

The output contains, for each gridpoint, the MSE of the prediction, the MSE of a (leave-one-year-out) climatological forecast, as well as the skill score.
The function MSESS_dt needs three arguments: The data table containing predictions and observations, and the names of the columns containing forecast and observation.

If we want to plot our results as a map, we can use the following function:
  ```{r}
ggplot_dt(mse_dt,'MSESS',rr = c(-0.5,0.5))

```

For averaging (skill) scores by country we can do the following:
  
  ```{r}
mse_dt = add_countries(mse_dt)

mse_country_averaged = mse_dt[,.(MSE = mean(MSE),
                                 clim_MSE = mean(clim_MSE),
                                 MSESS = mean(MSESS)),
                              by = country]
print(mse_country_averaged)

```

We can use the plotting function above to generate all kinds of spatial plots from the derived data. For example, say, we're interested in the predicted anomaly for 2016. We can do the following:

```{r}
cv_dt[,prec_ano := prec - mean(prec), by = .(lon,lat)] # derive anomaly for each gridpoint
ggplot_dt(cv_dt,'prec_ano',rr = c(-10,10))

```


And we can (visually) compare to observed climatology:

```{r}
cv_dt[,obs_ano := obs - mean(obs), by = .(lon,lat)] # derive anomaly for each gridpoint
ggplot_dt(cv_dt,'obs_ano',rr = c(-10,10))

```


## The way going forward...

We will publish this code on github in form of an R package. It is then easy to install and get access to all the functions shown here. Moreover, we can continue to add functionality. These functions already now come with more flexibility than shown here: for example the MSESS_dt-function can also handle ensemble forecasts, and the plotting function allows for a large variety of adjustments (plot title, colorscale, etc.).


