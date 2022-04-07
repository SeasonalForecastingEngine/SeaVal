# Data import and processing



The `SeaVal` package provides some tools for data import and export (currently limited to netcdf files). 
Moreover, evaluation always requires comparison to observations, and the package downloads and organizes monthly means CHIRPS data.
Note that, for seasonal forecasts, observations are frequently considered relative to the local climatology: For example, *high rainfall* is frequently defined as more rainfall than in 2/3 of all other years (at the samme location and time of year). This requires the download of more observations than just for the year you want to evaluate (because you need to establish what is normal for the considered region/season).

## The function `netcdf_to_dt` {#netcdf_to_dt}

The central function for importing netcdf-data as data.table is called `netcdf_to_dt`. It takes a filename of a netcdf (including directory path) as argument.
The example files we consider are hosted on ICPACs ftp server at SharedData/gcm/seasonal/202102.


```r
print(data_dir) # the directory the data is stored in, you need to adjust this to your platform.
```

```
## [1] "/nr/project/stat/CONFER/Data/validation/example_data/202102/"
```

```r
fn = "CorrelationSkillRain_Feb-Apr_Feb2021.nc"
dt = netcdf_to_dt(paste0(data_dir,fn))
```

```
## File /nr/project/stat/CONFER/Data/validation/example_data/202102/CorrelationSkillRain_Feb-Apr_Feb2021.nc (NC_FORMAT_CLASSIC):
## 
##      1 variables (excluding dimension variables):
##         float corr[lon,lat]   
##             lead: 0
##             average_op_ncl: dim_avg_n over dimension(s): model
##             type: 0
##             time: 13
##             _FillValue: -9999
## 
##      3 dimensions:
##         time  Size:0   *** is unlimited *** (no dimvar)
##         lat  Size:77 
##             units: degrees_north
##         lon  Size:66 
##             units: degrees_east
## 
##     6 global attributes:
##         units: mm
##         MonInit_month: 2
##         valid_time: Feb-Apr
##         creation_date: Mon Feb 15 06:59:57 EAT 2021
##         Conventions: None
##         title:  Correlation between Cross-Validated and Observed Rainfall
```

```r
print(dt)
```

```
##        lon   lat corr
##    1: 20.5 -13.5   NA
##    2: 21.0 -13.5   NA
##    3: 21.5 -13.5   NA
##    4: 22.0 -13.5   NA
##    5: 22.5 -13.5   NA
##   ---                
## 5078: 51.0  24.5   NA
## 5079: 51.5  24.5   NA
## 5080: 52.0  24.5   NA
## 5081: 52.5  24.5   NA
## 5082: 53.0  24.5   NA
```

By default, the function prints out all the information it gets from the netcdf, including units, array sizes etc. 
This can be turned off by the `verbose` argument of the function: setting it to 0 supresses all messages, setting it to 1 only prints units of the variables. The default value is 2.

A netcdf file always contains *variables* (such as precip or temperature) and *dimension variables* (such as longitude or time). The function `netcdf_to_dt` by default tries to extract all variables into a single data table that also contains all dimension variables that are indexing at least one variable: For example, the netcdf file above has three dimension variables: lon,lat, and time (which is empty). It has one variable ('corr') that is indexed by lon and lat, therefore the resulting data table has three columns: corr, lon and lat.

The default behavior of merging all netcdf data into a single data table may sometimes be inappropriate. Say, for example, we have a netcdf with three dimension variables lon,lat and time, and it has a variable precipitation[lon,lat,time] and a second variable grid_point_index[lon,lat]. The resulting data table would have the columns lon,lat,time,precipitation, and grid_point_index.
This is not very memory efficient because the grid_point_indices are repeated for every instance of time. Moreover, in this case we probably don't need the grid_point_index anyway. We can use the `vars` argument of the `netcdf_to_dt` function to extract only selected variables. So, in this example, `netcdf_to_dt('example_file.nc', vars = 'precipitation')` would have done the trick.

Merging the data tables for all variables is particularly memory efficient when you have multiple variables that have different dimension variables. For large netcdfs with many variables and many dimension variables this can easily get out of hand. In this case you can use `netcdf_to_dt('example_file.nc',trymerge = FALSE)`. This will return a list of data tables, one data table for each variable, containing only the variable values and the dimension variables it is indexed by. If you have two or more variables that do not share a dimension variable, the function requires you to set `trymerge = FALSE`, see the example in Section \@ref(ex-corrupted-netcdf).

For the example above, the resulting data table looks like this:


```r
ggplot_dt(dt,
          mn = 'Corr. skill rain Feb-Apr, Feb initialized', # title
          rr = c(-1,1), # range of the colorbar
          discrete_cs = TRUE,binwidth = 0.4) # discretize colorbar
```

<img src="03-data_import_and_download_files/figure-html/unnamed-chunk-2-1.png" width="480" />

Note that the area shown by `ggplot_dt` is always the full extend of the data contained in the data table. In particular, the correlation plot above extends beyond areas where we have data, because the netcdf-file contained these locations (with missing values in the 'corr'-array). To just plot a window taylored to the data that is not missing, we can simply suppress the missing values by using `dt[!is.na(corr)]`.
We can compare to the February initialized forecast for March to May:


```r
fn = "CorrelationSkillRain_Mar-May_Feb2021.nc"

dt = netcdf_to_dt(paste0(data_dir,fn),verbose = 0) 

ggplot_dt(dt[!is.na(corr)], # here we suppress missing values
          mn = 'Corr. skill rain Mar-May, Mar initialized', # title
          rr = c(-1,1), # range of the colorbar
          discrete_cs = TRUE,binwidth = 0.4) # discretize colorbar
```

<img src="03-data_import_and_download_files/figure-html/unnamed-chunk-3-1.png" width="480" />

Similarly, for writing netcdf files from data tables, the package has a function `dt_to_netcdf`. The function requires a data table as input as well as the names of the columns containing the variables and dimension variables, and a filename to write to. The function will prompt you for units for all variables, but otherwise does not allow to include detailed descriptions in the netcdf. It also currently does not support writing netcdfs with multiple variables that have different dimension variables. You can use the Rpackage `ncdf4` for that.

## Downloading and processing CHIRPS data

Evaluation of forecasts always requires observations to assess the forecast performance. Moreover, usually we are interested whether the prediction was as good or better than a naive climatological forecast. This requires establishing a climatology which requires access to past observations as well. To this end, the `SeaVal` package provides code that simplifies the download and use of the CHIRPS monthly means rainfall data set. The CHIRPS data is created by the [Climate Hazard Group of UC Santa Barbara](https://www.chc.ucsb.edu/data/chirps). The data is mirrored on the [IRI data library](https://iridl.ldeo.columbia.edu/), which allows downloading (area-)subsetted data.

In order to download all available CHIRPS monthly mean data to your local machine, it is sufficient to run the function

```r
download_chirps_monthly()
```

The first time you run this function, it will ask you to specify a data directory on your local machine. This path is saved and from now on generally used by the `SeaVal` package for storing and loading data. You can later look up which data directory you specified by running `data_dir()`. In theory you should not change your data directory. If, for some reason, you have to you can run `data_dir(set_dir = TRUE)`. However, this simply generates a new empty data directory and specifies the new directory as lookup path for the `SeaVal` package. It does not move over or delete data in the old data directory - you have to do that manually.

The `download_chirps_monthly` function comes with several useful options. You can specify `months` and `years` you want to download (the default is to download everything there is). Moreover, the function automatically looks up which months have been downloaded previously and only loads the data for months that you are still missing. If you want to re-download and overwrite existing files, you can set `update = FALSE`. 

The CHIRPS data is on the very high spatial resolution of 0.05 degree lon/lat. While this makes for great-looking plots, it also means that the entire CHIRPS data is roughly 800 MB on disk, even though it is just monthly means. Moreover loading and processing this data can take a long time. To avoid this, the function provides you options to derive an upscaled version with a coarser spatial resolution (default is 0.5 degree lon/lat). The three possible options are 

* `resolution = 'both'`: This downloads the original data and additionally derives an upscaled version that is easier to work with. This is recommended when you have a bit over 800 MB of disk space to spare.
* `resolution = 'low'`: Downloads the file and upscales it before saving. Only the coarse resolution is kept. In this format, the entire data is roughly 8 MB on disk.
* `resolution  = 'high'`: Downloads only the original data, and does not upscale. You need roughly 800 MB.

By default, the function downloads only data for the greater-horn-of-Africa area. You can change this setting to download larger datasets such as Africa or even global, see function documentation, but be wary of long download times and disk storage.

After having downloaded the chirps data, you can load it using the function `load_chirps`:

```r
dt = load_chirps()
print(dt)
```

```
##           lon   lat       prec year month
##       1: 21.5 -12.0 7.33704274 1981     1
##       2: 22.0 -12.0 6.98149261 1981     1
##       3: 22.5 -12.0 7.30148010 1981     1
##       4: 23.0 -12.0 9.03189596 1981     1
##       5: 23.5 -12.0 9.07711182 1981     1
##      ---                                 
## 2105106: 49.5  22.5 0.01895365 2022     1
## 2105107: 50.0  22.5 0.02897875 2022     1
## 2105108: 50.5  22.5 0.03328009 2022     1
## 2105109: 51.0  22.5 0.03312253 2022     1
## 2105110: 51.5  22.5 0.03632925 2022     1
```

```r
# example plot
pp = ggplot_dt(dt[year == 2022 & month == 1],high = 'blue',midpoint = 0)
plot(pp)
```

<img src="03-data_import_and_download_files/figure-html/unnamed-chunk-5-1.png" width="480" />

By default, the upscaled data is loaded (which is smaller in memory and loads faster) if it is available.
Moreover, the function provides options to only load subsets of the data, and to load the data in the original high resolution (if you kept it by setting `resolution = 'both'` in `download_chirps()`):


```r
dt = load_chirps(years = 2022,months = 1,us = FALSE)
print(dt)
```

```
##           lon     lat        prec year month
##      1: 21.50  22.475 0.010631205 2022     1
##      2: 21.55  22.475 0.010624977 2022     1
##      3: 21.60  22.475 0.010612711 2022     1
##      4: 21.65  22.475 0.010597722 2022     1
##      5: 21.70  22.475 0.005510498 2022     1
##     ---                                     
## 414686: 51.30 -11.975          NA 2022     1
## 414687: 51.35 -11.975          NA 2022     1
## 414688: 51.40 -11.975          NA 2022     1
## 414689: 51.45 -11.975          NA 2022     1
## 414690: 51.50 -11.975          NA 2022     1
```

```r
# example plot
pp = ggplot_dt(dt,high = 'blue',midpoint = 0)
plot(pp)
```

```
## Warning: Raster pixels are placed at uneven horizontal intervals and will be
## shifted. Consider using geom_tile() instead.
```

```
## Warning: Raster pixels are placed at uneven vertical intervals and will be
## shifted. Consider using geom_tile() instead.
```

<img src="03-data_import_and_download_files/figure-html/unnamed-chunk-6-1.png" width="480" />


