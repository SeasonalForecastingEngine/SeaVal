# SeaVal

This package supports validation of seasonal weather forecasts. 
The focus lies on tercile forecasts, i.e. predictions of three probabilities, 
one for 'below normal' (e.g. temperature or precipitation), 'normal', and 
'above normal', respectively. The package implements a large variety of evaluation metrics,
as recommended by the [World Meteorological Organization](<https://library.wmo.int/idurl/4/56227>).
In particular, it simplifies the task of evaluating gridded forecasts against gridded observations. 
It also provides tools for data import/export and plotting. Finally, it provides functionality for 
downloading and managing monthly-means gridded precipitation observations provided by
Climate Hazards Group InfraRed Precipitation with Station data 
[(CHIRPS)](https://www.chc.ucsb.edu/data/chirps).

The package can be installed by running `install.packages("SeaVal")`. Thereafter it can be loaded by running `library(SeaVal)`
For a comprehensive online tutorial, see http://files.nr.no/samba/CONFER/SeaVal/.
