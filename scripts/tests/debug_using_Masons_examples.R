# In the recommendations for evaluating Seasonal Forecasts Mason gives some examples that should be reproduced:

devtools::load_all()

#example data, Table B.2.

ex_dt = data.table(lon = 1:8,
                   tercile_cat = c(-1,-1,-1,-1,0,0,1,1),
                   below = c(0.45,0.5,0.35,0.33,0.25,0.2,0.2,0.25),
                   normal = c(0.35,0.3,0.4,0.33,0.35,0.35,0.35,0.4),
                   above = c(0.2,0.2,0.25,0.33,0.4,0.45,0.45,0.35))
test = ROCS(ex_dt)
