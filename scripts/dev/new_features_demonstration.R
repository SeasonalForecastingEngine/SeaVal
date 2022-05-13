rm(list = ls())

library(SeaVal)


#prepare example data:

obs = chirps_monthly
setnames(obs,'prec','obs')


dt = merge(ecmwf_monthly,obs, by = c('lon','lat','year','month'))
setnames(dt,'terc_cat','tercile_cat')

print(dt)

# we don't need multiple members for tercile forecasts:
dt = dt[member == 1]

# focus on November:
dt = dt[month == 11]

dt[,clim := mean(obs),by = .(lon,lat)]
ggplot_dt(dt,'clim',midpoint = 0,high = 'blue')

# eliminate dry gridpoints:
dt = dt[clim > 0.1]
ggplot_dt(dt,'clim',midpoint = 0,high = 'blue')

####### Evaluate ########

# ignorance score/ignorance skill score:

ign = IGSS(dt)

dt[,member := NULL]
ign = IGSS(dt)
ggplot_dt(ign,'IGSS')

ggplot_dt(ign,'IGSS',rr = c(-0.5,0.5))

### remember the multicategory Brier (skill) score: ###
mbs = MBS(dt)
ggplot_dt(mbs,'MBS',rr = c(-0.5,0.5))

hits = HS(dt)
ggplot_dt(hits,'HS_above',midpoint = 0)

#### new: diagnostic diagrams for tercile forecasts ####

# reliability diagrams:
rel_diag(dt)

dt = add_country_names(dt)
dt_ken = dt[country == 'Kenya']
ggplot_dt(dt_ken,'clim')

rel_diag(dt_ken)

# ROC-curves:
ROC_curve(dt)

ROC_curve(dt,interpolate = FALSE)

roc_scores = ROCS(dt)
roc_scores

ggplot_dt(roc_scores,'ROC_above',midpoint = 0.5, low = 'yellow', high = 'forestgreen', mid = 'black')
ggplot_dt(roc_scores,'ROC_normal',midpoint = 0.5)
