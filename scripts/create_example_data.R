rm(list = ls())

library(data.table)
devtools::load_all()

library(SeaVal)

dt = fread('/nr/project/stat/CONFER/Data/monthly_mean_prec/monthly_prec_fc_init_mon8.csv')

dt = dt[system == 'ecmwf' & member <= 25]

setnames(dt,c('forecast_year','target_month','total_precipitation'),c('year','month','prec'))
dt[,forecast_month := NULL]
dt = dt[month %in% 10:12]
dt[,prec := 1000 * 24 * 3600 * prec]

## tercile forecast ##

dt[,t33:=quantile(prec,0.33),.(lon,lat,month)]
dt[,t67:=quantile(prec,0.67),.(lon,lat,month)]
dt[,terc_cat := -(prec <= t33) + (prec >= t67)]

dt[,below := mean(terc_cat == -1),by = .(lon,lat,month,year)]
dt[,normal := mean(terc_cat == 0),by = .(lon,lat,month,year)]
dt[,above := mean(terc_cat == 1),by = .(lon,lat,month,year)]

dt[,c('t33','t67','terc_cat') := NULL]


ggplot_dt(dt[member == 1],'prec')


dt = dt[member <= 5][,system := NULL]

save_dir = '~/pkg/SeaVal/data/'
dir.create(save_dir)

save(dt,file = paste0(save_dir,'ecmwf_monthly.RData'))


dt_chirps = load_chirps()

dt_chirps = dt_chirps[,index_coarse:= NULL][!is.na(prec)]
dt_chirps = dt_chirps[month %in% 10:12]

dt_chirps[,t33:=quantile(prec,0.33),.(lon,lat,month)]
dt_chirps[,t67:=quantile(prec,0.67),.(lon,lat,month)]
dt_chirps[,terc_cat := -(prec <= t33) + (prec >= t67)]

dt_chirps[,c('t33','t67') := NULL]

save(dt_chirps,file = paste0(save_dir,'chirps_monthly.RData'))
