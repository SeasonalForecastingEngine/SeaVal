devtools::load_all()
library(ggplot2)


pred = ecmwf_monthly
obs = chirps_monthly

setnames(obs,'prec','obs')

dt = merge(pred,obs,c('lon','lat','month','year'))

setnames(dt,'terc_cat','tercile_cat')

probs = dt[1:10000,above]
obs = dt[1:10000,tercile_cat == 1]
?order



by = 'month'
o = 'obs'



setorderv(dt,c(by,'lon','lat',o))

dt[,sample_quantile := seq(1/.N,1,length.out = .N),by = c(by,'lon','lat')]
dt[,sample_quantile := sample_quantile * 100]

ttest = ggplot_dt(dt[year == 2020 & month == 10],'sample_quantile')


qqs = c(10,20,33,33,67,70,80,90)
library(ggplot2)
my_colors1 <- RColorBrewer::brewer.pal(9, "OrRd")[9:6]
my_colors2 = "#FFFFFF" # yellow checkout gplots::col2hex
my_colors3 <- RColorBrewer::brewer.pal(9, "Blues")[6:9]

my_colors = c(my_colors1,my_colors2,my_colors3)


sc = scale_fill_stepsn(breaks = c(0,qqs,100),colors = my_colors,name = '')
ttest = ttest + sc

wtf = grid::grob(ttest)
ttest = ttest + theme(legend.direction = 'horizontal',legend.position = 'bottom',legend.key.width = unit(0:1,'npc'))

ttest

ttest = ttest + guides(fill = guide_coloursteps(show.limits = TRUE,even.steps = FALSE,barwidth = unit(0:1,'grobwidth',data = wtf)))

ttest

ttest = ttest + ,legend.key.width = unit(ttest$coordinates$limits$x,'npc'))
plot(ttest)

obs = dt[,obs]

by = c('lon','lat','month')
test = dt[,.(qq = quantile(obs,probs = qqs)),by = by]
test[,prob := qqs,by=by]

ttest = dcast(test,lon+lat+month ~prob,fun.aggregate = mean,value.var = 'qq')

dt = merge(dt,test,by = by)


ChickWeight = as.data.table(ChickWeight)
setnames(ChickWeight, tolower(names(ChickWeight)))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table

# dcast is an S3 method in data.table from v1.9.6
dcast(DT, time ~ variable, fun=mean) # using partial matching of argument
dcast(DT, diet ~ variable, fun=mean)
