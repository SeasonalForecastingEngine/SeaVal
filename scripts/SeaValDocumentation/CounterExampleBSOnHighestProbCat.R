



library(ggplot2)
library(data.table)

dt = data.table(expand.grid(category = c('below','normal','above'), prediction = c('climatology','forecaster')))

dt[prediction == 'climatology',value:= c(0.34,0.33,0.33)]
dt[prediction == 'forecaster',value:= c(0.1,0.4,0.5)]

dt[,count:= round(value * 60)]


theme_set(theme_bw(base_size = 22))

pp = ggplot(dt) + geom_col(aes(x = category,y = value,color = prediction,fill = prediction),position = position_dodge(0.55),width = 0.45) + 
  ylim(c(0,1)) + ylab('assigned prob.') + xlab('') + 
  geom_text(aes(x = category,y = value,label=value),data = dt[prediction == 'climatology'], position=position_dodge(2), hjust = 1,vjust=-0.25) +
  geom_text(aes(x = category,y = value,label=value),data = dt[prediction == 'forecaster'], position=position_dodge(2), hjust = 0,vjust=-0.25)



pp = pp + geom_rect(xmin = 0.5,xmax = 1.5,ymin = -1,ymax = 2,fill = 'red',alpha = 0.05)
pp = pp + geom_rect(xmin = 2.5,xmax = 3.5,ymin = -1,ymax = 2,fill = 'turquoise',alpha = 0.05)

pp

pdf('example_plot.pdf',width = 15,height = 5)

pp

dev.off()

