

library(SeaVal)

setwd('/nr/project/stat/CONFER/Data/Data_OND_2022/')


shapefile = './Shapefile/gha.shp'

test = maptools::readShapeSpatial(shapefile)

nr <- nrow(test@data)
# assign id for each lsoa

test@data$id <- 1:nr


test.fort <- fortify(test, region='id')

library(ggplot2)

ggplot(test.fort) + geom_polygon(aes(x = long,y = lat,group = group),color = 'black',fill = 'white')

fwrite(test.fort,file = paste0(data_dir(),'GHA_map.csv'))


tf2 = as.data.table(test.fort)

tf2[,group := as.integer(group)]
ggplot(tf2) + geom_polygon(aes(x = long,y = lat,group = group),color = 'black',fill = 'white')
fwrite(tf2,file = paste0(data_dir(),'GHA_map.csv'))
