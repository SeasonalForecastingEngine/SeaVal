

add_countries = function(dt,countryfile = '/nr/project/stat/CONFER/Data/GHAcountries.csv')
{
  cs = fread(countryfile)
  return(merge(dt,cs,by = c('lon','lat')))
}
