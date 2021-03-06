
# trying to make a map


adm1 <- getData('GADM', country='Finland', level=0)
adm2 <- getData('GADM', country='Norway', level=0)
adm3 <- getData('GADM', country='Sweden', level=0)
adm4 <- getData('GADM', country='Pakistan', level=0)

fadm1 = fortify(adm1)
fadm2 = fortify(adm2)
fadm3 = fortify(adm3)
fadm4 = fortify(adm4)

ggplot(fadm1, aes(x = long, y = lat, group = group)) + geom_path() +
  geom_polygon(data = fadm1, aes(x = long, y = lat), fill = "green") +
  geom_polygon(data = fadm2, aes(x = long, y = lat), fill = "green") +
  geom_polygon(data = fadm3, aes(x = long, y = lat), fill = "green") +
  geom_polygon(data = fadm3, aes(x = long, y = lat), fill = "green") +




places <- c('Helsinki, Finland', 'Oslo, Norway', 'Stockholm, Sweden', 'Islamabad, Pakistan')
geoplaces <- geocode(places)

basemap <- qmap("Ukraine", zoom = 4, source='stamen', maptype='toner')

FinalMap <- basemap +
  geom_point(data = geoplaces, aes(x = lon, y = lat), colour="red", fill="red")
print(FinalMap)