library(tibble)
library(ggplot2)

#Test data
test <- tribble(  ~place,  ~lon,         ~lat,
                  "JGCRI",    -76.92238, 38.97160,
                  "Thompson", -97.84862, 55.74706,
                  "Fes",      -5.01007,  34.03614,
                  "Lima",     -77.03086, -12.04545,
                  "Estonia", 27.25, 58.250,
                  "Canada", -99.940, 56.630,
                  "Canada2", -99.940, 56.630,
                  "Canada3", -99.940, 56.630)
test$ID <- seq_len(nrow(test)) #adds an ID column

##MODIS
#World map
modisMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=modis)) +
  scale_color_gradient(low = "white", high = "darkgreen")
plot(modis)
#Built in ANPP vs MODIS
modisCheck <- ggplot(srdb, aes(ANPP, modis)) + geom_point() + geom_smooth() + geom_abline()
modisCheckhl <- ggplot(srdb_hl, aes(ANPP, modis)) + geom_point() + geom_smooth() + geom_abline()


##Permafrost
#World map
permafrostMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=permafrost)) +
  scale_color_gradient(low = "pink", high = "blue")
plot(permafrost)

##Peatlands
#maps world and circumpolar
peatMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=peatland)) +
  scale_color_gradient(low = "blue", high = "yellow")
peatMaphl <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = peatland)) +
  scale_color_gradient(low="blue", high="yellow")
plot(peatland)

###WorldClim
##MAT/tavg
#maps world and circumpolar
wTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAT_wc)) + 
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))
hlTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = MAT_wc)) +
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))
#Graph MAT by MAT_wc
tempCheck <- ggplot(srdb, aes(MAT, MAT_wc)) + geom_point() + geom_abline()
tempCheckhl <- ggplot(srdb_hl, aes(MAT, MAT_wc)) + geom_point() + geom_abline()

##MAP/prec
#maps world and circumpolar
wPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAP_wc)) + 
  scale_color_gradientn(colors = rainbow(5))
hlPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = MAP_wc)) + 
  scale_color_gradientn(colors = rainbow(5))
#Graph MAP by MAP_wc
percCheck <- ggplot(srdb, aes(MAP, MAP_wc)) + geom_point() + geom_abline()
percCheckhl <- ggplot(srdb_hl, aes(MAP, MAP_wc)) + geom_point() + geom_abline()

##Solar Radiation (srad)
sradMap <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = srad)) + 
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))

##SoilGrids
hlOSC <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = OCS)) + 
  scale_color_gradientn(colors = rainbow(5))
#plot(soilgrids) #takes a while
#trying to make a check with C_soilmineral but they use different units
oscCheck <- ggplot(srdb_hl, aes(C_soilmineral, OCS*200)) + geom_point() + geom_abline()
