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
#0.5 vs 0.1
modis.5 <- terra::rast("MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
modis_clamp.5 <- terra::clamp(modis.5, lower=1, upper=254, values=FALSE)
srdb_modis.5 <- terra::extract(modis_clamp.5, srdb[15:14])
srdb$modis.5 <- srdb_modis.5$`MOD17A3H_Y_NPP_2023-01-01_rgb_720x360` * (1950/254) + 50 #scale data
ggplot(srdb, aes(modis, modis.5)) + geom_point() + geom_abline()

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
