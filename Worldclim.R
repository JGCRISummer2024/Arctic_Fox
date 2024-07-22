library(geodata)
library(terra)
library(ggplot2)

#Making test data
library(tibble)
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

#testing tavg with a few data points
testTemp <- terra::extract(tavg, test[2:3]) #extracts data from tavg for the coordinate pairs in columns 2 and 3
test$MAT <- rowMeans(testTemp[-1]) #adds a MAT column and puts mean of each row from testTemp in it
testTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = test, mapping = aes(lon, lat, color = MAT)) #plot on a map color according to temperature

#testing tavg with world data set
srdbTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAT_wc)) + 
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))

#testing tavg with high latitude set
srdbhlTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hlf, mapping = aes(Longitude, Latitude, color = MAT_wc)) +
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))

#Graph MAT by MAT_wc
tempCheck <- ggplot(srdb, aes(MAT, MAT_wc)) + geom_point()
tempCheckhl <- ggplot(srdb_hl, aes(MAT, MAT_wc)) + geom_point()

#testing prec with a few data points
testPrec <- terra::extract(prec, test[2:3]) 
test$MAP <- rowSums(testPrec[-1])
testPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = test, mapping = aes(lon, lat, color = MAP)) 

#testing prec with world srdb
srdbPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAP_wc)) + 
  scale_color_gradientn(colors = rainbow(5))

#testing perc with high latitude set
srdbhlPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = MAP_wc)) + 
  scale_color_gradientn(colors = rainbow(5))

#Graph MAP by MAP_wc
percCheck <- ggplot(srdb, aes(MAP, MAP_wc)) + geom_point()
percCheckhl <- ggplot(srdb_hl, aes(MAP, MAP_wc)) + geom_point()

#testing srad with a few data points
testSrad <- terra::extract(srad, test[2:3])
test$srad <- rowMeans(testSrad[-1])