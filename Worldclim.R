#Download temperature data
library(geodata)
tavg <- worldclim_global("tavg", 10, "worldclim_data/")

#testing with a few data points
library(tibble)
test <- tribble(  ~place,  ~lon,         ~lat,
                  "JGCRI",    -76.92238, 38.97160,
                  "Thompson", -97.84862, 55.74706,
                  "Fes",      -5.01007,  34.03614,
                  "Lima",     -77.03086, -12.04545,
                  "Estonia", 27.25, 58.250,
                  "Canada", -99.940, 56.630,
                  "Canada2", -99.940, 56.630,
                  "Canada3", -99.940, 56.630,
                  "Canada4", -99.940, 56.630,
                  "Canada5", -99.940, 56.630,
                  "Canada6", -99.940, 56.630,
                  "Canada7", -99.940, 56.630)
test$ID <- seq_len(nrow(test)) #adds an ID column
library(terra)
testTemp <- terra::extract(tavg, test[2:3]) #extracts data from tavg for the coordinate pairs in columns 2 and 3
test$MAT <- rowMeans(testTemp[-1]) #adds a MAT column and puts mean of each row from testTemp in it
library(ggplot2)
testMP <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = test, mapping = aes(lon, lat, color = MAT)) #plot on a map color according to temperature

#testing with world data set
srdb_temp <- terra::extract(tavg, srdb[15:14])
srdb$MAT_wc <- rowMeans(srdb_temp[-1])
srdb_wcm <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAT_wc))

#testing with high latitude set
srdbhl_temp <- terra::extract(tavg, srdb_hl[3:2])
srdb_hl$MAT_wc <- rowMeans(srdbhl_temp[-1])
srdbhl_wcm <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = MAT_wc))

#Graph MAT by MAT_wc
ggplot(srdb, aes(MAT, MAT_wc)) + geom_point()

#Download precipitation data
prec <- worldclim_global("prec", 10, "worldclim_data/")
