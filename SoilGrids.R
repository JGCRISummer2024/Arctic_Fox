library(raster)
library(geodata)
soilgrids <- raster("ocs_0-30cm_mean.tif")
x_tpoints <- SpatialPoints(test[2:3], proj4string = CRS("+proj=longlat + datum=WGS84"))
x_tpoints <- spTransform(x_tpoints, projection(soilgrids))
test$soilgrids <- raster::extract(soilgrids, x_tpoints, buffer = 1000, fun = mean)

#now srdb
x_points <- SpatialPoints(srdb_hl[3:2], proj4string = CRS("+proj=longlat + datum=WGS84"))
x_points <- spTransform(x_points, projection(soilgrids))
srdb_hl$OCS <- raster::extract(soilgrids, x_points, buffer = 1000, fun = mean)
