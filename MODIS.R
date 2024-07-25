library(raster)
library(terra)
library(ggplot2)
modis <- raster("MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")

#test data
test$modist <- terra::extract(modis, test[2:3])
xpointt <- SpatialPoints(test[2:3], proj4string = CRS("+proj=longlat + datum=WGS84"))
xpointt <- spTransform(xpointt, projection(modis))
test$modisr <- raster::extract(modis, xpointt, buffer = 1000, fun = mean)

#srdb_hl data
srdb_hl$modis <- terra::extract(modis, srdb_hl[3:2])

#srdb data
#I feel like numbers are too low...
srdb$modis <- terra::extract(modis, srdb[15:14])
modisMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=modis)) +
  scale_color_gradient(low = "white", high = "darkgreen")

#Built in NPP vs MODIS
modisCheck <- ggplot(srdb, aes(NPP, modis)) + geom_point()
modisCheckhl <- ggplot(srdb_hl, aes(NPP, modis)) + geom_point()
