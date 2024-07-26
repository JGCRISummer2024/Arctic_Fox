library(terra)
library(ggplot2)
library(scales)

#get MODIS data
modis <- terra::rast("MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
modis_clamp <- terra::clamp(modis, lower=1, upper=254, values=FALSE)

#attempting with test data
test_modis <- terra::extract(modis_clamp, test[2:3])
test$modis <- scales::rescale(test_modis$`MOD17A3H_Y_NPP_2023-01-01_rgb_720x360`, to = c(50, 2000))

#srdb data
srdb_modis <- terra::extract(modis_clamp, srdb[15:14])
srdb$modis <- scales::rescale(srdb_modis$`MOD17A3H_Y_NPP_2023-01-01_rgb_720x360`, to = c(50, 2000))
#modisMap <- ggplot() + 
#  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
#  geom_point(srdb, mapping=aes(Longitude, Latitude, color=modis)) +
#  scale_color_gradient(low = "white", high = "darkgreen")

#Built in NPP vs MODIS
#modisCheck <- ggplot(srdb, aes(NPP, modis)) + geom_point()
modisCheckhl <- ggplot(srdb_hl, aes(NPP, modis)) + geom_point()
