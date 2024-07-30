library(terra)
library(ggplot2)

#attempting with test data
test_modis <- terra::extract(modis_clamp, test[2:3])
test$modis <- test_modis$`MOD17A3H_Y_NPP_2023-01-01_rgb_3600x1800` * (1950/254) + 50

#srdb data
modisMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=modis)) +
  scale_color_gradient(low = "white", high = "darkgreen")

#Built in NPP vs MODIS
modisCheck <- ggplot(srdb, aes(ANPP, modis)) + geom_point() + geom_smooth() + geom_abline()
modisCheckhl <- ggplot(srdb_hl, aes(ANPP, modis)) + geom_point() + geom_smooth() + geom_abline()
