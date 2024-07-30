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

##SoilGrids


##MODIS
modisMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=modis)) +
  scale_color_gradient(low = "white", high = "darkgreen")
#Built in ANPP vs MODIS
modisCheck <- ggplot(srdb, aes(ANPP, modis)) + geom_point() + geom_smooth() + geom_abline()
modisCheckhl <- ggplot(srdb_hl, aes(ANPP, modis)) + geom_point() + geom_smooth() + geom_abline()


##Permafrost
permafrostMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=permafrost)) +
  scale_color_gradient(low = "pink", high = "blue")
