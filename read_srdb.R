library(geodata)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(raster)

##Get data
#Soil Respiration Data Base
srdb <- read.csv("srdb-data.csv")
srdb <- srdb %>% filter(!is.na(Longitude) & !is.na(Latitude))
#WorldClim
tavg <- worldclim_global("tavg", 10, "worldclim_data/")
prec <- worldclim_global("prec", 10, "worldclim_data/")
srad <- worldclim_global("srad", 10, "worldclim_data/")
#SoilGrids
soilgrids <- raster("ocs_0-30cm_mean.tif")
#MODIS
modis <- terra::rast("MOD17A3H_Y_NPP_2023-01-01_rgb_3600x1800.TIFF")
modis_clamp <- terra::clamp(modis, lower=1, upper=254, values=FALSE)
#Permafrost
permafrost <- terra::rast("PF_baseline.tif")

##Combine Data
#WorldClim
srdb_temp <- terra::extract(tavg, srdb[15:14])
srdb$MAT_wc <- rowMeans(srdb_temp[-1]) #get year average
srdb_prec <- terra::extract(prec, srdb[15:14]) 
srdb$MAP_wc <- rowSums(srdb_prec[-1]) #get cumulative year average
srdb_srad <- terra::extract(srad, srdb[15:14])
srdb$srad <- rowMeans(srdb_srad[-1]) #get year average
#MODIS
srdb_modis <- terra::extract(modis_clamp, srdb[15:14])
srdb$modis <- srdb_modis$`MOD17A3H_Y_NPP_2023-01-01_rgb_3600x1800` * (1950/254) + 50 #scale data
#Permafrost
srdb_permafrost <- terra::extract(permafrost, srdb[15:14])
srdb$permafrost <- srdb_permafrost$PF_baseline
srdb$permafrost[is.na(srdb$permafrost[])] <- 0 #set NA values (which mean no permafrost) to 0

#Filters for only cold high latitudes and chooses columns to focus on
srdb_hl <- srdb %>% 
  filter(Latitude >= 50 & MAT_wc <= 3 & Quality_flag != "Q13" & Quality_flag != "Q12" & Manipulation == "None" & !is.na(Rs_annual)) %>%
  dplyr::select(Country, Latitude, Longitude, Rs_annual, Rs_growingseason, MAP, MAT, MAT_wc, MAP_wc, srad, Soil_drainage, Soil_CN, NPP, C_soilmineral, ANPP, modis, permafrost)

#Combine srdb_hl with SoilGrids OCS (combining with full srdb takes way too long so I am only connecting to the high latitude set)
x_points <- SpatialPoints(srdb_hl[3:2], proj4string = CRS("+proj=longlat + datum=WGS84"))
x_points <- spTransform(x_points, projection(soilgrids))
srdb_hl$OCS <- raster::extract(soilgrids, x_points, buffer = 1000, fun = mean)

# Sanity check - quickly plot the data colored by country
p <- ggplot(srdb_hl, aes(Longitude, Latitude, color = Country)) + geom_point()

# Improved plots on map
#1
map(database = "world")
points(x=srdb_hl$Longitude, y=srdb_hl$Latitude, col="Purple")

#2
m2 <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = Country))

#3 circumpolar
m3 <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(srdb_hl, mapping = aes(Longitude, Latitude, color = Country))

#MAP and MAT
matVrsa <- ggplot(srdb_hl, aes(MAT, Rs_annual)) + geom_point() + geom_smooth()
matVrsg <- ggplot(srdb_hl, aes(MAT, Rs_growingseason)) + geom_point() + geom_smooth()
mapVrsa <- ggplot(srdb_hl, aes(MAP, Rs_annual)) + geom_point() + geom_smooth()
mapvrsg <- ggplot(srdb_hl, aes(MAP, Rs_growingseason)) + geom_point() + geom_smooth()

#Circumpolar annual respiration map
RsMap <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 5), labels = NULL, lim = c(40, 90)) + #lim = c(40, 90) zooms in but makes edges messy
  geom_point(srdb_hl, mapping = aes(Longitude, Latitude, color = Rs_annual), size = 1) +
  scale_color_gradient(high = c("purple"), low = c("yellow", "salmon")) +
  xlab("") +
  ylab("")+
  theme(panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', color = "darkgrey"), axis.ticks=element_blank()) +
  ggtitle("Annual Carbon Flux from Soil Respiration") +
  labs(color = "Annual Rs (g C m^-2)")

#Normal distribution of srdb_hl
srdbhl_norm <- ggplot(srdb_hl, aes(Rs_annual)) + geom_histogram()
srdbhl_sqrt <- ggplot(srdb_hl, aes(sqrt(Rs_annual))) + geom_histogram()
shapiro.test(srdb_hl$Rs_annual) #p-value = 3.336e-06 
shapiro.test(sqrt(srdb_hl$Rs_annual)) #p-value = 0.03656 still not quite a normal distribution
