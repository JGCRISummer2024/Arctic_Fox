library(geodata)
#Read in soil respiration data
srdb <- read.csv("srdb-data.csv")

#Get worldclim data
tavg <- worldclim_global("tavg", 10, "worldclim_data/")
prec <- worldclim_global("prec", 10, "worldclim_data/")
srad <- worldclim_global("srad", 10, "worldclim_data/")

#Combine Data
srdb_temp <- terra::extract(tavg, srdb[15:14])
srdb$MAT_wc <- rowMeans(srdb_temp[-1])
srdb_prec <- terra::extract(prec, srdb[15:14]) 
srdb$MAP_wc <- rowSums(srdb_prec[-1])
srdb_srad <- terra::extract(srad, srdb[15:14])
srdb$srad <- rowMeans(srdb_srad[-1])

#Filters for only high latitude data and chooses columns to focus on
library(dplyr)
srdb_hlf <- srdb %>%
  #ask if this is good
  filter(Latitude >= 45 & MAT_wc <= 5 & Quality_flag != "Q13" & Quality_flag != "Q12" & Manipulation == "None" & !is.na(Rs_annual)) %>%
  select(Country, Latitude, Longitude, Rs_annual, Rs_growingseason, MAT_wc, MAP_wc, srad)
srdb_hl <- srdb %>% 
  filter(Latitude >= 50 & Quality_flag != "Q13" & Quality_flag != "Q12" & Manipulation == "None" & !is.na(Rs_annual)) %>%
  select(Country, Latitude, Longitude, Rs_annual, Rs_growingseason, MAT_wc, MAP_wc, srad, Soil_drainage, Soil_CN, NPP, C_soilmineral, ER)

# Sanity check - quickly plot the data colored by country
library(ggplot2)
p <- ggplot(srdb_hl, aes(Longitude, Latitude, color = Country)) + geom_point()

# Improved plots on map
#1
library(maps)
map(database = "world")
points(x=srdb_hl$Longitude, y=srdb_hl$Latitude, col="Purple")

#2
m2 <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = Country))

#3 circumpolar
library(mapproj)
m3 <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(srdb_hlf, mapping = aes(Longitude, Latitude, color = Country))


#MAP and MAT
matVrsa <- ggplot(srdb_hl, aes(MAT, Rs_annual)) + geom_point() + geom_smooth()
matVrsg <- ggplot(srdb_hl, aes(MAT, Rs_growingseason)) + geom_point() + geom_smooth()
mapVrsa <- ggplot(srdb_hl, aes(MAP, Rs_annual)) + geom_point() + geom_smooth()
mapvrsg <- ggplot(srdb_hl, aes(MAP, Rs_growingseason)) + geom_point() + geom_smooth()
