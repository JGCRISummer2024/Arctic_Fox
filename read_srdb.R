#Read in soil respiration data
srdb <- read.csv("srdb-data.csv")

#Filters for only high latitude data and chooses columns to focus on
library(dplyr)
srdb_hlf <- srdb %>%
  #ask about filter this gives pretty much only European data
  filter(Latitude >= 50 & Biome == c("Boreal", "Arctic", "Subarctic", "Tundra") & Quality_flag != "Q13" & Quality_flag != "Q12" & Manipulation == "None" & !is.na(Rs_annual)) %>%
  select(Country, Latitude, Longitude, Rs_annual)
srdb_hl <- srdb %>% 
  filter(Latitude >= 50 & Quality_flag != "Q13" & Quality_flag != "Q12" & Manipulation == "None" & !is.na(Rs_annual)) %>%
  select(Country, Latitude, Longitude, Rs_annual, MAT, Rs_growingseason, MAP)

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
