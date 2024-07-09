#Read in soil respiration data
srdb <- read.csv("srdb-data.csv")

#Filters for only high latitude data and chooses columns to focus on
library(dplyr)
srdb_hl <- srdb %>%
  filter(Latitude > 50) %>%
  select(Country, Latitude, Longitude, Rs_annual)

# Sanity check - quickly plot the data colored by country
library(ggplot2)
p <- ggplot(srdb_hl, aes(Longitude, Latitude, color = Country)) + geom_point()

# Improved plot on map (not working)
library(maps)
m <- ggplot(map_data("world"), aes(x=long, y=lat, group=group)) + geom_polygon()
mapData <- map_data("world") %>% rename("Latitude" = "lat")
mapData <- srdb %>% left_join(mapData, by = "Latitude")
a <- ggplot(mapData, aes(x=long, y=Latitude, group=group)) + geom_polygon()
print(a)