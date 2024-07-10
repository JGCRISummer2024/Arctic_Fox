#Read in soil respiration data
srdb <- read.csv("srdb-data.csv")

#Filters for only high latitude data and chooses columns to focus on
library(dplyr)
srdb_hl <- srdb %>%
  #ask about filter this gives pretty much only European data
  filter(Latitude > 50 & Quality_flag != "Q13" & Quality_flag != "Q12" & Quality_flag != "Q11" & Manipulation != "None" & !is.na(Rs_annual)) %>%
  select(Country, Latitude, Longitude, Rs_annual)

# Sanity check - quickly plot the data colored by country
library(ggplot2)
p <- ggplot(srdb_hl, aes(Longitude, Latitude, color = Country)) + geom_point()

# Improved plots on map
#1
library(maps)
map(database = "world")
points(x=srdb_hl$Longitude, y=srdb_hl$Latitude, col="Purple")

#2
m2 <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = Country))

#3 (doesn't work at all)
#m <- ggplot(map_data("world"), aes(x=long, y=lat, group=group)) + geom_polygon()
#print(m)
#mapData <- map_data("world") %>% rename(c("Latitude" = "lat", "Longitude" = "long"))
#mapData <- left_join(mapData, srdb_hl, by = c("Latitude", "Longitude")) %>% filter(!is.na(mapData$Rs_annual))
#View(mapData)
#a <- ggplot(mapData, aes(x=long, y=Latitude, group=group)) + geom_polygon()
#print(a)
