srdb <- read.csv("srdb-data.csv")

library(dplyr)
srdb_hl <- srdb %>%
  filter(Latitude > 50) %>%
  select(Country, Latitude, Longitude, Rs_annual)

# Sanity check - quickly plot the data colored by country
library(ggplot2)
p <- ggplot(srdb_hl, aes(Longitude, Latitude, color = Country)) + geom_point()
print(p)
