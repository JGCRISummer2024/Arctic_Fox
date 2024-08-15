#Analysis of High Latitude of Soil Respiration
#Erin Morrison
#Summer 2024

library(geodata)
library(dplyr)
library(ggplot2)
library(maps)
library(raster)
library(car)
library(caret)

#### Get data ####
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
modis_clamp <- terra::clamp(modis, lower=1, upper=254, values=FALSE) #remove NA data (recorded as 255)

#Permafrost
permafrost <- terra::rast("PF_baseline.tif")

#Peatlands
peatland <- terra::rast("Peat-ML_global_peatland_extent.nc")

## Combine Data ##
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

#Peatlands
srdbPeat <- terra::extract(peatland, srdb[15:14])
srdb$peatland <- srdbPeat$PEATLAND_P

## Filters for only cold high latitudes and chooses columns to focus on
srdb_hl <- srdb %>% 
  filter(Latitude >= 50 & MAT_wc <= 3 & Quality_flag != "Q13" & Quality_flag != "Q12" & Manipulation == "None" & !is.na(Rs_annual)) %>%
  dplyr::select(Country, Latitude, Longitude, Rs_annual, Rs_growingseason, MAP, MAT, MAT_wc, MAP_wc, srad, Soil_drainage, Soil_CN, NPP, C_soilmineral, ANPP, modis, permafrost, peatland)

#Combine srdb_hl with SoilGrids OCS (combining with full srdb takes way too long so I am only connecting to the high latitude set)
x_points <- SpatialPoints(srdb_hl[3:2], proj4string = CRS("+proj=longlat + datum=WGS84"))
x_points <- spTransform(x_points, projection(soilgrids))
srdb_hl$OCS <- raster::extract(soilgrids, x_points, buffer = 1000, fun = mean)

#Improved plots on map
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

#### Ancillary Data Maps & Graphs ####
##MODIS
#World map
modisMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=modis)) +
  scale_color_gradient(low = "white", high = "darkgreen")
#Built in ANPP vs MODIS
modisCheck <- ggplot(srdb, aes(ANPP, modis)) + 
  geom_point() + 
  geom_point(srdb_hl, mapping = aes(ANPP, modis), color = "cyan") +
  geom_abline() +
  xlab("SRDB ANPP") +
  ylab("MODIS NPP") +
  ggtitle("On Site ANPP vs MODIS")
modisCheckhl <- ggplot(srdb_hl, aes(ANPP, modis)) + geom_point() + geom_abline()
#Resolution 0.5 vs 0.1
modis.5 <- terra::rast("MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
modis_clamp.5 <- terra::clamp(modis.5, lower=1, upper=254, values=FALSE)
srdb_modis.5 <- terra::extract(modis_clamp.5, srdb[15:14])
srdb$modis.5 <- srdb_modis.5$`MOD17A3H_Y_NPP_2023-01-01_rgb_720x360` * (1950/254) + 50 #scale data
resolution <- ggplot(srdb, aes(modis, modis.5)) + geom_point() + geom_abline()

##Permafrost
#World map
permafrostMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=permafrost)) +
  scale_color_gradient(low = "pink", high = "blue")

##Peatlands
#maps world and circumpolar
peatMap <- ggplot() + 
  geom_polygon(map_data("world"), mapping = aes(long, lat, group=group)) + 
  geom_point(srdb, mapping=aes(Longitude, Latitude, color=peatland)) +
  scale_color_gradient(low = "blue", high = "yellow")
peatMaphl <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = peatland)) +
  scale_color_gradient(low="blue", high="yellow")

#WorldClim Temperature
#maps world and circumpolar
wTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAT_wc)) + 
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))
hlTM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  xlab("") +
  ylab("")+
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = MAT_wc), size = 1) +
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red")) +
  ggtitle("Mean Annual Temperature") +
  theme(panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', color = "darkgrey"), axis.ticks=element_blank()) +
  labs(color = "Degrees Celsius")
#Graph MAT by MAT_wc
tempCheck <- ggplot(srdb, aes(MAT, MAT_wc)) + 
  geom_point() + 
  geom_point(data = srdb_hl, mapping = aes(MAT, MAT_wc), color="cyan") + 
  geom_abline() +
  xlab("Mean Annual Temperature SRDB") +
  ylab("Mean Annual Temperature WorldClim") +
  ggtitle("SRDB Temperature vs WorldClim Temperature")
tempCheckhl <- ggplot(srdb_hl, aes(MAT, MAT_wc)) + geom_point() + geom_abline()

##WorldClim Precipitation
#maps world and circumpolar
wPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = MAP_wc)) + 
  scale_color_gradientn(colors = rainbow(5))
hlPM <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = MAP_wc)) + 
  scale_color_gradientn(colors = rainbow(5))
#Graph MAP by MAP_wc
percCheck <- ggplot(srdb, aes(MAP, MAP_wc)) + geom_point() + geom_abline()
percCheckhl <- ggplot(srdb_hl, aes(MAP, MAP_wc)) + geom_point() + geom_abline()

##WorldClim Solar Radiation
sradMap <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  geom_point(data = srdb, mapping = aes(Longitude, Latitude, color = srad)) + 
  scale_color_gradient(low=c("purple", "blue", "cyan", "green"), high=c("yellow", "orange", "red"))

##SoilGrids
OCSmap <- ggplot() + geom_polygon(map_data("world"), mapping = aes(x=long, y=lat, group=group)) + 
  coord_map("ortho") + 
  scale_y_continuous(breaks = seq(30, 90, by = 10), labels = NULL) +
  geom_point(data = srdb_hl, mapping = aes(Longitude, Latitude, color = OCS)) + 
  scale_color_gradientn(colors = rainbow(5))

#### Linear Regression Models ####
##Using sqrt(Rs_annual) for normal distribution
##Single Variable Models

#MAT_wc: 13.06%
rm_MAT <- lm(sqrt(Rs_annual) ~ MAT_wc, data = srdb_hl)
summary(rm_MAT)

#MAP_wc: 0.1734%
rm_MAP <- lm(sqrt(Rs_annual) ~ MAP_wc, data = srdb_hl)
summary(rm_MAP)

#srad (solar radiation): 5.747%
rm_srad <- lm(sqrt(Rs_annual) ~ srad, data = srdb_hl)
summary(rm_srad)

#OCS (organic carbon stock): 1.481%
rm_OCS <- lm(sqrt(Rs_annual) ~ OCS, data = srdb_hl)
summary(rm_OCS)

#Soil_drainage: 14.02%
rm_SD <- lm(sqrt(Rs_annual) ~ Soil_drainage, data = srdb_hl)
summary(rm_SD)

#Permafrost: 7.494%
rm_perma <- lm(sqrt(Rs_annual) ~ permafrost, data = srdb_hl)
summary(rm_perma)

#Peatlands: -0.4264%
rm_peat <- lm(sqrt(Rs_annual) ~ peatland, data = srdb_hl)
summary(rm_peat)

#NPP: 31.61%
rm_NPP <- lm(sqrt(Rs_annual) ~ NPP, data = srdb_hl)
summary(rm_NPP)

#ANPP: 12.9%
rm_ANPP <- lm(sqrt(Rs_annual) ~ ANPP, data = srdb_hl)
summary(rm_ANPP)

#MODIS ANPP: 7.788%
rm_modis <- lm(sqrt(Rs_annual) ~ modis, data = srdb_hl)
summary(rm_modis)

##Multivariable Modesl

#Local NPP: 69.73%
rm_locN <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_locN)
srdbhl_locN <- srdb_hl %>% filter(!is.na(NPP) & !is.na(OCS) & !is.na(Soil_drainage))
srdbhl_locN$locNModel <- predict(rm_locN) ^ 2
locN_rp <- ggplot(srdbhl_locN, aes(Rs_annual, locNModel)) + geom_point() + geom_abline()
res_locN <- ggplot(srdbhl_locN, aes(locNModel, Rs_annual-locNModel)) +
  geom_point()

#Local ANPP: 66.87%
rm_locA <- lm(sqrt(Rs_annual) ~ ANPP + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_locA)
srdbhl_locA <- srdb_hl %>% filter(!is.na(ANPP) & !is.na(OCS) & !is.na(Soil_drainage))
srdbhl_locA$locAModel <- predict(rm_locA) ^ 2
locA_rp <- ggplot(srdbhl_locA, aes(Rs_annual, locAModel)) + 
  geom_point() + 
  geom_abline() +
  xlab("Annual Rs (g C m^-2)") +
  ylab("Model Prediction") +
  ggtitle("On Site ANPP Model")
res_locA <- ggplot(srdbhl_locA, aes(locAModel, Rs_annual-locAModel)) +
  geom_point()

#MODIS Satellite ANPP: 26.05%
rm_sat <- lm(sqrt(Rs_annual) ~ modis + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_sat)
srdbhl_sat <- srdb_hl %>% filter(!is.na(modis) & !is.na(OCS) & !is.na(Soil_drainage))
srdbhl_sat$satModel <- predict(rm_sat) ^ 2
sat_rp <- ggplot(srdbhl_sat, aes(Rs_annual, satModel)) + 
  geom_point() + 
  geom_abline() +
  xlab("Annual Rs (g C m^-2)") +
  ylab("Model Prediction") +
  ggtitle("MODIS Satellite Model")
res_sat <- ggplot(srdbhl_sat, aes(satModel, Rs_annual-satModel)) +
  geom_point()

#Info on NPP/ANPP models
Anova(rm_sat)
Anova(rm_locA)
Anova(rm_locN)

#Best Model: 71.84%
srdb_hlf <- srdb_hl %>% filter(!is.na(NPP) & !is.na(OCS) & !is.na(ANPP) & !is.na(Soil_drainage))
rm_best <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + ANPP + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_best)
srdb_hlf$bestModel <- predict(rm_best) ^2
best_rp <- ggplot(srdb_hlf, aes(Rs_annual, bestModel)) + 
  geom_point() + 
  xlab("Actual Annual Rs") +
  ylab("Model Estimate") +
  geom_abline()
res_best <- ggplot(srdb_hlf, aes(bestModel, Rs_annual-bestModel)) +
  geom_point()

#### Model Validation ####
##k-fold validation

##Local NPP
result_locN <- list()
for(i in 1:30){
  set.seed(i)
  rand_locN <- createDataPartition(srdbhl_locN$Rs_annual, p = 0.75, list = FALSE)
  train_locN <- srdbhl_locN[rand_locN,]
  check_locN <- srdbhl_locN[-rand_locN,]
  valMod_locN <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = train_locN)
  guess_locN <- predict(valMod_locN, check_locN)
  go_locN <- data.frame(R2 = R2(guess_locN, check_locN$Rs_annual),
                        RMSE = RMSE(guess_locN, check_locN$Rs_annual),
                        MAE = MAE(guess_locN, check_locN$Rs_annual))
  result_locN[[i]] <- data.frame(go_locN)
  #Plot use seed 13 because its closest to average
  if(i == 13){
    train_locN$valMod_locN <- predict(valMod_locN) ^ 2
    check_locN$guess_locN <- guess_locN ^ 2
    valPlot_locN <- ggplot(train_locN, aes(Rs_annual, valMod_locN)) +
      geom_point() +
      geom_point(check_locN, mapping = aes(Rs_annual, guess_locN), color="red") +
      geom_abline()
  }
}
result_locN <- bind_rows(result_locN)
val_locN <- colMeans(result_locN)

##Local ANPP
result_locA <- list()
for(i in 1:30){
  set.seed(i)
  rand_locA <- createDataPartition(srdbhl_locA$Rs_annual, p = 0.75, list = FALSE)
  train_locA <- srdbhl_locA[rand_locA,]
  check_locA <- srdbhl_locA[-rand_locA,]
  valMod_locA <- lm(sqrt(Rs_annual) ~ ANPP + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = train_locA)
  guess_locA <- predict(valMod_locA, check_locA)
  go_locA <- data.frame(R2 = R2(guess_locA, check_locA$Rs_annual),
                        RMSE = RMSE(guess_locA, check_locA$Rs_annual),
                        MAE = MAE(guess_locA, check_locA$Rs_annual))
  result_locA[[i]] <- data.frame(go_locA)
}
result_locA <- bind_rows(result_locA)
val_locA <- colMeans(result_locA)

##MODIS Satellite ANPP
result_sat <- list()
#There is only one "Mixed" value in the whole data set for soil drainage so there is an error if it is in the check data
srdbhl_sat <- srdbhl_sat %>% filter(Soil_drainage != "Mixed")
for(i in 1:30){
  set.seed(i)
  rand_sat <- createDataPartition(srdbhl_sat$Rs_annual, p = 0.75, list = FALSE)
  train_sat <- srdbhl_sat[rand_sat,]
  check_sat <- srdbhl_sat[-rand_sat,]
  valMod_sat <- lm(sqrt(Rs_annual) ~ MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + modis + srad, data = train_sat)
  guess_sat <- predict(valMod_sat, check_sat)
  go_sat <- data.frame(R2 = R2(guess_sat, check_sat$Rs_annual),
                       RMSE = RMSE(guess_sat, check_sat$Rs_annual),
                       MAE = MAE(guess_sat, check_sat$Rs_annual))
  result_sat[[i]] <- data.frame(go_sat)
  #Plot use seed 14 because its closest to average
  if(i == 14){
    train_sat$valMod_sat <- predict(valMod_sat) ^ 2
    check_sat$guess_sat <- guess_sat ^ 2
    valPlot_sat <- ggplot(train_sat, aes(Rs_annual, valMod_sat)) +
      geom_point() +
      geom_point(check_sat, mapping = aes(Rs_annual, guess_sat), color="red") +
      geom_abline()
  }
}
result_sat <- bind_rows(result_sat)
val_sat <- colMeans(result_sat)

##Best Model
#Works but gives warning
result_best <- list()
for(i in 1:30){
  set.seed(i)
  rand_best <- createDataPartition(srdb_hlf$Rs_annual, p = 0.75, list = FALSE)
  train_best <- srdb_hlf[rand_best,]
  check_best <- srdb_hlf[-rand_best,]
  valMod_best <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + ANPP + Soil_drainage + permafrost + srad, data = train_best)
  guess_best <- predict(valMod_best, check_best)
  go_best <- data.frame(R2 = R2(guess_best, check_best$Rs_annual),
                        RMSE = RMSE(guess_best, check_best$Rs_annual),
                        MAE = MAE(guess_best, check_best$Rs_annual))
  result_best[[i]] <- data.frame(go_best)
}
result_best <- bind_rows(result_best)
val_best <- colMeans(result_best)
