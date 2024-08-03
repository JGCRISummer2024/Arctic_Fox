#Linear regression models

#MAT_wc
#13.06%
rm_MAT <- lm(sqrt(Rs_annual) ~ MAT_wc, data = srdb_hl)
summary(rm_MAT)
res_MAT <- ggplot(srdb_hl, aes(MATmodel, Rs_annual-MATmodel)) +
  geom_point()

#MAT SRDB
#4.241%
rm_MAT2 <- lm(sqrt(Rs_annual) ~ MAT, data = srdb_hl)
summary(rm_MAT2)

#MAP_wc
#0.1734%
rm_MAP <- lm(sqrt(Rs_annual) ~ MAP_wc, data = srdb_hl)
summary(rm_MAP)
res_MAP <- ggplot(srdb_hl, aes(MAPmodel, Rs_annual-MAPmodel)) +
  geom_point()

#MAT_wc
#-0.5518
rm_MAP2 <- lm(sqrt(Rs_annual) ~ MAP, data = srdb_hl)
summary(rm_MAP2)

#srad (solar radiation)
#5.747%
rm_srad <- lm(sqrt(Rs_annual) ~ srad, data = srdb_hl)
summary(rm_srad)

#Soil_CN (C:N)
#-1.894%
rm_CN <- lm(sqrt(Rs_annual) ~ Soil_CN, data = srdb_hl)
summary(rm_CN)

#C_soilmineral (carbon in soil organic matter)
#-1.283%
rm_C <- lm(sqrt(Rs_annual) ~ C_soilmineral, data = srdb_hl)
summary(rm_C)

#OCS (organic carbon storage)
#1.481%
rm_OCS <- lm(sqrt(Rs_annual) ~ OCS, data = srdb_hl)
summary(rm_OCS)
ggplot(srdb_hl, aes(OCS, Rs_annual)) + geom_point() + geom_smooth(method = lm)

#NPP (net primary production)
#31.61%
rm_NPP <- lm(sqrt(Rs_annual) ~ NPP, data = srdb_hl)
summary(rm_NPP)

#MODIS NPP
#7.788%
rm_modis <- lm(sqrt(Rs_annual) ~ modis, data = srdb_hl)
summary(rm_modis)
ggplot(srdb_hl, aes(modis, Rs_annual)) + geom_point() + geom_smooth(method = lm)

#ANPP (above ground net primary production)
#12.9%
#This is more effective than MODIS but a higher p-value likely do to MODIS having more data
rm_ANPP <- lm(sqrt(Rs_annual) ~ ANPP, data = srdb_hl)
summary(rm_ANPP)

#Permafrost
#7.494%
rm_perma <- lm(sqrt(Rs_annual) ~ permafrost, data = srdb_hl)
summary(rm_perma)

#Soil_drainage
#14.02%
rm_SD <- lm(sqrt(Rs_annual) ~ Soil_drainage, data = srdb_hl)
summary(rm_SD)

#Peatlands
#-0.4264%
rm_peat <- lm(sqrt(Rs_annual) ~ peatland, data = srdb_hl)
summary(rm_peat)
ggplot(srdb_hl, aes(peatland, Rs_annual)) + geom_point() + geom_smooth(method = lm)

#MAT_wc and MAP_wc
#15.76%
rm_TP <- lm(sqrt(Rs_annual) ~ MAT_wc + MAP_wc, data = srdb_hl)
summary(rm_TP)


#Satellite ANPP
#26.05%
rm_sat <- lm(sqrt(Rs_annual) ~ modis + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_sat)
srdbhl_sat <- srdb_hl %>% filter(!is.na(modis) & !is.na(OCS) & !is.na(Soil_drainage))
srdbhl_sat$satModel <- predict(rm_sat) ^ 2
sat_rp <- ggplot(srdbhl_sat, aes(Rs_annual, satModel)) + geom_point() + geom_abline()
res_sat <- ggplot(srdbhl_sat, aes(satModel, Rs_annual-satModel)) +
  geom_point()

#Local ANPP
#66.87%
rm_locA <- lm(sqrt(Rs_annual) ~ ANPP + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_locA)
srdbhl_locA <- srdb_hl %>% filter(!is.na(ANPP) & !is.na(OCS) & !is.na(Soil_drainage))
srdbhl_locA$locAModel <- predict(rm_locA) ^ 2
locA_rp <- ggplot(srdbhl_locA, aes(Rs_annual, locAModel)) + geom_point() + geom_abline()
res_locA <- ggplot(srdbhl_locA, aes(locAModel, Rs_annual-locAModel)) +
  geom_point()

#Local NPP
#69.73%
rm_locN <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_locN)
srdbhl_locN <- srdb_hl %>% filter(!is.na(NPP) & !is.na(OCS) & !is.na(Soil_drainage))
srdbhl_locN$locNModel <- predict(rm_locN) ^ 2
locN_rp <- ggplot(srdbhl_locN, aes(Rs_annual, locNModel)) + geom_point() + geom_abline()
res_locN <- ggplot(srdbhl_locN, aes(locNModel, Rs_annual-locNModel)) +
  geom_point()

#Multiple variables best model (assume no interaction)
#71.84%
srdb_hlf <- srdb_hl %>% filter(!is.na(NPP) & !is.na(OCS) & !is.na(ANPP) & !is.na(Soil_drainage))
rm_best <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + ANPP + Soil_drainage + permafrost + srad, data = srdb_hl)
summary(rm_best)
srdb_hlf$bestModel <- predict(rm_best) ^2
best_rp <- ggplot(srdb_hlf, aes(Rs_annual, bestModel)) + geom_point() + geom_abline()
res_best <- ggplot(srdb_hlf, aes(bestModel, Rs_annual-bestModel)) +
  geom_point()

#Multiple variables not including built in srdb NPP (assume no interaction)
#26.05
rm_rem <- lm(sqrt(Rs_annual) ~ MAT_wc + MAP_wc + OCS + permafrost + modis + srad + Soil_drainage, data = srdb_hl)
summary(rm_rem)
srdb_hlf2 <- filter(srdb_hl, !is.na(MAT_wc) & !is.na(MAP_wc) & !is.na(OCS) & !is.na(Soil_drainage) & !is.na(modis))
srdb_hlf2$model <- predict(rm_rem) ^2
rp_rem <- ggplot(srdb_hlf2, aes(Rs_annual, model)) + geom_point() + geom_abline()
res_rem <- ggplot(srdb_hlf2, aes(model, Rs_annual-model)) +
  geom_point()

#Only on site data
#55.43%
rm_on <- lm(sqrt(Rs_annual) ~ MAT + MAP + NPP + Soil_drainage + ANPP + Soil_drainage, data = srdb_hl)
summary(rm_on)

#Info on important models
Anova(rm_sat)
Anova(rm_locA)
Anova(rm_locN)
