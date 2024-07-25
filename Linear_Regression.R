#Trying a linear regression model
library(dplyr)

#Trying with test data set
test$lrm <- c(3, 7, 8, 14, 14, 19, 22, 23)
rm1 <- lm(lrm ~ ID, data = test)
summary(rm1)
test$model <- predict(rm1)
testRP <- ggplot(test, aes(ID, lrm)) +
    geom_point() + 
    geom_line(mapping = aes(y = model), linetype = 2, color = "blue")

#High Latitude Data Rs_annual as a function of MAT_wc
#Explains 12.13% of variability in Rs_annual (from adjusted R^2)
srdb_hl <- srdb_hl %>% filter(!is.na(MAT_wc))
rm2 <- lm(Rs_annual ~ MAT_wc, data = srdb_hl)
summary(rm2)
srdb_hl$MATmodel <- predict(rm2)
MATrp <- ggplot(srdb_hl, aes(MAT_wc, Rs_annual)) +
  geom_point() + 
  geom_line(mapping = aes(y = MATmodel), color = "blue", linewidth = 1)

#High Latitude Data Rs_annual as a function of MAP_wc
#Explains 0.2911% of variability in Rs_annual (from adjusted R^2)
srdb_hl <- srdb_hl %>% filter(!is.na(MAP_wc))
rm3 <- lm(Rs_annual ~ MAP_wc, data = srdb_hl)
summary(rm3)
srdb_hl$MAPmodel <- predict(rm3)
MAPrp <- ggplot(srdb_hl, aes(MAP_wc, Rs_annual)) +
  geom_point() + 
  geom_line(mapping = aes(y = MAPmodel), color = "blue", linewidth = 1)

#High Latitude Data Rs_annual as a function of MAT_wc and MAP_wc
#Explains 14.22% of variability in Rs_annual (from adjusted R^2)
#This is actually slightly worse than the MAT only model
rm4 <- lm(Rs_annual ~ MAT_wc + MAP_wc, data = srdb_hl)
summary(rm4)
srdb_hl$TPmodel <- predict(rm4)
TPRP <- ggplot(srdb_hl, aes(MAT_wc, Rs_annual, size = MAP_wc)) +
  geom_point() + 
  geom_line(mapping = aes(y = TPmodel), color = "blue", linewidth = 1)

#Plot residuals
MATres <- ggplot(srdb_hl, aes(MATmodel, Rs_annual-MATmodel)) +
  geom_point()
MAPres <- ggplot(srdb_hl, aes(MAPmodel, Rs_annual-MAPmodel)) +
  geom_point()

#High Latitude Data Rs_annual as a function of srad
#Explains 5.842% of variability in Rs_annual (from adjusted R^2)
rm5 <- lm(Rs_annual ~ srad, data = srdb_hl)
summary(rm5)

#High Latitude Data Rs_annual as a function of Soil_drainage
#Explains 13% of variability in Rs_annual (from adjusted R^2)
rm6 <- lm(Rs_annual ~ Soil_drainage, data = srdb_hl)
summary(rm6)

#High Latitude Data Rs_annual as a function of soil C:N (Soil_CN)
#Explains -3.152% of variability in Rs_annual (from adjusted R^2)
rm7 <- lm(Rs_annual ~ Soil_CN, data = srdb_hl)
summary(rm7)

#High Latitude Data Rs_annual as a function of soil net primary production (NPP)
#Explains 29.69% of variability in Rs_annual (from adjusted R^2)
rm8 <- lm(Rs_annual ~ NPP, data = srdb_hl)
summary(rm8)

#High Latitude Data Rs_annual as a function of carbon in soil organic matter (C_soilmineral)
#Explains -0.6379% of variability in Rs_annual (from adjusted R^2)
rm9 <- lm(Rs_annual ~ C_soilmineral, data = srdb_hl)
summary(rm9)

#High Latitude Data Rs_annual as a function of organic carbon storage (OCS)
#Explains 1.541% of variability in Rs_annual (from adjusted R^2)
rm10 <- lm(Rs_annual ~ OCS, data = srdb_hl)
summary(rm10)
ggplot(srdb_hl, aes(OCS, Rs_annual)) + geom_point() + geom_smooth(method = lm)

#High Latitude Data Rs_annual as a function of MODIS NPP
#Explains 11.91% of variability in Rs_annual (from adjusted R^2)
rm10 <- lm(Rs_annual ~ modis, data = srdb_hl)
summary(rm10)
ggplot(srdb_hl, aes(modis, Rs_annual)) + geom_point() + geom_smooth(method = lm)

#High Latitude Data Rs_annual as a function of MAT_wc and Soil_drainage
#Explains 18.06% of variability in Rs_annual (from adjusted R^2)
#I want soil drainage data (Soil_drainage only classifies as dry, mixed, and wet)
#I want soil/SOM carbon content (C_soilmineral has lots of missing data)
#I want NPP or similar plant productivity data (built in NPP has lots of missing data)
rmBest <- lm(Rs_annual ~ modis + MAT_wc + srad, data = srdb_hl)
summary(rmBest)
