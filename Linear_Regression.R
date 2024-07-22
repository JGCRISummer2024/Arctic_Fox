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
#Explains 12.38% of variability in Rs_annual (from adjusted R^2)
srdb_hl <- srdb_hl %>% filter(!is.na(MAT_wc))
rm2 <- lm(Rs_annual ~ MAT_wc, data = srdb_hl)
summary(rm2)
srdb_hl$MATmodel <- predict(rm2)
MATrp <- ggplot(srdb_hl, aes(MAT_wc, Rs_annual)) +
  geom_point() + 
  geom_line(mapping = aes(y = MATmodel), color = "blue", linewidth = 1)

#High Latitude Data Rs_annual as a function of MAP_wc
#Explains 5.068% of variability in Rs_annual (from adjusted R^2)
srdb_hl <- srdb_hl %>% filter(!is.na(MAP_wc))
rm3 <- lm(Rs_annual ~ MAP_wc, data = srdb_hl)
summary(rm3)
srdb_hl$MAPmodel <- predict(rm3)
MAPrp <- ggplot(srdb_hl, aes(MAP_wc, Rs_annual)) +
  geom_point() + 
  geom_line(mapping = aes(y = MAPmodel), color = "blue", linewidth = 1)

#High Latitude Data Rs_annual as a function of MAT_wc and MAP_wc
#Explains 12.3% of variability in Rs_annual (from adjusted R^2)
#This is actually slightly worse than the MAT only model
rm4 <- lm(Rs_annual ~ MAT_wc + MAP_wc, data = srdb_hl)
summary(rm4)
srdb_hl$multiModel <- predict(rm4)
multiRP <- ggplot(srdb_hl, aes(MAT_wc, Rs_annual, size = MAP_wc)) +
  geom_point() + 
  geom_line(mapping = aes(y = multiModel), color = "blue", linewidth = 1)

#Plot residuals
MATres <- ggplot(srdb_hl, aes(MATmodel, Rs_annual-MATmodel)) +
  geom_point()
MAPres <- ggplot(srdb_hl, aes(MAPmodel, Rs_annual-MAPmodel)) +
  geom_point()
