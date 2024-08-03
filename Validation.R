library(caret)
#Validation of Model
#k-fold validation
set.seed(22) #results range a lot depending on the seed

#rm_best
rand_best <- createDataPartition(srdb_hlf$Rs_annual, p = 0.75, list = FALSE)
train_best <- srdb_hlf[rand_best,]
check_best <- srdb_hlf[-rand_best,]

valMod_best <- lm(sqrt(Rs_annual) ~ NPP + MAT_wc + MAP_wc + OCS + ANPP + Soil_drainage + permafrost + srad, data = train_best)
summary(valMod_best)

guess_best <- predict(valMod_best, check_best)

result_best <- data.frame(R2 = R2(guess_best, check_best$Rs_annual),
                     RMSE = RMSE(guess_best, check_best$Rs_annual),
                     MAE = MAE(guess_best, check_best$Rs_annual))

#rm_rem
rand_rem <- createDataPartition(srdb_hlf2$Rs_annual, p=0.75, list = FALSE)
train_rem <- srdb_hlf2[rand_rem,]
check_rem <- srdb_hlf2[-rand_rem,]

valMod_rem <- lm(sqrt(Rs_annual) ~ MAT_wc + MAP_wc + OCS + permafrost + modis + srad + Soil_drainage, data = train_rem)
summary(valMod_rem)

guess_rem <- predict(valMod_rem, check_rem)

result_rem <- data.frame(R2 = R2(guess_rem, check_rem$Rs_annual),
                          RMSE = RMSE(guess_rem, check_rem$Rs_annual),
                          MAE = MAE(guess_rem, check_rem$Rs_annual))
