library(caret)
#Validation of Model
#k-fold validation

##rm best
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

##rm remote
result_rem <- list()
for(i in 1:30){
  set.seed(i)
  rand_rem <- createDataPartition(srdbhl_sat$Rs_annual, p = 0.75, list = FALSE)
  train_rem <- srdbhl_sat[rand_rem,]
  check_rem <- srdbhl_sat[-rand_rem,]
  valMod_rem <- lm(sqrt(Rs_annual) ~ MAT_wc + MAP_wc + OCS + permafrost + modis + srad + Soil_drainage, data = train_rem)
  guess_rem <- predict(valMod_rem, check_rem)
  go_rem <- data.frame(R2 = R2(guess_rem, check_rem$Rs_annual),
                        RMSE = RMSE(guess_rem, check_rem$Rs_annual),
                        MAE = MAE(guess_rem, check_rem$Rs_annual))
  result_rem[[i]] <- data.frame(go_rem)
}
result_rem <- bind_rows(result_rem)
val_rem <- colMeans(result_rem)

##rm on site ANPP
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

##rm on site NPP
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
  #Doesn't work I think bc train doesn't have a predict col
  if(i == 13){
    valPlot <- ggplot(train_locN, aes(Rs_annual, valMod_locN)) +
      geom_point()
      #geom_point(check_locN, mapping = aes(Rs_annual, ))
  }
}
result_locN <- bind_rows(result_locN)
val_locN <- colMeans(result_locN)


