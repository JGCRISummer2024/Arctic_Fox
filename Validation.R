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

##rm remote MODIS
result_sat <- list()
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
  #Plot use seed 13 because its closest to average
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
for(i in 1:13){
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


