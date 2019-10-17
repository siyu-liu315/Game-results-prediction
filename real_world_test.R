###real test
game1 <- data.frame(X15.golddiffADC = c(-963,-388,-1567),
                X15.golddiffJungle = c(50,-935,386),
                X15.golddiffMiddle = c(-2973,-562,839),
                X15.golddiffSupport = c(-325,971,-873),
                X15.golddiffTop = c(-1694,231,735),
                X15.top_outer_accum = c(0,0,0),
                X15.top_inner_accum = c(0,0,0),
                X15.top_base_accum = c(0,0,0),
                X15.mid_outer_accum = c(0,0,1),
                X15.mid_inner_accum = c(0,0,0),
                X15.mid_base_accum = c(0,0,0),
                X15.mid_inhibitor_accum = c(0,0,0),
                X15.bot_outer_accum = c(0,0,0),
                X15.bot_inner_accum = c(0,0,0),
                X15.bot_base_accum = c(0,0,0),
                X15.nexus_turret_accum = c(0,0,0),
                X15.killer_accum = c(1,0,7),
                X15.air_dragon_accum = c(0,0,0),
                X15.earth_dragon_accum = c(0,0,0),
                X15.fire_dragon_accum = c(0,0,0),
                X15.water_dragon_accum =c(0,0,0),
                X15.dragon_accum = c(0,0,0),
                X15.baron_accum = c(0,0,0),
                X15.elder_dragon_accum =c(0,0,0),
                X15.rift_herald_accum = c(0,0,1),
                X15.bResult  = c(0,0,1))

game1_y_test_hat <- predict(rf_final_15, game1)
actual <- cbind.data.frame(game1_y_test_hat,game1$X15.bResult)
names(actual)<- c('predict','actual')
actual <- actual%>%mutate(correct_prediction = predict == actual) 
actual
mean(actual$correct_prediction)

