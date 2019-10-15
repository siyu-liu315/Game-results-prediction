library(ggplot2) 
library(ggthemes) 
library(glmnet)
library(tidyverse)
library(ggthemes)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(glmnet) # this is the library that allows us to run penalized regressions
theme_set(theme_bw())

##### Random Forest 

f1 <- as.formula(bResult ~ golddiffADC + golddiffJungle + golddiffMiddle 
                 + golddiffSupport + golddiffTop + top_outer_accum + top_inner_accum
                 + top_base_accum + mid_outer_accum + mid_inner_accum + mid_base_accum
                 + mid_inhibitor_accum + bot_outer_accum + bot_inner_accum + bot_base_accum
                 + nexus_turret_accum + killer_accum + air_dragon_accum + earth_dragon_accum
                 + fire_dragon_accum + water_dragon_accum + dragon_accum + baron_accum
                 + elder_dragon_accum + rift_herald_accum)

##
final <- read.csv('final.csv')

rf_error_dataset_test <- tibble(min = '', y_test_hat ="", y_test = "")

### train_set spli

for (i in 1:60){
  
  df <- final %>% filter(min == i)
  
  df$train <- sample(c(0, 1), nrow(df), replace = TRUE, prob = c(.3, .7))
  
  test <- df%>% filter(train == 0)
  train <-df%>% filter(train == 1)
  
  x_train <- train %>% select(-bResult, -train, -matchname)
  y_train <- train %>% select(bResult)
  
  x_test <- test %>% select(-bResult, -train, -matchname)
  y_test <- test %>% select(bResult)
  
  rf_train <- randomForest(f1,
                          train,
                          ntree=5,
                          do.trace= T)
  
  rf_y_test_hat <- predict(rf_train, x_test)
  rf_y_train_hat <- predict(rf_train, x_train)
  
  rf_error_dataset_test <- add_row(rf_error_dataset_test, min = i, y_test_hat = rf_y_test_hat, y_test = y_test)
} 
  
  
  
  
  
  rf_error_dataset_test <- rf_error_dataset_test %>%  
    mutate(correct_prediction = 
             rf_error_dataset_test$rf_y_test_hat == rf_error_dataset_test$`test_tree$X15.bResult`)
  
  x
  rf_error_rate_train <- 1 - (length(rf_error_dataset_train[rf_error_dataset_train == TRUE]) / length(rf_error_dataset_train$correct_prediction))
  rf_error_rate_test <- 1 - (length(rf_error_dataset_test[rf_error_dataset_test == TRUE]) / length(rf_error_dataset_test$correct_prediction))
  
  
   
  
  ### MSE for Train Data
  mse_tibble <-  add_row(mse_tibble,min = i,mse_train = rf_mse_train, mse_test = rf_mse_test)
  }


