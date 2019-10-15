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

final$bResult <- as.factor(final$bResult)

lst <- list()

### train_set spli


for (i in 1:10){
  
  df <- final %>% filter(min == i)
  
  df$train <- sample(c(0, 1), nrow(df), replace = TRUE, prob = c(.3, .7))
  
  test <- df %>% filter(train == 0)
  train <-df %>% filter(train == 1)
  
  x_train <- train %>% select(-bResult, -train, -matchname)
  y_train <- train %>% select(bResult)
  
  x_test <- test %>% select(-bResult, -train, -matchname)
  y_test <- test %>% select(bResult)
  
  
  rf_train <- randomForest(f1,
                          train,
                          ntree=5,
                          do.trace= T)
  
  
  rf_y_train_hat <- predict(rf_train, x_train)
  rf_y_test_hat <- predict(rf_train, x_test)
  
  
  rf_error_dataset_train <- cbind.data.frame(rf_y_train_hat, y_train)
  rf_error_dataset_train <- rf_error_dataset_train %>%  
                               mutate(correct_prediction = 
                                   rf_error_dataset_train$rf_y_train_hat == rf_error_dataset_train$bResult)
  
  
  rf_error_dataset_test <- cbind.data.frame(rf_y_test_hat, y_test)
  rf_error_dataset_test <- rf_error_dataset_test %>%  
                              mutate(correct_prediction = 
                                   rf_error_dataset_test$rf_y_test_hat == rf_error_dataset_test$bResult)
  
  
  rf_error_rate_train <- 1 - (length(rf_error_dataset_train[rf_error_dataset_train == TRUE]) / length(rf_error_dataset_train$correct_prediction))
  rf_error_rate_test <- 1 - (length(rf_error_dataset_test[rf_error_dataset_test == TRUE]) / length(rf_error_dataset_test$correct_prediction))
  
  lst[[i]] <- cbind.data.frame(i, rf_error_rate_train, rf_error_rate_test)
    
} 

View(lst)
  




lst[[i]] <- cbind.data.frame(rf_y_test_hat, y_test) %>% 
  mutate(correct_prediction = 
           rf_y_test_hat == y_test) %>% 
  mutate(error_rate = 1 - (length(correct_prediction[correct_prediction == TRUE])) / length(correct_prediction))

  
  rf_error_dataset_test <- rf_error_dataset_test %>%  
    mutate(correct_prediction = 
             rf_error_dataset_test$rf_y_test_hat == rf_error_dataset_test$`test_tree$X15.bResult`)
  
  
  rf_error_rate_train <- 1 - (length(rf_error_dataset_train[rf_error_dataset_train == TRUE]) / length(rf_error_dataset_train$correct_prediction))
  rf_error_rate_test <- 1 - (length(rf_error_dataset_test[rf_error_dataset_test == TRUE]) / length(rf_error_dataset_test$correct_prediction))
  
  
   
  
 


