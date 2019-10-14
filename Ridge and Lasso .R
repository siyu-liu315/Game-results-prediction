library(ggplot2) 
library(ggthemes) 
library(glmnet)
library(tidyverse)
library(ggthemes)
library(glmnet) # this is the library that allows us to run penalized regressions
theme_set(theme_bw())

final_30 <- final %>% filter(min == 12)
view(final_30)

set.seed(10086)
final_30$train <- sample(c(0, 1), nrow(final_30), replace = TRUE, prob = c(.3, .7))
test <- final_30 %>% filter(train == 0)
train <- final_30 %>% filter(train == 1)



x_train <- train %>% select(-bResult, -train, -matchname)
y_train <- train %>% select(bResult)
x_test <- test %>% select(-bResult, -train, -matchname)
y_test <- test %>% select(bResult)

x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)

###   Ridge
Ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)

Ridge_y_train_hat <- predict(Ridge, x_train, s = Ridge$lambda.min)
Ridge_y_test_hat <- predict(Ridge, x_test, s = Ridge$lambda.min)

y_train <- as.numeric(y_train)
y_test <- as.numeric(y_test)

Ridge_RSS_train <- (y_train - Ridge_y_train_hat)^2
Ridge_RSS_test <- (y_test - Ridge_y_test_hat)^2 
Ridge_mse_train <- apply(Ridge_RSS_train, 2, mean)
Ridge_mse_test <- apply(Ridge_RSS_test, 2, mean)

Ridge_lambda_min_mse_train <- Ridge_mse_train[which.min(Ridge_mse_train)] %>% print()
Ridge_lambda_min_mse_test <- Ridge_mse_test[which.min(Ridge_mse_test)] %>% print()

coef(Ridge)

### LASSO 
Lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

Lasso_y_train_hat <- predict(Lasso, x_train, s = Lasso$lambda.min)
Lasso_y_test_hat <- predict(Lasso, x_test, s = Lasso$lambda.min)

y_train <- as.numeric(y_train)
y_test <- as.numeric(y_test)

Lasso_RSS_train <- (y_train - Lasso_y_train_hat)^2
Lasso_RSS_test <- (y_test - Lasso_y_test_hat)^2 
Lasso_mse_train <- apply(Lasso_RSS_train, 2, mean)
Lasso_mse_test <- apply(Lasso_RSS_test, 2, mean)

Lasso_lambda_min_mse_train <- Lasso_mse_train[which.min(Lasso_mse_train)] %>% print()
Lasso_lambda_min_mse_test <- Lasso_mse_test[which.min(Lasso_mse_test)] %>% print()

coef(Lasso)





