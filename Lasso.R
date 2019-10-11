library(ggplot2) 
library(ggthemes) 
library(glmnet)
library(tidyverse)
library(ggthemes)
library(glmnet) # this is the library that allows us to run penalized regressions
theme_set(theme_bw())

final_30 <- final %>% filter(min == 30)
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

est <- glmnet(x_train, y_train, alpha = 1, nlambda = 100)

y_train_hat <- predict(est, x_train) 
y_test_hat <- predict(est, x_test)

RSS_train <- (y_train - y_train_hat)^2
RSS_test <- (y_test - y_test_hat)^2 
mse_train <- apply(RSS_train, 2, mean) 
mse_test <- apply(RSS_test, 2, mean)






