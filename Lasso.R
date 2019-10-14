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

est <- glmnet(x_train, y_train, alpha = 1, nlambda = 100)

y_train_hat <- predict(est, x_train) 
y_test_hat <- predict(est, x_test)

y_train <- as.numeric(y_train)
y_test <- as.numeric(y_test)

RSS_train <- (y_train - y_train_hat)^2
RSS_test <- (y_test - y_test_hat)^2 
mse_train <- apply(RSS_train, 2, mean) 
mse_test <- apply(RSS_test, 2, mean)

### Minimum MSE for Train Data
lambda_min_mse_train <- mse_train[which.min(mse_train)] %>% print()

### Minimum MSE for Test Data
lambda_min_mse_test <- mse_test[which.min(mse_test)] %>% print()

# create a tibble of train MSEs and lambdas
dd_mse <- tibble(
  lambda = est$lambda,
  mse = mse_train,
  dataset = "Train"
)
dd_mse <- rbind(dd_mse, tibble(
  lambda = est$lambda,
  mse = mse_test,
  dataset = "Test"
))
# Use the rbind command to combine dd_mse_train # and dd_mse_test into a single data frame

min_train <- dd_mse  %>%
  filter(mse == lambda_min_mse_test)
min_test <- dd_mse  %>%
  filter(mse == lambda_min_mse_train)
min_point <- rbind(min_train, min_test)
ggplot() +
  geom_point(data = min_point, aes(x = lambda, y = mse, color = dataset)) + geom_line(data = dd_mse, aes(x = lambda, y = mse, color = dataset)) + scale_x_reverse() +
  labs(x = "λ", y = "MSE")

nb <- dd_mse %>% filter(dataset == "Test") %>%
  filter(mse == lambda_min_mse_test) %>% select(lambda)

coef(est, s = nb$lambda)


