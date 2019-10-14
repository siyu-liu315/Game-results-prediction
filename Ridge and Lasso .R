install.packages(c("rpart.plot", "rpart"))
install.packages(c("randomForest", "gbm"))
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

final_20 <- final %>% filter(min == 20)
view(final_20)

set.seed(10086)
final_20$train <- sample(c(0, 1), nrow(final_20), replace = TRUE, prob = c(.3, .7))
test <- final_20 %>% filter(train == 0)
train <- final_20 %>% filter(train == 1)



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

### Random Forest 
f1 <- as.formula(bResult ~ golddiffADC + golddiffJungle + golddiffMiddle 
                 + golddiffSupport + golddiffTop + top_outer_cum + top_inner_cum
                 + top_base_cum + mid_outer_cum + mid_inner_cum + mid_base_cum
                 + mid_inhibitor_cum + bot_outer_cum + bot_inner_cum + bot_base_cum
                 + nexus_turret_cum + killer_cum + air_dragon_cum + earth_dragon_cum
                 + fire_dragon_cum + water_dragon_cum + dragon_cum + baron_cum
                 + elder_dragon_cum + rift_herald_cum)
### Train random forest 
rf_train <- randomForest(f1,
                       train,
                       ntree=100,
                       do.trace=T)
varImpPlot(rf_train)

rf_y_train_hat <- predict(rf_train, x_train)

rf_mse_train <- mean((rf_y_train_hat - y_train) ^ 2)
### MSE for Train Data
print(rf_mse_train)

## Random Forest for Test
rf_test <- randomForest(f1,
                         test,
                         ntree=100,
                         do.trace= T)
rf_y_test_hat <- predict(rf_test, x_test)

rf_mse_test <- mean((rf_y_test_hat - y_test) ^ 2)
### MSE for Train Data
print(rf_mse_test)

### Tree but useless 
fit.tree <- rpart(f1,
                  train,
                  control = rpart.control(cp = 0.1))
par(xpd = TRUE)
plot(fit.tree, compress=TRUE)
text(fit.tree, use.n=TRUE)

rpart.plot(fit.tree)


