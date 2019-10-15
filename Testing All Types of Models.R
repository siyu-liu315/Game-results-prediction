library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(glmnet)
## set a standard seed number for reproducible results

set.seed(123456)

## First We Try Linear And Forward Selection

## This is our new dataset for the tests
final_15 <- as.data.frame(final_list[15])

## make sure only the minute 15 is in the dataframe
unique(final_15$X15.min)

## 70% of the sample size is our Train
smp_size <- floor(0.70 * nrow(final_15))

train_ind <- sample(seq_len(nrow(final_15)), size = smp_size)

## creating a linear test and train data
linear_train_15 <- final_15[train_ind, ]
linear_test_15 <- final_15[-train_ind, ]

xnames <- colnames(final_15)
xnames <- xnames[!xnames %in% c("X15.min", "X15.matchname", "X15.bResult")]




## We Can Create A Single Linear Model With All of The Predictors
linear_train_15 <- linear_train_15 %>% select(-X15.matchname, -X15.min)

linear_all_predictors <- lm(X15.bResult ~ ., data = linear_train_15)

yhat_train_15_linear <- predict(linear_all_predictors, linear_train_15)
mse_train_15_linear <- mean((linear_train_15$X15.bResult - yhat_train_15_linear) ^ 2)
yhat_test_15_linear <- predict(linear_all_predictors, linear_train_15)
mse_test_15_linear <- mean((linear_test_15$X15.bResult - yhat_test_15_linear) ^ 2)

all_predictors_train_mse <- mse_train_15_linear
all_predictors_test_mse <- mse_test_15_linear

all_predictors_train_mse
all_predictors_test_mse




## Forward Selection Start
## fit the intercept to start the model

fit_linear_15 <- lm(X15.bResult ~ 1, data = linear_train_15)

yhat_train_15_linear <- predict(fit_linear_15, linear_train_15)
mse_train_15_linear <- mean((linear_train_15$X15.bResult - yhat_train_15_linear) ^ 2)
yhat_test_15_linear <- predict(fit_linear_15, linear_train_15)
mse_test_15_linear <- mean((linear_train_15$X15.bResult - yhat_test_15_linear) ^ 2)

##create a log for our results

log_fw <-
  tibble(
    xname = "intercept",
    model = deparse(fit_linear_15$call),
    mse_train = mse_train_15_linear,
    mse_test = mse_test_15_linear
  )


## Loop to Forward Selection Linear Model

while (length(xnames) > 0) {
  best_mse_train_15_linear <- NA
  best_mse_test_15_linear <- NA
  best_fit_fw_15_linear <- NA
  best_xname_15_linear <- NA
  
  
  for (xname in xnames) {
    fit_fw_tmp <- update(fit_linear_15, as.formula(paste0(". ~ . + ", xname)))
    
    
    yhat_train_tmp <- predict(fit_fw_tmp, linear_train_15)
    mse_train_tmp <- mean((linear_train_15$X15.bResult - yhat_train_tmp) ^ 2)
    
    
    yhat_test_tmp <- predict(fit_fw_tmp, linear_test_15)
    mse_test_tmp <- mean((linear_test_15$X15.bResult - yhat_test_tmp) ^ 2)
    
    
    if (is.na(best_mse_test_15_linear) | mse_test_tmp < best_mse_test_15_linear) {
      best_xname_15_linear <- xname
      best_fit_fw_15_linear <- fit_fw_tmp
      best_mse_train_15_linear <- mse_train_tmp
      best_mse_test_15_linear <- mse_test_tmp
    }
  }
  
  log_fw <-
    log_fw %>% add_row(
      xname = best_xname_15_linear,
      model = paste0(deparse(best_fit_fw_15_linear$call), collapse = ""),
      mse_train = best_mse_train_15_linear,
      mse_test = best_mse_test_15_linear
    )
  
  fit_linear_15 <- best_fit_fw_15_linear
  
  xnames <- xnames[xnames!=best_xname_15_linear]
}


## We Can Graph the MSE For Linear Forward Selection

ggplot(log_fw, aes(seq_along(xname), mse_test)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=mse_train), color="blue") +
  geom_line(aes(y=mse_train), color="blue") +
  scale_x_continuous("Variables", labels = log_fw$xname, breaks = seq_along(log_fw$xname)) +
  scale_y_continuous("MSE test") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Let Us Create The Same Train And Test

final_15$train <- sample(c(0, 1), nrow(final_15), replace = TRUE, prob = c(.3, .7))
test <- final_15 %>% filter(train == 0)
train <- final_15 %>% filter(train == 1)

x_train <- train %>% select(-X15.bResult, -train, -X15.matchname, -X15.min)
y_train <- train %>% select(X15.bResult)
x_test <- test %>% select(-X15.bResult, -train, -X15.matchname, -X15.min)
y_test <- test %>% select(X15.bResult)

x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)




## We Can Use Ridge Regression

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




## We Can Try It With LASSO 
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




## Creating The Test And Train For The Trees

train_tree <- train %>% select(-train, -X15.matchname, -X15.min)
test_tree <- test %>% select(-train, -X15.matchname, -X15.min)

## Unline Regression Models Trees and Forests Can Do Classification

train_tree$X15.bResult <- as.factor(train_tree$X15.bResult)
test_tree$X15.bResult <- as.factor(test_tree$X15.bResult)



## Create A Tree

f1 <- as.formula(X15.bResult ~ .)

tree_final_15 <- rpart(f1,
                  train_tree, 
                  method = "class")

## Create The Predictions For Our Tree 
yhat.train.tree <- predict(tree_final_15, train_tree)
yhat.test.tree <- predict(tree_final_15, test_tree)



## Plot The Tree

rpart.plot(tree_final_15)




## Lets Take A Look At Random Forest

rf_final_15 <- randomForest(f1,
                         train_tree,
                         ntree=5000,
                         do.trace=T)


varImpPlot(rf_final_15)

rf_y_train_hat <- predict(rf_final_15, train_tree)
rf_y_test_hat <- predict(rf_final_15, test_tree)

rf_error_dataset_train <- cbind.data.frame(rf_y_train_hat, train_tree$X15.bResult)
rf_error_dataset_train <- rf_error_dataset_train %>%  
                              mutate(correct_prediction = 
                                       rf_error_dataset_train$rf_y_train_hat == rf_error_dataset_train$`train_tree$X15.bResult`)

rf_error_dataset_test <- cbind.data.frame(rf_y_test_hat, test_tree$X15.bResult)
rf_error_dataset_test <- rf_error_dataset_test %>%  
                             mutate(correct_prediction = 
                                       rf_error_dataset_test$rf_y_test_hat == rf_error_dataset_test$`test_tree$X15.bResult`)


rf_error_rate_train <- 1 - (length(rf_error_dataset_train[rf_error_dataset_train == TRUE]) / length(rf_error_dataset_train$correct_prediction))
rf_error_rate_test <- 1 - (length(rf_error_dataset_test[rf_error_dataset_test == TRUE]) / length(rf_error_dataset_test$correct_prediction))


## This Is the Percentage of Incorrect Answers of Predictions
rf_error_rate_train
rf_error_rate_test




