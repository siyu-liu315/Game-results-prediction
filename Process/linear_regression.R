# Linear Regression Test

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

## We are going to use minute 15 to test all of the methods for supervised machine learning to see which best fits. 

View(final_list)

## This is our new dataset for the tests
final_15 <- as.data.frame(final_list[15])

## make sure only the minute 15 is in the dataframe
unique(final_15$X15.min)

## set a standard seed number for reproducible results

set.seed(123456)

## 80% of the sample size
smp_size <- floor(0.70 * nrow(final_15))

train_ind <- sample(seq_len(nrow(final_15)), size = smp_size)

## creating a linear test and train data
linear_train_15 <- final_15[train_ind, ]
linear_test_15 <- final_15[-train_ind, ]

xnames <- colnames(final_15)
xnames <- xnames[!xnames %in% c("X15.min", "X15.matchname", "X15.bResult")]

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

View(log_fw)

ggplot(log_fw, aes(seq_along(xname), mse_test)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=mse_train), color="blue") +
  geom_line(aes(y=mse_train), color="blue") +
  scale_x_continuous("Variables", labels = log_fw$xname, breaks = seq_along(log_fw$xname)) +
  scale_y_continuous("MSE test") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



