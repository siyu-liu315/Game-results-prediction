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