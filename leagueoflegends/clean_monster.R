library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)

air_dragon <- read.csv('./clean_air_dragon.csv')
baron <- read.csv('./clean_baron.csv')
dragon <- read.csv('./clean_dragon.csv')
earth_dragon <- read.csv('./clean_earth_dragon.csv')
elder_dragon <- read.csv('./clean_elder_dragon.csv')
fire_dragon <- read.csv('./clean_fire_dragon.csv')
rift_herald <- read.csv('./clean_rift_herald.csv')
water_dragon <- read.csv('./clean_water_dragon.csv')

monster <- merge(air_dragon,baron, by = c('matchname','min'))
monster <- merge(monster, dragon, by = c('matchname','min'))
monster <- merge(monster, earth_dragon, by = c('matchname','min'))
monster <- monster[c(1,2,4,6,8,10)]
monster <- merge(monster, fire_dragon,by= c('matchname','min'))
monster <- merge(monster, rift_herald, by = c('matchname', 'min'))
monster <- merge(monster, water_dragon, by = c('matchname', 'min'))
monster <- merge(monster,elder_dragon,by = c('matchname','min'))
monster <- monster[c(1:6,8,10,12,14)]

monster <- monster %>% arrange(matchname, min)
