library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)


### clean gold ##
gold <- read.csv("leagueoflegends/gold.csv")
clean_gold <- gold %>% 
  filter(is.na(min_61)) %>%  
  select(Address:min_60) %>% 
  gather(min_1:min_60,key = 'min',value = 'golddiff')%>% 
  spread(key = Type, value  = golddiff) %>% arrange(min, Address) %>% 
  mutate(golddiffADC = goldblueADC -goldredADC, 
        golddiffJungle = goldblueJungle - goldredJungle,
        golddiffMiddle = goldblueMiddle - goldredMiddle,
        golddiffSupport = goldblueSupport - goldredSupport,
        golddiffTop = goldblueTop-goldredTop)

clean <- clean_gold %>% select(-(3:15))
