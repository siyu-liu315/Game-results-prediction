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


##clean monster
monster <- read.csv("leagueoflegends/monsters.csv")
tag_monster <- monster %>% 
 filter(Team == 'bDragons') %>% 
  select(Address:Type) %>% 
  mutate(monster_type = 
           ifelse(Type %in% c('AIR_DRAGON','WATER_DRAGON', "EARTH_DRAGON","FIRE_DRAGON","DRAGON"), "mini_dragon",
                               ifelse(Type=="BARON_NASHOR","BARON", 
                                      ifelse(Type == "ELDER_DRAGON","ELDER_DRAGON","rift_herald")))) %>% 
  mutate(Time_int = ceiling(Time)) %>% 
  arrange(Address,Type,Time_int)




### could use loop to assign

##vec <- c('AIR_DRAGON', 'EARTH_DRAGON', 'FIRE_DRAGON', 'WATER_DRAGON',
           ##'DRAGON','BARON_NASHOR', 'ELDER_DRAGON', 'rift_herald')


air_dragon <- tag_monster %>% filter(Type == 'AIR_DRAGON')
earth_dragon <- tag_monster %>% filter(Type =='EARTH_DRAGON')
fire_dragon <- tag_monster %>% filter(Type =='FIRE_DRAGON')
water_dragon <- tag_monster %>% filter(Type =='WATER_DRAGON')
dragon <- tag_monster %>% filter(Type =='DRAGON')
baron <- tag_monster %>% filter(Type =='BARON_NASHOR')
elder_dragon <- tag_monster %>% filter(Type =='ELDER_DRAGON')
rift_herald <- tag_monster %>% filter(Type =='rift_herald')

tb <- tibble(match="", min = "",air_dragon= "")
game <- unique(monster$Address) 

for (n in game){
  x = 0
  match <- air_dragon %>% filter(Address == n)
  row = nrow(match)
  for (i in 1:60) { 
    if (i %in% match$Time_int[1:row])
      {
      x = x+1}
    else{
        x = x
        }
     vec <- c(match = n, min = paste("min",i, sep = "_"),air_dragon = x)
     tb <- rbind(tb,vec)
  }
  print(n)
}

x = 0
for (n in game){
  match <- air_dragon %>% filter(Address == n)
  row = nrow(match)
  for (i in 1:60) { 
    if (i %in% match$Time_int[1:row])
    {
      x = x+1}
    else{
      x = x
    }
    vec <- c(match = n, min = paste("min",i, sep = "_"),type = x)
    tb <- rbind(tb,vec)
  }
}



##tb$min[i] <- paste("min",i, sep = "_")
##tb$AIR_DRAGON[i] <- x
##tb$match[i] <- n



##
count <- tag_monster %>% 
  count(vars = c('Address', 'Type'))


