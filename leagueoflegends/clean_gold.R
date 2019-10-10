library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)


### clean gold ##
gold <- read.csv("leagueoflegends/gold.csv")
cclean_gold <- gold %>% 
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
View(monsters)
tag_monster <- monster %>% 
  filter(Team %in% c('bBarons','bDragons','bHeralds'))%>% 
  select(Address:Type) %>% 
  mutate(Time_int = ceiling(Time))
View(tag_monster)

### diviede by different type of monster
air_dragon <- tag_monster %>% filter(Type == 'AIR_DRAGON')
earth_dragon <- tag_monster %>% filter(Type =='EARTH_DRAGON')
fire_dragon <- tag_monster %>% filter(Type =='FIRE_DRAGON')
water_dragon <- tag_monster %>% filter(Type =='WATER_DRAGON')
dragon <- tag_monster %>% filter(Type =='DRAGON')
baron <- tag_monster %>% filter(Type =='BARON_NASHOR')
elder_dragon <- tag_monster %>% filter(Type =='ELDER_DRAGON')
rift_herald <- tag_monster %>% filter(Type =='RIFT_HERALD')


##get unique gamename
game <- unique(monster$Address) 


###########################################air_dragon###################################
##create vector to hold all data
matchname <- vector()
min <- vector()
type <- vector()

##loop will output theree vetor:matchname, min, type, needed to be input df.
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
    matchname <- c(matchname, n)
    min<- c(min,paste("min",i, sep = "_"))
    type<- c(type, x)
  }
  print(n)
}


## insert df.
df_air_dragon <- data.frame(matchname = matchname,
                 min = min,
                 air_dragon = type)


##need to check whether this loop correct.
check <- merge(df_air_dragon,air_dragon, by.x = "matchname", by.y = "Address")
check <- check %>% arrange(matchname,min)

##save as csv
write.csv(df_air_dragon,file = "clean_air_dragon.csv")
write.csv(check,file = 'need_to_check.csv')


############################################rift_herald ########################################
##create vector to hold all data
matchname <- vector()
min <- vector()
type <- vector()

for (n in game){
  x = 0
  match <- rift_herald  %>% filter(Address == n)
  row = nrow(match)
  for (i in 1:60) { 
    if (i %in% match$Time_int[1:row])
    {
      x = x+1}
    else{
      x = x
    }
    matchname <- c(matchname, n)
    min<- c(min,paste("min",i, sep = "_"))
    type <- c(type, x)
  }
  print(n)
}

df_rift_herald <- data.frame(matchname = matchname,
                            min = min,
                            rift_herald = type)

write.csv(df_rift_herald,file = "clean_rift_herald.csv")


############################################baron ########################################
##create vector to hold all data
matchname <- vector()
min <- vector()
type <- vector()

for (n in game){
  x = 0
  match <- baron  %>% filter(Address == n)
  row = nrow(match)
  for (i in 1:60) { 
    if (i %in% match$Time_int[1:row])
    {
      x = x+1}
    else{
      x = x
    }
    matchname <- c(matchname, n)
    min<- c(min,paste("min",i, sep = "_"))
    type <- c(type, x)
  }
  print(n)
}


df_baron <- data.frame(matchname = matchname,
                             min = min,
                             baron = type)

write.csv(df_baron,file = "clean_baron.csv")


############################################elder_dragon ########################################
##create vector to hold all data
matchname <- vector()
min <- vector()
type <- vector()

for (n in game){
  x = 0
  match <- elder_dragon  %>% filter(Address == n)
  row = nrow(match)
  for (i in 1:60) { 
    if (i %in% match$Time_int[1:row])
    {
      x = x+1}
    else{
      x = x
    }
    matchname <- c(matchname, n)
    min<- c(min,paste("min",i, sep = "_"))
    type <- c(type, x)
  }
  print(n)
}


df_elder_dragon <- data.frame(matchname = matchname,
                              min = min,
                              elder_dragon = type)

write.csv(df_elder_dragon,file = "clean_elder_dragon.csv")



##draft
### could use loop to assign
##vec <- c('AIR_DRAGON', 'EARTH_DRAGON', 'FIRE_DRAGON', 'WATER_DRAGON',
##'DRAGON','BARON_NASHOR', 'ELDER_DRAGON', 'rift_herald')
##tb <- tibble(match="", min = "",air_dragon= "")
##count <- tag_monster %>% 
##count(vars = c('Address', 'Type'))
##tb$min[i] <- paste("min",i, sep = "_")
##tb$AIR_DRAGON[i] <- x
##tb$match[i] <- n
