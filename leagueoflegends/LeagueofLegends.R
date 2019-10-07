library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)


####this is the old version!!!!!

### clean gold, tag time range##
gold <- read.csv("leagueoflegends/gold.csv")
clean_gold <- gold %>% 
  filter(is.na(min_61)) %>%  
  select(Address:min_60) %>% 
  gather(min_1:min_60,key = 'min',value = 'golddiff')%>% 
  mutate(golddiff_range  =  ifelse(min %in% c("min_1","min_2","min_3","min_4","min_5"),"(0,5]",
                            ifelse(min%in% c("min_6","min_7","min_8","min_9","min_10"),"(0,10]",
                            ifelse(min%in% c("min_11","min_12","min_13","min_14","min_15"),"(0,15]",
                            ifelse(min%in% c("min_16","min_17","min_18","min_19","min_20"),"(0,20]",
                            ifelse(min%in% c("min_21","min_22","min_23","min_24","min_25"),"(0,25]",
                            ifelse(min%in% c("min_26","min_27","min_28","min_29","min_30"),"(0,30]",
                            ifelse(min%in% c("min_31","min_32","min_33","min_34","min_35"),"(0,35]",
                            ifelse(min%in% c("min_36","min_37","min_38","min_39","min_40"),"(0,40]",
                            ifelse(min%in% c("min_41","min_42","min_43","min_44","min_45"),"(0,45]",
                            ifelse(min%in% c("min_46","min_47","min_48","min_49","min_50"),"(0,50]",
                            ifelse(min%in% c("min_51","min_52","min_53","min_54","min_55"),"(0,55]",
                            ifelse(min%in% c("min_56","min_57","min_58","min_59","min_60"),"(0,60]",NA)))))))))))))
          
##ifelse(min%in% c("min_11","min_12","min_13","min_14","min_15"),"(10,15]",NA)##
##ifelse(min%in% c("min_16","min_17","min_18","min_19","min_20"),"(15,20]",NA)

###clean kills, tag time range
kills <- read.csv("leagueoflegends/kills.csv")
clean_kills <- kills %>% filter(Time <= 60) %>% 
  select(Address:Time) %>% 
  mutate(kills_range = ifelse(Time <= 5,"(0,5]",
                       ifelse(Time <= 10,"(0,10]",
                       ifelse(Time <= 15,"(0,15]",
                       ifelse(Time <= 20,"(0,20]",
                       ifelse(Time <= 25,"(0,25]",
                       ifelse(Time <= 30,"(0,30]",
                       ifelse(Time <= 35,"(0,35]",
                       ifelse(Time <= 40,"(0,40]",
                       ifelse(Time <= 45,"(0,45]",
                       ifelse(Time <= 50,"(0,50]",
                       ifelse(Time <= 55,"(0,55]",
                       ifelse(Time <= 60, "(0,60]"))))))))))))) 

###count kills number by timerange and team
kills_count <- clean_kills %>% 
  group_by(kills_range,Address) %>%
  summarise(n())



##merge kill and gold
merge1 <- merge(kills_count, clean_gold, by.x = c("Address", "kills_range"), by.y= c("Address", "golddiff_range"))
  

##clean monster by tagging monster type, time range, and count
monster <- read.csv("monsters.csv")
tag_monster <- monster %>% 
  select(Address:Type) %>% 
  mutate(monster_Type = ifelse(Type %in% c('AIR_DRAGON','WATER_DRAGON', "EARTH_DRAGON","FIRE_DRAGON"), "mini_dragon",
                                                                                   ifelse(Type %in% c("DRAGON", "ELDER_DRAGON", "BARON_NASHOR"),"dragon", "rift_herald"))) ## 
clean_monster <- tag_monster %>% 
  mutate(monster_range = ifelse(Time <= 5,"(0,5]",
                       ifelse(Time <= 10,"(0,10]",
                       ifelse(Time <= 15,"(0,15]",
                       ifelse(Time <= 20,"(0,20]",
                       ifelse(Time <= 25,"(0,25]",
                       ifelse(Time <= 30,"(0,30]",
                       ifelse(Time <= 35,"(0,35]",
                       ifelse(Time <= 40,"(0,40]",
                       ifelse(Time <= 45,"(0,45]",
                       ifelse(Time <= 50,"(0,50]",
                       ifelse(Time <= 55,"(0,55]",
                       ifelse(Time <= 60, "(0,60]",NA))))))))))))) 

monster_count <- clean_monster %>% 
  group_by(monster_range,monster_Type, Address) %>%
  summarise(n())

### mearge monster and matchinfo
matchinfo <- read.csv("leagueoflegends/matchinfo.csv")
merge2 <- merge(merge1,monster_count,by.x = c("Address", "kills_range"),by.y = c("Address", "monster_range") )
merge3 <- merge(merge2, matchinfo,by = "Address") %>% 
  select("kills_range","bResult","rResult","golddiff","n().x","monster_Type","n().y","blueTeamTag","redTeamTag","Address") %>% 
  arrange(kills_range,Address)

merge3 <- plyr::rename(merge3, c("n().x" = "kills_count"))
merge3 <- plyr::rename(merge3, c("n().y" = "dragon_count"))

merge3 <- merge3 %>% select(-'Address')
merge3 %>% group_by(blueTeamTag, redTeamTag) %>% 
  mutate(game_count = count())

