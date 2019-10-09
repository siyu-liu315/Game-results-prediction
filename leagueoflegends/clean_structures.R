library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)
library(bigrquery)
### shorten the address to unique game ID
structures <- read.csv("leagueoflegends/structures.csv")
structures$Address <- gsub(".*=","",structures$Address)
structures$Time <- as.integer(structures$Time) + 1

### We only foucs on Blue Team
bhome <- structures %>% filter(Team == "bInhibs" | Team =="bTowers")

### Change all Fountain Turret to Base Turret
unique(bhome$Type)
unique(bhome$Lane)
class(bhome$Type)

### replace them
bhome$Type <- as.vector(bhome$Type)
bhome$Type <- str_replace(bhome$Type, "FOUNTAIN_TURRET", "BASE_TURRET")

### see if the code works
tem <- structures %>% filter(Address == "055b17da8456fdc8") %>% view()

#### create columns for each type if tower and inhibiors

bhome %>% mutate(top_outer = (Lane == "TOP_LANE" & Type == "OUTER_TURRET"),
                 top_inner = (Lane == "TOP_LANE" & Type == "INNER_TURRET"),
                 top_base = (Lane == "TOP_LANE" & Type == "BASE_TURRET")) %>%
  mutate(mid_outer = (Lane == "MID_LANE" & Type == "OUTER_TURRET"),
         mid_inner = (Lane == "MID_LANE" & Type == "INNER_TURRET"),
         mid_base = (Lane == "MID_LANE" & Type == "BASE_TURRET"),
         mid_inhibitor = (Lane == "MID_LANE" & Type == "INHIBITOR")) %>% 
  mutate(bot_outer = (Lane == "BOT_LANE" & Type == "OUTER_TURRET"),
         bot_inner = (Lane == "BOT_LANE" & Type == "INNER_TURRET"),
         bot_base = (Lane == "BOT_LANE" & Type == "BASE_TURRET"),
         nexus_turret = (Lane == "MID_LANE" & Type == "NEXUS_TURRET")) -> bhome

### replace Ture or False to 0/1
bhome %>% select(-c(Address, Team, Lane, Type, Time)) -> tower
infor <- bhome %>% select(c(Address, Team, Lane, Type, Time)) 
tower <- ifelse(tower == "TRUE", 1, 0)
b_tower <- cbind(infor, tower)

## test (1 minute 1 tower?)
rowSums(tower) %>% unique()

### remove the NA
b_tower <- na.omit(b_tower)

####change Time formate
b_tower$Time <- paste('min',b_tower$Time,sep = '_')

##change column name
monster$matchname <- gsub(".*=","",monster$matchname)
names(b_tower)[1]<-"matchname"
names(b_tower)[5] <- 'min'

##left join and select
merge <- left_join(monster, b_tower, by = c('matchname', 'min'))%>% arrange(matchname)
merge <- merge[-(11:13)]

###repalce Na
merge[is.na(merge)] <- 0

