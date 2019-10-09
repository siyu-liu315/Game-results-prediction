library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)
library(bigrquery)

########################construction##############################
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
tem <- structures %>% filter(Address == "055b17da8456fdc8") 

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

#########################################monster#########################################

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

##change column name
monster$matchname <- gsub(".*=","",monster$matchname)
names(b_tower)[1]<-"matchname"
names(b_tower)[5] <- 'min'


##change min to number###
monster <- separate(monster, min, into =c('del', 'min'), "_")
monster <- monster[-2]
monster$min <- as.numeric(monster$min)


###################### join kills.and monster & tower ##############

kills <- read.csv("leagueoflegends/kills.csv")
kills$Time <- as.integer(kills$Time) + 1
kills$Address <- gsub(".*=","",kills$Address)
killing <- kills %>%
  filter(Team == 'bKills') %>% 
  select("Address","Team","Time") %>% mutate(killers = 1) 

killing <- killing[-2]
names(killing)[1] <- "matchname"
names(killing)[2] <- 'min'
merge1<- left_join(monster, killing, by = c('matchname', 'min'))

names(b_tower)[1] <- "matchname"
names(b_tower)[5] <- 'min'
b_tower <- b_tower[-(2:4)]


merge <- left_join(merge1, b_tower, by = c('matchname', 'min'))

###repalce Na
merge[is.na(merge)] <- 0

##accumulate
try <- merge %>% group_by(matchname) %>% 
  dplyr::mutate(top_outer_cum = cumsum(top_outer),
                top_inner_cum = cumsum(top_inner),
                top_base_cum = cumsum(top_base),
                mid_outer_cum = cumsum(mid_outer),
                mid_inner_cum = cumsum(mid_inner),
                mid_base_cum = cumsum(mid_base),
                mid_inhibitor_cum = cumsum(mid_inhibitor),
                bot_outer_cum = cumsum(bot_outer),
                bot_inner_cum = cumsum(bot_inner),
                bot_base_cum = cumsum(bot_base),
                nexus_turret_cum = cumsum(nexus_turret),
                killers_cum = cumsum(killers))

final <- try[-(12:22)]
