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

####change column name
b_tower <- b_tower[c(-2:-4)]
names(b_tower)[1]<-"matchname"
names(b_tower)[2] <- 'min'
View(b_tower)

########################################clean gold###########
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

gold <- clean_gold %>% select(-(3:15))
gold <- separate(gold, min, into =c('del', 'min'), "_")
gold <- gold[-2]
gold$min <- as.numeric(gold$min)

names(gold)[1]<-"matchname"
gold$matchname <- gsub(".*=","",gold$matchname)

#########################################monster#########################################

monster <- read.csv("leagueoflegends/monsters.csv")
match_id <- unique(monster$Address)

monster <- monster %>% 
  filter(Team %in% c('bBarons','bDragons','bHeralds'))%>% 
  select(Address:Type) %>% 
  mutate(Time_int = ceiling(Time)) %>% 
  mutate(Count = 1)

monster <- spread(monster, key = Type, value = Count)%>% 
  arrange(Address, Time_int)

monster <- monster[c(-2,-3)]


##change column name
names(monster)[1]<-"matchname"
names(monster)[2] <- 'min'
monster$matchname <- gsub(".*=","",monster$matchname)
monster[is.na(monster)] <- 0


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

#####build a right structure. 
str <- data.frame(matchname = match_id)
for (x in 1:60){
new_var <- paste("min", x, sep = '_')
str[[new_var]] <- rep(x,7620)}
str <- str %>% gather(key = time ,value = min,2:61) 
str <- str[-2]

names(str)[1] <- "matchname"
str$matchname <- gsub(".*=","",str$matchname)
str
View(str)

str$matchname <- gsub(".*=","",str$matchname)


### merge with right struture first to handle NA.
merge<- left_join(str, monster, by=c('matchname', 'min')) %>%
  left_join(., killing, by=c('matchname', 'min')) %>% 
 HEAD
  left_join(.,b_tower,by=c('matchname', 'min'))
View(merge)
View(killing)
View(b_tower)



  left_join(.,b_tower, by = c('matchname','min'))

merge[is.na(merge)] <- 0


##accumulate number
yy <- merge %>% group_by(matchname) %>% 
  arrange(matchname, min) %>% 
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
                killers_cum = cumsum(killers),
                air_dragon_cum = cumsum(AIR_DRAGON),
                earth_dragon_cum = cumsum(EARTH_DRAGON),
                fire_dragon_cum = cumsum(FIRE_DRAGON),
                water_dragon_cum = cumsum(WATER_DRAGON),
                dragon_cum = cumsum(DRAGON),
                baron_cum = cumsum(BARON_NASHOR),
                elder_dragon_cum = cumsum(ELDER_DRAGON),
                rift_herald_cum = cumsum(RIFT_HERALD))

yy <- yy[-(3:22)]

<<<<<<< HEAD
merge<- left_join(str, yy, by=c('matchname', 'min')) %>%
  left_join(., killing, by=c('matchname', 'min')) %>% 
  left_join(.,b_tower, by = c('matchname','min'))

final <- left_join(gold, yy,by = c('matchname', 'min'))

<<<<<<< HEAD
=======
final <- left_join(gold, yy,by = c('matchname', 'min'))
>>>>>>> 61321282ced06ad2794e03f26bf434389a1bc5f7
final <- na.omit(final)
=======
## Subsets data by minute and puts into a list
final_list_minutes <- split(final, final$min)
>>>>>>> 6a894be9eebffaef3caf40e47099d95613ba9213

dim(final)
dim(gold)


##DRAFT
####change column name
##names(b_tower)[1]<-"matchname"
##names(b_tower)[2] <- 'min'
##b_tower$matchname <- gsub(".*=","",monster$matchname)

##
##




