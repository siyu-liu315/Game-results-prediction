library(tidyverse)
library(dplyr)
library(varhandle)
library(plyr)
<<<<<<< HEAD
=======
library(bigrquery)
### shorten the address to unique game ID
>>>>>>> 9435e2f0072816ab517d31e66638aa56956dc80b
structures <- read.csv("leagueoflegends/structures.csv")
structures$Address <- gsub(".*=","",structures$Address)
structures$Time <- as.integer(structures$Time) + 1
### 提出Red队只观测Blue。创建为bhome
bhome <- structures %>% filter(Team == "bInhibs" | Team =="bTowers")
### 2014-2015年使用FOUNTAIN_TURRET, 2016年开始改名为BASE_TURRENT.
unique(bhome$Type)
unique(bhome$Lane)
class(bhome$Type)
### 将所有名字替换为BASE_TURRENT
bhome$Type <- as.vector(bhome$Type)
bhome$Type <- str_replace(bhome$Type, "FOUNTAIN_TURRET", "BASE_TURRET")
### 利用下面的数据确定数据对错
tem <- structures %>% filter(Type == "NEXUS_TURRET") %>% view()

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
                 mid_nexus = ((Lane == "MID_LANE" & Type == "NEXUS_TURRET"))) -> bhome
### replace Ture or False to 0/1
bhome %>% select(-c(Address, Team, Lane, Type, Time)) -> tower
infor <- bhome %>% select(c(Address, Team, Lane, Type, Time)) 
tower <- ifelse(tower == "TRUE", 1, 0)
b_tower <- cbind(infor, tower)
### 检测是否每一分钟只推掉一座塔
rowSums(tower) %>% unique()
### 把NA去除,无效
b_tower <- na.omit(b_tower)
<<<<<<< HEAD
write_excel_csv(b_tower,file = "temp1")

### teams
teamname <- unique(b_tower$Address) %>% as.matrix()
teamname <- teamname[rep(seq_len(nrow(teamname)), each=60),]
teamname$Time <- teamname$Time

### add min_
b_tower$Time <- paste("min", b_tower$Time,sep="_")

### change the Address to matchname
colnames(b_tower)[colnames(b_tower)=="Address"] <- "matchname"

=======
View(b_tower)
>>>>>>> 4fca984d4ef452e82a1729c6841bb7495c4734db

<<<<<<< HEAD
=======
####change Time formate
b_tower$Time <- paste('min',b_tower$Time,sep = '_')
>>>>>>> 9435e2f0072816ab517d31e66638aa56956dc80b

##change column name
monster$matchname <- gsub(".*=","",monster$matchname)
names(b_tower)[1]<-"matchname"
names(b_tower)[5] <- 'min'

View(b_tower)
##left join and select
merge <- left_join(monster, b_tower, by = c('matchname', 'min'))%>% arrange(matchname)
<<<<<<< HEAD
merge <- merge[-c(1,2,14,22)]
=======
merge <- merge[c(1,2,14:22)]
>>>>>>> fa03c230c342148861503492ef291ee4dd87f3d1

###repalce Na
merge[is.na(merge)] <- 0

<<<<<<< HEAD
#######
=======
##accumulate
try <- merge %>% group_by(matchname) %>% 
  dplyr::mutate(top_outer = cumsum(top_outer),
                ) 


>>>>>>> fa03c230c342148861503492ef291ee4dd87f3d1
