library(tidyverse)
library(dplyr)
library(varhandle)
library(readr)
s
###### load the data 

kills <- read.csv(file = "kills.csv")
monsters <- read.csv(file = "monsters.csv")
structures <- read.csv(file = "structures.csv")
matchinfo <- read.csv(file = "matchinfo.csv")
gold <- read.csv(file = "gold.csv")
bans <- read.csv(file = "bans.csv")

View(kills)
#### cleaning the data 
View(lol)
View(monsters)
View(gold)
View(matchinfo)
kills$Address <- gsub(".*=","",kills$Address)
kills$Time <- as.integer(kills$Time) + 1
monsters$Address <- gsub(".*=","",monsters$Address)
monsters$Time <- as.integer(monsters$Time) + 1

############2015 SP
###### Season 2015 top 3 winrate champion at top lane
bluewin_2015_sp_top <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2015_sp_top <- matchinfo %>% filter(Year == 2015 & Season == "Spring" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))
blue_2015sp_top <- head(bluewin_2015_sp_top, 3)
red_2015sp_top <- head(redwin_2015_sp_top, 3)

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2015_sp_mid <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2015_sp_mid <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))
blue_2015sp_mid <- head(bluewin_2015_sp_mid, 3)
red_2015sp_mid <- head(redwin_2015_sp_mid, 3)


### Season 2015 top 3 winrate champion at dammage carry
bluewin_2015_sp_adc <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2015_sp_adc <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))
blue_2015sp_adc <- head(bluewin_2015_sp_adc, 3)
red_2015sp_adc <- head(redwin_2015_sp_adc, 3)

### Season 2015 top 3 winrate champion at support 
bluewin_2015_sp_sup <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2015_sp_sup <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))
blue_2015sp_sup <- head(bluewin_2015_sp_sup, 3)
red_2015sp_sup <- head(redwin_2015_sp_sup, 3)

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2015_sp_jg <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2015_sp_jg <- matchinfo %>% filter(Year == 2015 & Season == "Spring" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))
blue_2015sp_jun <- head(bluewin_2015_sp_top, 3)
red_2015sp_jun <- head(redwin_2015_sp_top, 3)


###### Season 2015 top 3 winrate champion at top lane
bluewin_2015_su_top <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2015_sp_top <- matchinfo %>% filter(Year == 2015 & Season == "Summer" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2015_su_mid <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2015_su_mid <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at dammage carry
bluewin_2015_su_adc <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2015_su_adc <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at support 
bluewin_2015_su_sup <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2015_su_sup <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2015_su_jg <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2015_su_jg <- matchinfo %>% filter(Year == 2015 & Season == "Summer" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))

###############2016
###### Season 2015 top 3 winrate champion at top lane
bluewin_2016_sp_top <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2016_sp_top <- matchinfo %>% filter(Year == 2016 & Season == "Spring" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2016_sp_mid <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2016_sp_mid <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at dammage carry
bluewin_2016_sp_adc <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2016_sp_adc <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at support 
bluewin_2016_sp_sup <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2016_sp_sup <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2016_sp_jg <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2016_sp_jg <- matchinfo %>% filter(Year == 2016 & Season == "Spring" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))


###### Season 2016 top 3 winrate champion at top lane
###### Season 2015 top 3 winrate champion at top lane
bluewin_2016_su_top <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2015_sp_top <- matchinfo %>% filter(Year == 2016 & Season == "Summer" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2016_su_mid <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2016_su_mid <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at dammage carry
bluewin_2016_su_adc <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2016_su_adc <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at support 
bluewin_2016_su_sup <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2016_su_sup <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2016_su_jg <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2016_su_jg <- matchinfo %>% filter(Year == 2016 & Season == "Summer" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))


##### season 2017 top3 winrate champion 
bluewin_2017_sp_top <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2017_sp_top <- matchinfo %>% filter(Year == 2017 & Season == "Spring" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2017_sp_mid <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2017_sp_mid <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at dammage carry
bluewin_2017_sp_adc <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2017_sp_adc <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at support 
bluewin_2017_sp_sup <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2017_sp_sup <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2017_sp_jg <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2017_sp_jg <- matchinfo %>% filter(Year == 2017 & Season == "Spring" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))


### season 2017 summer

bluewin_2016_su_top <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2015_sp_top <- matchinfo %>% filter(Year == 2017 & Season == "Summer" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2016_su_mid <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2016_su_mid <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at dammage carry
bluewin_2016_su_adc <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2016_su_adc <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at support 
bluewin_2016_su_sup <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2016_su_sup <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2016_su_jg <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2016_su_jg <- matchinfo %>% filter(Year == 2017 & Season == "Summer" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))


#### season 2018 sp
bluewin_2018_sp_top <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 1) %>% count(blueTopChamp) %>% arrange(desc(n))
redwin_2018_sp_top <- matchinfo %>% filter(Year == 2018 & Season == "Spring" & bResult == 0) %>% count(redTopChamp) %>% arrange(desc(n))

#### Season 2015 top 3 winrate champion at mid lane
bluewin_2018_sp_mid <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 1) %>% count(blueMiddleChamp) %>% arrange(desc(n))
redwin_2018_sp_mid <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 0) %>% count(redMiddleChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at dammage carry
bluewin_2018_sp_adc <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 1) %>% count(blueADCChamp) %>% arrange(desc(n))
redwin_2018_sp_adc <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 0) %>% count(redADCChamp) %>% arrange(desc(n))

### Season 2015 top 3 winrate champion at support 
bluewin_2018_sp_sup <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 1) %>% count(blueSupportChamp) %>% arrange(desc(n))
redwin_2018_sp_sup <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 0) %>% count(redSupportChamp) %>% arrange(desc(n))

### Seasib 2015 top 3 winrate champion at jungle
bluewin_2018_sp_jg <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 1) %>% count(blueJungleChamp) %>% arrange(desc(n))
redwin_2018_sp_jg <- matchinfo %>% filter(Year == 2018 & Season == "Spring" &bResult == 0) %>% count(redJungleChamp) %>% arrange(desc(n))

