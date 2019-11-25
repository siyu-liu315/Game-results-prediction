monster <- read.csv("leagueoflegends/monsters.csv")
match_id <- unique(monster$Address)

monster <- monster %>% 
  filter(Team %in% c('bBarons','bDragons','bHeralds'))%>% 
  select(Address:Type) %>% 
  mutate(Time_int = ceiling(Time))%>% 
  mutate(Count = 1)

monster <- spread(monster, key = Type, value = Count)%>% 
  arrange(Address, Time_int)
monster <- monster[-2:-3]

names(monster)[1]<-"matchname"
names(monster)[2] <- 'min'
monster$matchname <- gsub(".*=","",monster$matchname)
monster[is.na(monster)] <- 0

count <- monster %>% dplyr::count(matchname, min)
merge <- left_join(monster, count,by = c("matchname","min"))
duplicate <- merge %>% filter(n>1)
keep <- merge %>% filter(n == 1)

agg = aggregate(duplicate[c(-1,-2)], 
                by = list(duplicate$matchname,duplicate$min),
                          FUN = sum)

names(agg)[1]<-"matchname"
names(agg)[2] <- 'min'

clean_monster <- rbind(keep, agg)

