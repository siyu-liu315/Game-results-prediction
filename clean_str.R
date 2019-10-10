right <- b_tower %>% select(-Lane, -Type, -Team)
left <- clean
left$matchname <- gsub(".*=","",left$matchname)
left$Time <- as.character(left$Time)
###rename
colnames(right)[colnames(right)=="Address"] <- "matchname"
full_join(left, right, by = NULL, copy = T)

view(left)


games <- unique(b_tower$matchname)
games <- as.data.frame(games)
games <- games[rep(seq_len(nrow(games)), each=60),]
View(games)

mins <- readxl::read_excel("~/Desktop/60mins.xlsx")
mins <- as.data.frame(mins)
mins <- do.call("rbind", replicate(7372, mins, simplify = FALSE))

mins <- mins %>% mutate(id = row_number(mins$Time))
View(mins)
games_mins <- cbind(games, mins)
games_mins <- as.data.frame(games_mins)
View(games_mins)
View(right)
games_mins <- games_mins %>% arrange(games,Time)
##change name
colnames(games_mins)[colnames(games_mins)=="games"] <- "matchname"
full_join(games_mins, right, by = c(matchname, Time), copy = T)

