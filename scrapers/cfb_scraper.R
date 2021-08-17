setwd("~/Documents/Football Analytics/Football Database/R Code")
library(cfbfastR)
Sys.setenv(CFBD_API_KEY = "BEsYM8cZblZaUnPGM8NIXpWszAyBqXMkS1NKFZBS6s2CZ/zJl85Fm/mkfUUXTjBb")
library(dplyr)
library(stringr)


###########Scrape Data from CFBD API###################
#Define Years for Upcoming Loops
year <- seq(2009,2020,by=1)
year <- as.numeric(year)

#Create Conference Vector
conferences <- as.vector(cfbd_conf_types_df$abbreviation)
#Create Teams Vector 
final_teams=data.frame()
for(i in conferences){
  teams <- cfbd_team_info()
  final_teams <- rbind(final_teams, teams)
}

#Define Years for Upcoming Loops
year <- seq(2009,2020,by=1)
year <- as.numeric(year)

final_conf <- data_frame()
for(zz in 1:length(year)){ 
  conf <- cfbd_ratings_sp_conference(year[zz])
  conf <- as.data.frame(conf)
  conf$year <- year[zz]
  print(i)
  final_conf <- rbind(final_conf, conf)
}

final_conf <- final_conf %>%
  group_by(conference) %>%
  summarise(mean_rating = mean(rating))

conference_rating <- final_teams %>%
  left_join(final_conf, by = c("conference")) %>%
  rename(conference_rating = mean_rating )

teams <- as.vector(final_teams$school)
#teams <- teams[1:4]

#Build loop to grab first and last name of player from roster
final_roster = data.frame()

for(zz in 1:length(year)){ 
  for (i in 1:length(teams)){
    roster <- cfbd_team_roster(year[zz], team = teams[i])
    roster <- as.data.frame(roster)
    try(roster$year <- year[zz])
    print(i)
    final_roster <- rbind(final_roster, roster)
  }
}
save(final_roster,file="~/Documents/Football Analytics/Football Database/R Code/college_roster.RData")
#


#Grab number of games for a player for each season 
final_games = data.frame()

#Grab Regular Season
for(zz in 1:length(year)){ 
  for (i in 1:length(teams)){
    games <- cfbd_game_player_stats(year[zz], team = teams[i])
    games <- games[games$team == teams[i], ]
    games <- as.data.frame(games)
    try(games$year <- year[zz])
    try(games <- games[, c(33, 1:32)])
    print(i)
    final_games <- rbind(final_games, games)
  }
}

#Grab Postseason
final_games_post = data.frame()

for(zz in 1:length(year)){ 
  for (i in 1:length(teams)){
    games <- cfbd_game_player_stats(year[zz], season_type = "postseason", team = teams[i])
    games <- games[games$team == teams[i], ]
    games <- as.data.frame(games)
    try(games$year <- year[zz])
    try(games <- games[, c(33, 1:32) ])
    print(i)
    final_games_post <- rbind(final_games_post, games)
  }
}

player_games <- rbind(final_games, final_games_post)
save(player_games,file="~/Documents/Football Analytics/Football Database/R Code/player_game_stats.RData")
#load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/player_game_stats.RData")

