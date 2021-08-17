setwd("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/")
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/college_roster.RData")
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/player_game_stats.RData")
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/team_game_stats.RData")
library(stringr)
library(dplyr)

##############Clean Data#####################
#Left join position, first and last name on player data by game by season
final_roster <- final_roster[,c("position", "athlete_id", "year", "first_name", "last_name") ]

player_games <- player_games %>%
  left_join(final_roster, by = c("athlete_id", "year"))


#Grab Team Pass attempts and Rushing Attempts 
team_columns <- final_games_team[, c("year", "school", "game_id", "net_passing_yards", "yards_per_pass", "rushing_attempts") ]
team_columns$net_passing_yards <- as.numeric(team_columns$net_passing_yards)
team_columns$yards_per_pass <- as.numeric(team_columns$yards_per_pass)
team_columns$rushing_attempts <- as.numeric(team_columns$rushing_attempts)
#Create team attempts column using variables we have 
team_columns <- team_columns %>%
  mutate(team_passing_att = round(net_passing_yards / yards_per_pass))
names(team_columns)[names(team_columns) == "rushing_attempts"] <- "team_rushing_car"
team_columns <- team_columns[ , c("year", "school", "game_id", "team_passing_att", "team_rushing_car") ]

#Summarize - Count Distinct Game ID group by Player, Year, etc.
games <- player_games %>%
  dplyr::select("year", "name", "game_id", "athlete_id", "position", "team") %>%
  group_by (year, name, athlete_id, position, team) %>%
  summarize(games = n_distinct(game_id))

#Join team column data on (will need this when we neutralize stats later)
player_games <- player_games %>%
  left_join(team_columns, by = c("year" = "year", "team" = "school", "game_id" = "game_id"))

#De-string completions and attempts in final_games
player_games$passing_att <- as.numeric(str_extract(player_games$c_att, '\\b\\w+$'))
player_games$passing_comp <- as.numeric(str_extract(player_games$c_att, "[^/]+"))

#Aggregate stats by game all on to one line rather than multiple categories - 1 record for each player and game
#Clean Data - Organize columns
player_games <- player_games[, -c(4:6, 10, 20, 22)] 
player_games <- player_games[, c(1:6, 29:30, 28, 34, 33, 7:27, 31, 32 )]

#make all stat columns numeric in player_games
player_games[ , 10:34] <- sapply(player_games[ , 10:34], as.numeric)


#Break down by stat categories - passing, rushing, receiving
#Passing
player_passing <- player_games %>%
  filter(category == "passing") %>%
  rename(passing_yds = yds,
         passing_tds = td,
         ypa = avg)
player_passing <- player_passing[, -c(2, 4, 16:32)]
#Sum each category for season stats
sum_NA <- function(x) {if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)}
player_passing <- player_passing %>%
  group_by(year, team, athlete_id, name, first_name, last_name, position) %>%
  summarise_all(funs(sum_NA)) %>%
  mutate(ypa= round(passing_yds / passing_att, 1)) %>%
  rename(pass_team_passing_att = team_passing_att,
         pass_team_rushing_att = team_rushing_car)

#Rushing
player_rushing <- player_games %>%
  filter(category == "rushing") %>%
  rename(rushing_yds = yds,
         rushing_tds = td,
         ypc = avg)
player_rushing <- player_rushing[, c(1, 3, 5:6, 9, 17, 12:14, 33:34)]
#Sum each category for season stats
sum_NA <- function(x) {if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)}
player_rushing <- player_rushing %>%
  group_by(year, team, athlete_id, name, position) %>%
  summarise_all(funs(sum_NA)) %>%
  mutate(ypc= round(rushing_yds / car, 1)) %>%
  rename(rush_team_passing_att = team_passing_att,
         rush_team_rushing_att = team_rushing_car)

#Receiving
player_receiving <- player_games %>%
  filter(category == "receiving") %>%
  rename(receiving_yds = yds,
         receiving_tds = td,
         ypr = avg)
player_receiving <- player_receiving[, c(1, 3, 5:6, 9, 19, 12:14, 33:34)]
#Sum each category for season stats
sum_NA <- function(x) {if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)}
player_receiving <- player_receiving %>%
  group_by(year, team, athlete_id, name, position) %>%
  summarise_all(funs(sum_NA)) %>%
  mutate(ypr= round(receiving_yds / rec, 1)) %>%
  rename(rec_team_passing_att = team_passing_att,
         rec_team_rushing_att = team_rushing_car)



#join on each player to get all records in a row
player_season_stats <- player_passing %>%
  full_join(player_rushing, by = c("year", "team", "athlete_id", "name", "position")) %>%
  full_join(player_receiving, by = c("year", "team", "athlete_id", "name", "position"))


#Join on Games Played
player_season_stats <- player_season_stats %>%
  left_join(dplyr::select(games, athlete_id, year, games), by = c("athlete_id", "year"))

player_season_stats <- player_season_stats[, c(1:7, 39, 8:36)]
player_season_stats <- player_season_stats %>%
  rename(name = name.x,
         position = position.x)
player_season_stats$games <- as.numeric(player_season_stats$games)

#Drop first and last name - will have to split them apart later (some rosters do not have first and last name so when grouping by the sum of totals for career stats it is messed up)
player_season_stats <- player_season_stats[ , -c(5,6)]

##Split into career vs. final season stats

#Career Stats
#Aggregate season stats to total career stats / adjust stats that were averaged instead of summed
player_career_stats <- player_season_stats 
player_career_stats$year <- NULL
player_career_stats <- player_career_stats %>%
  group_by(team, athlete_id, name, position) %>%
  summarise_all(funs(sum_NA)) %>%
  mutate(ypa = round(passing_yds / passing_att, 1),
         ypc= round(rushing_yds / car,1),
         ypr = round(receiving_yds / rec,1))


#Grab data frame of player's final season 
player_season_stats_test <- player_season_stats
player_season_stats_test$id <- 1:nrow(player_season_stats_test)
player_season_stats_test <- player_season_stats_test[order(player_season_stats_test$name),]
player_season_stats_test$order <- unlist(with(player_season_stats_test, tapply(year, name, function(x) rank(-x, 
                                                                                                            ties.method = "first"))))
player_final_year_stats <-player_season_stats_test[player_season_stats_test$order == 1, ]
player_final_year_stats <- player_final_year_stats[, c(-36:-37)]

##Build Neutralization Function for Final Year Stats
neutralize <- function(df) {
  passing_vectors <- cbind(df$passing_comp, df$passing_att, df$passing_yds, df$passing_tds, df$int.x)
  passing_vectors <- as.data.frame(passing_vectors)
  rushing_vectors <- cbind(df$car, df$rushing_yds, df$rushing_tds)
  rushing_vectors <- as.data.frame(rushing_vectors)
  receiving_vectors <- cbind(df$rec, df$receiving_yds, df$receiving_tds)
  receiving_vectors <- as.data.frame(receiving_vectors)
  final_passing_norms <- list()
  final_rushing_norms <- list()
  final_receiving_norms <- list()
  
  for (i in 1:length(passing_vectors)) {
    df$colName <- with(df,
                       round((passing_vectors[i]/ df$games)    
                             * (30 / (df$pass_team_passing_att / df$games)), 1)                      
    )
    #print(df$colName)
    final_passing_norms[[i]] <- df$colName
  }
  
  for (i in 1:length(rushing_vectors)) {
    df$colName <- with(df,
                       round((rushing_vectors[i]/ df$games)    
                             * (35 / (df$rush_team_rushing_att / df$games)), 1)                      
    )
    #print(df$colName)
    final_rushing_norms[[i]] <- df$colName
  }
  
  for (i in 1:length(receiving_vectors)) {
    df$colName <- with(df,
                       round((receiving_vectors[i]/ df$games)    
                             * (30 / (df$rec_team_passing_att / df$games)), 1)                      
    )
    #print(df$colName)
    final_receiving_norms[[i]] <- df$colName
  }
  final_norms <- c(final_passing_norms, final_rushing_norms, final_receiving_norms)
  final_norms <- as.data.frame(final_norms)
  names(final_norms)[1:11] <- c("norm_pass_comp_game", "norm_pass_att_game", "norm_pass_yards_game",
                                "norm_pass_tds_game", "norm_ints_game", "norm_rush_car_game", "norm_rush_yards_game",
                                "norm_rush_tds_game", "norm_rec_rec_game", "norm_rec_yards_game", "norm_rec_tds_game"
  )
  final_norms$norm_rush_avg_game <- round(final_norms$norm_rush_yards_game / final_norms$norm_rush_car_game, 1)
  final_norms$norm_rec_avg_game <- round(final_norms$norm_rec_yards_game / final_norms$norm_rec_rec_game, 1)
  df <- cbind(df, final_norms)
  return(df)
}

#Neutralize Final Year Stats
player_final_year_stats <- neutralize(player_final_year_stats)

#Neutralize Career Stats
player_career_stats <- neutralize(player_career_stats)


#Save season stats and career stats data frame
save(player_final_year_stats,file="~/Documents/Football Analytics/Football Database/R Code/player_final_year_stats.RData")
save(player_career_stats,file="~/Documents/Football Analytics/Football Database/R Code/player_career_stats.RData")
#load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/player_career_stats.RData")
#load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/player_final_year_stats.RData")
rm(player_games)
rm(player_interceptions)
rm(player_passing)
rm(player_rushing)
rm(player_season_stats_test)
rm(player_season_stats)
rm(player_defensive)
rm(games)
rm(team_columns)
rm(final_games_team)








