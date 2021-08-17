setwd("~/Documents/Football Analytics/Football Database/R Code")
library(nflfastR)
library(RCurl)
library(XML)
library(cfbfastR)
Sys.setenv(CFBD_API_KEY = "BEsYM8cZblZaUnPGM8NIXpWszAyBqXMkS1NKFZBS6s2CZ/zJl85Fm/mkfUUXTjBb")
library(dplyr)
library(stringr)


#Draft Picks
draft_picks <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
draft_picks <- draft_picks[draft_picks$season >= 2011 & draft_picks$season <= 2020, ]
#Other Notable Datasets that we will use later
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/roster.RData")
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/combine.RData")

#Alabama 2020 stats not in the API - have to manually add
devonta_smith <- data.frame("DeVonta Smith", "WR", "Alabama", "6-1", 170, 73, 4.43, NA, NA, NA, NA, NA, NA, 2021 )
jaylen_waddle <- data.frame("Jaylen Waddle", "WR", "Alabama", "5-10", 183, 70, 4.27, 40, NA, 130, 6.67, 4.00, NA, 2021)
names(jaylen_waddle) <- c("Player", "Pos", "School", "Ht", "Wt", "Ht_adj", "Forty","Vertical",  
                          "Bench", "Broad", "Three_Cone", "Shuttle", "Drafted", "Year")  
names(devonta_smith) <- c("Player", "Pos", "School", "Ht", "Wt", "Ht_adj", "Forty","Vertical",  
                          "Bench", "Broad", "Three_Cone", "Shuttle", "Drafted", "Year")  
final_combine <- rbind(final_combine, jaylen_waddle, devonta_smith)

#Join combine and draft data
rosters <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/rosters.csv")

year=seq(2011,2020,by=1)
year=as.character(year)

final_draft=data.frame()
for(zz in 1:length(year)){
  
  url_nfl <- paste("https://www.pro-football-reference.com/years/",year[zz],"/draft.htm",sep="")
  tabs_nfl <- getURL(url_nfl)
  read_nfl <- readHTMLTable(tabs_nfl, stringsAsFactors = F)
  
  draft <- read_nfl$drafts
  draft <- draft[!(draft$Pos %in% c("Pos")),]
  draft <- draft[, -c(14:27, 29 )]
  draft$draft_year <- year[zz]
  draft$CarAV <- as.numeric(draft$CarAV)
  draft$G <- as.numeric(draft$G)
  draft$value_game <- round(draft$CarAV / draft$G, 2)
  
  final_draft=rbind(final_draft,draft)
}

#Make numerical columns numeric 
cols.num <- c("Rnd","Pick", "Age", "To", "AP1", "PB", "St", "CarAV", "DrAV", "G", "draft_year", "value_game")
final_draft[cols.num] <- sapply(final_draft[cols.num],as.numeric)

#Insert Quincy Williams into draft_picks data
quincy_williams <- data.frame(2019, "JAX", 3, 98, "WillQu01", "Quincy Williams", "", "D", "LB", "LB")      
names(quincy_williams) <- c("season", "team", "round", "pick", "pfr_id", "pfr_name", "player_id", "side", "category", "position")  
draft_picks <- rbind(draft_picks, quincy_williams)  

#Arrange dataset
draft_picks <- draft_picks %>%
  arrange(season, round, pick)

#Rearrange columns in draft data and cbind them
draft <- cbind(final_draft, draft_picks)  
draft <- draft[, c("draft_year", "team", "Rnd", "Pick", "category", "position", "Player", 
                   "College/Univ", "pfr_id", "CarAV", "DrAV", "G", "value_game")]
names(draft)[names(draft) == "College/Univ"] <- "School"

#Prepare data before joining to combine
#Change Eastern and Western school names
final_combine$Year <- as.character(final_combine$Year)
final_combine$Year <- as.numeric(final_combine$Year)
final_combine[final_combine$School == "West. Michigan", "School" ] <- "Western Michigan"
final_combine[final_combine$School == "East. Kentucky", "School" ] <- "Eastern Kentucky"
final_combine[final_combine$School == "East. Washington", "School" ] <- "Eastern Washington"
final_combine[final_combine$School == "East. Illinois", "School" ] <- "Eastern Illinois"
final_combine[final_combine$School == "East. Michigan", "School" ] <- "Eastern Michigan"
final_combine[final_combine$School == "Boston Col.", "School" ] <- "Boston College"
draft[draft$School == "East. Washington", "School" ] <- "Eastern Washington"
draft[draft$School == "West. Michigan", "School" ] <- "Western Michigan"
draft[draft$School == "East. Michigan", "School" ] <- "Eastern Michigan"
draft[draft$School == "West. Illinois", "School" ] <- "Western Illinois"
draft[draft$School == "East. Kentucky", "School" ] <- "Eastern Kentucky"
draft[draft$School == "Boston Col.", "School" ] <- "Boston College"

#sub in State for St.
final_combine$School <- gsub(' St[.]', ' State', final_combine$School)
draft$School <- gsub(' St[.]', ' State', draft$School)

#Drop Titles
draft$Player <- strsplit(draft$Player , "\\s|\\,|\\(|\\)") #Split by empty spaces, dots, commas and parenthesis
titles <- c("Jr.", "IV", "Sr.", "II",  "Jr", "Sr", "V", "III", "") #Everything you want to remove that isn't a separator above should be here
draft$Player <- sapply(draft$Player, function(st) paste(st[!(st %in% titles)], collapse=" "), USE.NAMES=FALSE)

final_combine$Player <- strsplit(final_combine$Player , "\\s|\\,|\\(|\\)") #Split by empty spaces, dots, commas and parenthesis
titles <- c("Jr.", "IV", "Sr.", "II",  "Jr", "Sr", "V", "III", "") #Everything you want to remove that isn't a separator above should be here
final_combine$Player <- sapply(final_combine$Player, function(st) paste(st[!(st %in% titles)], collapse=" "), USE.NAMES=FALSE)

draft[draft$Player == "Jonathan Baldwin", "Player"] <- "Jon Baldwin"

#Join on year, name and school
draft_combine <- final_combine %>% 
  left_join(draft, by = c("Year" = "draft_year", "Player" = "Player", "School" = "School" ))

#Make position Category
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "OLB", "LB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "WR", "WR", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "DT", "DL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "DE", "DL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "CB", "DB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "RB", "RB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "OG", "OL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "OT", "OL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "C", "OL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "ILB", "LB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "S", "DB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "TE", "TE", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "QB", "QB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "NT", "DL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "EDGE", "DL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "LB", "LB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "DB", "DB", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "OL", "OL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "DL", "DL", draft_combine$category)
draft_combine$category <- ifelse(is.na(draft_combine$category)  & draft_combine$Pos == "T", "OL", draft_combine$category)

#Make Funchess a WR instead of TE
draft_combine[draft_combine$Player == "Devin Funchess", "category"] <- "WR"

#make other School names identical for both datasets - use college data as baseline for schools (130 total)
draft_combine[draft_combine$School == "Hawaii", "School" ] <- "Hawai'i"
draft_combine[draft_combine$School == "La-Monroe", "School" ] <- "Louisiana Monroe"
draft_combine[draft_combine$School == "North Carolina State", "School" ] <- "NC State"
draft_combine[draft_combine$School == "Middle Tennessee State", "School" ] <- "Middle Tennessee"
draft_combine[draft_combine$School == "Mississippi", "School" ] <- "Ole Miss"
draft_combine[draft_combine$School == "San Jose State", "School" ] <- "San José State"
draft_combine[draft_combine$School == "Southern Miss", "School" ] <- "Southern Mississippi"
draft_combine[draft_combine$School == "Ala-Birmingham", "School" ] <- "UAB"
draft_combine[draft_combine$School == "Massachusetts", "School" ] <- "UMass"
draft_combine[draft_combine$School == "Texas-San Antonio", "School" ] <- "UT San Antonio"
draft_combine[draft_combine$School == "Texas-El Paso", "School" ] <- "UTEP"
draft_combine[draft_combine$School == "Miami (FL)", "School" ] <- "Miami"
draft_combine[draft_combine$School == "Central Florida", "School" ] <- "UCF"

##Separate 2011-20 data WRs and 2021 class for now
draft_combine_wr <- draft_combine[(draft_combine$Year <= 2020) & draft_combine$Pos == "WR", ]
wr_21_class <- draft_combine[(draft_combine$Year == 2021) & draft_combine$Pos == "WR", ]

#Drop Indexes of players that belong to different positions - Dri Archer, Niles Paul, Tony Lippett, etc.
row.names(draft_combine_wr) <- NULL
draft_combine_wr <- draft_combine_wr [-c(134, 150, 118, 29, 206, 6 ), ]

#make guys undrafted pick 300
draft_combine_wr[is.na(draft_combine_wr$Pick), "Pick"] <- 300

#Join WR 21 class draft rankings
ranks <-read.csv(file.choose(), header = TRUE)
ranks <- ranks[ranks$pos == "WR", c(1:3)]

#Make Round Projection 
ranks <- ranks %>%
  mutate(round = case_when(
      rank <= 32 ~ 1,
      rank <= 64 ~ 2,
      rank <= 102 ~ 3,
      rank <= 138 ~ 4,
      rank <= 173 ~ 5,
      rank <= 214 ~ 6,
      rank <= 254 ~ 7,
      TRUE ~ 8
  ))

wr_21_class <- wr_21_class %>%
  left_join(ranks, by = c("Player" = "player"))
wr_21_class <- wr_21_class[!is.na(wr_21_class$rank), ]
wr_21_class$rank <- ifelse(wr_21_class$rank >= 300 , 300 , wr_21_class$rank)
wr_21_class$Pick <- wr_21_class$rank
wr_21_class$Rnd <- wr_21_class$round
wr_21_class <- wr_21_class[ , c(1:24)]

#Add WR 21 Class back in 
draft_combine_wr <- rbind(draft_combine_wr, wr_21_class)

#median imputation the combine data and 2021 class Data
impute <- draft_combine_wr[ , c(5:12) ]
library(missForest)
library(mice)
set.seed(123)

impute_miss_forest <- prodNA(impute, noNA = 0.1)
summary(impute_miss_forest)
md.pattern(impute_miss_forest)

library(VIM)

aggr(impute_miss_forest, col=c('navyblue','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(impute), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))

train_imputed <- mice(impute_miss_forest[,1:8], maxit = 50, method = 'pmm', seed = 123) #replication
summary(train_imputed)

library(lattice)

xyplot(train_imputed, Forty ~ Vertical+Bench+Broad+Three_Cone+Shuttle, pch=18, cex = 1)
densityplot(train_imputed)
stripplot(train_imputed, pch=20, cex=1.2)

impute_miss_forest <- complete(train_imputed)

#Combine Imputed Data back in
draft_combine_wr_part_one <- draft_combine_wr[, 1:4]
draft_combine_wr_part_three <- draft_combine_wr[,13:24 ]
draft_combine_wr <- cbind(draft_combine_wr_part_one, impute_miss_forest, draft_combine_wr_part_three)


#Feature Engineering using the Combine Variables
#First Normalize all Combine Variables
draft_combine_wr <- draft_combine_wr %>%
  mutate(wt_norm = round((Wt - min(Wt)) / (max(Wt) - min(Wt)),3),
         ht_norm = round((Ht_adj - min(Ht_adj)) / (max(Ht_adj) - min(Ht_adj)),3), 
         forty_norm = 1 - round((Forty - min(Forty)) / (max(Forty) - min(Forty)),3), 
         vertical_norm = round((Vertical - min(Vertical)) / (max(Vertical) - min(Vertical)),3), 
         bench_norm = round((Bench - min(Bench)) / (max(Bench) - min(Bench)),3), 
         broad_norm = round((Broad - min(Broad)) / (max(Broad) - min(Broad)),3), 
         three_norm = 1- round((Three_Cone - min(Three_Cone)) / (max(Three_Cone) - min(Three_Cone)),3),
         shuttle_norm = 1- round((Shuttle - min(Shuttle)) / (max(Shuttle) - min(Shuttle)),3))


#Speed Score
draft_combine_wr$speed_score <- round((draft_combine_wr$wt_norm * .3) + (draft_combine_wr$forty_norm * .7),3)
#BMI
draft_combine_wr$BMI <- round((draft_combine_wr$Wt / draft_combine_wr$Ht_adj ^ 2) * 703, 1)
#Explosion Score
draft_combine_wr$explosion_score <- round((draft_combine_wr$broad_norm * .5) + (draft_combine_wr$vertical_norm * .5),3)
#Quickness Score
draft_combine_wr$quickness_score <- round((draft_combine_wr$three_norm *.4) + (draft_combine_wr$shuttle_norm *.4) + (draft_combine_wr$forty_norm * .2),3)    
#Strength Score
draft_combine_wr$strength_score <- draft_combine_wr$Bench / draft_combine_wr$Wt
draft_combine_wr$strength_score <- round((draft_combine_wr$strength_score - min(draft_combine_wr$strength_score)) / (max(draft_combine_wr$strength_score) - min(draft_combine_wr$strength_score)),3)
#Catch Radius
draft_combine_wr$catch_radius <- (draft_combine_wr$ht_norm * .334) + (draft_combine_wr$vertical_norm * .333) + (draft_combine_wr$vertical_norm * .333)
#Athletic Score
draft_combine_wr$athletic_score <- round(((draft_combine_wr$speed_score * .40) + (draft_combine_wr$explosion_score * .25) +
                                          (draft_combine_wr$quickness_score *.20) + (draft_combine_wr$strength_score * .05) +
                                          (draft_combine_wr$catch_radius * .10)) * 100, 1)


#Join to College Data
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/player_career_stats.RData")
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/player_final_year_stats.RData")

#Make ? positions as position of interest so that the later join works
player_career_stats <- player_career_stats[!is.na(player_career_stats$position), ]
player_career_stats[player_career_stats$position == "?", "position" ] <- "WR"

player_final_year_stats <- player_final_year_stats[!is.na(player_final_year_stats$position), ]
player_final_year_stats[player_final_year_stats$position == "?", "position" ] <- "WR"

#Change Name for Atwell
player_career_stats[player_career_stats$name == "Chatarius Atwell", "name" ] <- "Tutu Atwell"
player_final_year_stats[player_final_year_stats$name == "Chatarius Atwell", "name" ] <- "Tutu Atwell"

#Career Stats 
#Take out Jr., III, Sr
player_career_stats$name <- strsplit(player_career_stats$name , "\\s|\\,|\\(|\\)") #Split by empty spaces, dots, commas and parenthesis
titles <- c("Jr.", "IV", "Sr.", "II",  "Jr", "Sr", "V", "III", "") #Everything you want to remove that isn't a separator above should be here
player_career_stats$name <- sapply(player_career_stats$name , function(st) paste(st[!(st %in% titles)], collapse=" "), USE.NAMES=FALSE)

#Season Stats
#Take out Jr., III, Sr
player_final_year_stats$name <- strsplit(player_final_year_stats$name , "\\s|\\,|\\(|\\)") #Split by empty spaces, dots, commas and parenthesis
titles <- c("Jr.", "IV", "Sr.", "II",  "Jr", "Sr", "V", "III", "") #Everything you want to remove that isn't a separator above should be here
player_final_year_stats$name <- sapply(player_final_year_stats$name , function(st) paste(st[!(st %in% titles)], collapse=" "), USE.NAMES=FALSE)

#Give prefix to column names to separate career vs. season
colnames(player_career_stats)[5:48] <- paste("career", colnames(player_career_stats)[5:48], sep = "_")
colnames(player_final_year_stats)[6:49] <- paste("season", colnames(player_final_year_stats)[6:49], sep = "_")

#Trim to just WR data (can use passing and rushing data if doing RB/QB project)
player_career_stats <- player_career_stats[, c(1:5, 20, 24, 44:46, 48)]
player_final_year_stats <- player_final_year_stats[, c(1:6, 21, 25, 45:47, 49)]

#Join on position, school, and name
data <- draft_combine_wr %>%
  left_join(player_final_year_stats, by = c("Player" = "name", "category" = "position", "School" = "team"))

data <- data %>%
  left_join(player_career_stats, by = c("Player" = "name", "category" = "position", "School" = "team"))

#Make undrafted players 8th Round
data[is.na(data$Rnd), "Rnd" ] <- 8

#Join on conference rating, create usage stats, create production score
#Define Years for Upcoming Loop
year <- seq(2009,2020,by=1)
year <- as.numeric(year)

final_conf <- data_frame()
for(zz in 1:length(year)){ 
  conf <- cfbd_ratings_sp_conference(year[zz])
  conf <- as.data.frame(conf)
  conf$year <- year[zz]
  print(zz)
  final_conf <- rbind(final_conf, conf)
}

final_conf <- final_conf %>%
  group_by(conference) %>%
  summarise(mean_rating = mean(rating))

conferences <- as.vector(cfbd_conf_types_df$abbreviation)

final_teams=data.frame()
for(i in conferences){
  teams <- cfbd_team_info()
  final_teams <- rbind(final_teams, teams)
}

conference_rating <- final_teams %>%
  left_join(final_conf, by = c("conference")) %>%
  rename(conference_rating = mean_rating )

#remove players missing college stats (bad data or no join available)
data <- data[!is.na(data$season_norm_rec_rec_game), ]


#Adjust Jaylen Waddle - Alabama 2020 Stats did not import 
data[data$Player == "Jaylen Waddle", "season_norm_rec_rec_game"] <- 5.6
data[data$Player == "Jaylen Waddle", "season_norm_rec_yards_game"]  <- 118.2
data[data$Player == "Jaylen Waddle", "season_norm_rec_tds_game"] <- 0.8
data[data$Player == "Jaylen Waddle", "season_norm_rec_avg_game"] <- 21.1
data[data$Player == "Jaylen Waddle", "career_norm_rec_rec_game"] <- 3.6
data[data$Player == "Jaylen Waddle", "career_norm_rec_yards_game"] <- 65.0
data[data$Player == "Jaylen Waddle", "career_norm_rec_avg_game"] <- 19.0
data[data$Player == "Jaylen Waddle", "career_norm_rec_tds_game"] <- 0.5


data <- data %>%
  left_join(conference_rating, by = c("School" = "school")) %>%
  mutate(career_usage_rate = career_rec / career_rec_team_passing_att) %>%
  mutate(season_usage_rate = season_rec / season_rec_team_passing_att) %>%
  mutate(season_rec_norm = round((season_norm_rec_rec_game - min(season_norm_rec_rec_game)) / (max(season_norm_rec_rec_game) - min(season_norm_rec_rec_game)),3),
         season_rec_yds_norm = round((season_norm_rec_yards_game - min(season_norm_rec_yards_game)) / (max(season_norm_rec_yards_game) - min(season_norm_rec_yards_game)),3), 
         season_ypc_norm = round((season_norm_rec_avg_game - min(season_norm_rec_avg_game)) / (max(season_norm_rec_avg_game) - min(season_norm_rec_avg_game)),3), 
         season_tds_norm = round((season_norm_rec_tds_game - min(season_norm_rec_tds_game)) / (max(season_norm_rec_tds_game) - min(season_norm_rec_tds_game)),3), 
         career_rec_norm = round((career_norm_rec_rec_game - min(career_norm_rec_rec_game)) / (max(career_norm_rec_rec_game) - min(career_norm_rec_rec_game)),3), 
         career_rec_yds_norm = round((career_norm_rec_yards_game - min(career_norm_rec_yards_game)) / (max(career_norm_rec_yards_game) - min(career_norm_rec_yards_game)),3), 
         career_ypc_norm = round((career_norm_rec_avg_game - min(career_norm_rec_avg_game)) / (max(career_norm_rec_avg_game) - min(career_norm_rec_avg_game)),3),
         career_tds_norm = round((career_norm_rec_tds_game - min(career_norm_rec_tds_game)) / (max(career_norm_rec_tds_game) - min(career_norm_rec_tds_game)),3))

rownames(data) <- 1:nrow(data) 

#Remove dupes (player had multiple athlete IDs)
duplicated(data$Player)
data <- data[ -c(438, 424, 258),]

#Take out 2011 and 2012 WRs before we calculate production score since no complete career stats  
data <- data[(data$Year >= 2013), ]


#Production Score
data <- data %>%
  mutate(season_production_score =  round(season_rec_norm * .35 + season_rec_yds_norm * .15 + season_ypc_norm * .25 + season_tds_norm * .25, 3),
         career_production_score =  round(career_rec_norm * .35 + career_rec_yds_norm * .15 + career_ypc_norm * .25 + career_tds_norm * .25, 3))


#Adjustment for run happy teams (Georgia Tech and Air Force)
data$season_production_score <- ifelse(data$School == "Georgia Tech" | data$School == "Air Force" , data$season_production_score - .2 , data$season_production_score)
data$career_production_score <- ifelse(data$School == "Georgia Tech" | data$School == "Air Force" , data$career_production_score - .2 , data$career_production_score)


percentilerank<-function(x){
  rx<-rle(sort(x))
  smaller<-cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
  larger<-rev(cumsum(c(0, rev(rx$lengths))))[-1]
  rxpr<-smaller/(smaller+larger)
  rxpr[match(x, rx$values)]
}

#Production Score
data$season_production_score <- round(percent_rank(data$season_production_score) * 100, 1)
data$career_production_score <- round(percent_rank(data$career_production_score) * 100, 1)

#Between 80 and 20 Normalize Production (same scale as athletic Score)
data$season_production_score <- round(((data$season_production_score - min(data$season_production_score)) / (max(data$season_production_score) - min(data$season_production_score)) * (80-20)) + 20, 1)
data$career_production_score <- round(((data$career_production_score - min(data$career_production_score)) / (max(data$career_production_score) - min(data$career_production_score)) * (80-20)) + 20, 1)


#Grab columns we need for WR
data <- data[ , c("Player", "Pos", "School", "Year", "Pick", "Rnd", "speed_score", "BMI", "explosion_score",
                  "quickness_score", "strength_score", "catch_radius", "athletic_score", "season_norm_rec_rec_game",
                  "season_norm_rec_yards_game", "season_norm_rec_tds_game", "season_norm_rec_avg_game",
                  "career_norm_rec_rec_game", "career_norm_rec_yards_game", "career_norm_rec_tds_game", "career_norm_rec_avg_game",
                  "conference_rating", "career_usage_rate", "season_usage_rate", "season_production_score", "career_production_score")]

save(data,file="~/Documents/Football Analytics/Football Database/R Code/data.RData")

rm(devonta_smith)
rm(draft)
rm(draft_combine)
rm(draft_combine_wr)
rm(draft_combine_wr_part_one)
rm(draft_combine_wr_part_three)
rm(final_conf)
rm(draft_picks)
rm(final_combine)
rm(final_draft)
rm(final_roster)
rm(impute)
rm(impute_miss_forest)
rm(jaylen_waddle)
rm(player_receiving)
rm(quincy_williams)
rm(read_nfl)
rm(roster)
rm(teams)
rm(train_imputed)
rm(wr_21_class)
rm(rosters)
rm(final_teams)
rm(conf)
rm(conference_rating)
rm(combine)
gc()

