setwd("~/Documents/Football Analytics/Football Database/CSV")
library(XML)
library(RCurl)
library(dplyr)
library(data.table)

load("~/Documents/Football Analytics/Football Database/R Code/combine.RData")


year=seq(2009,2021,by=1)
year=as.character(year)

final_roster=data.frame()
for(zz in 1:length(year)){
  
  url_nfl <- paste("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_",year[zz],".csv",sep="")
  roster <- read.csv(url(url_nfl))
  
  #Clean Data
  roster <- roster[ , c("season", "team", "position", "jersey_number", "status",
                        "full_name", "first_name", "last_name", "birth_date", "height",
                        "weight", "college", "high_school", "gsis_id", "espn_id", "sportradar_id",
                        "yahoo_id", "rotowire_id", "pff_id")]
  
  final_roster=rbind(final_roster,roster)
  
}

save(final_roster,file="~/Documents/Football Analytics/Football Database/R Code/roster.RData")

  