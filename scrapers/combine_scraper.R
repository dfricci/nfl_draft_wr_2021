setwd("~/Documents/Football Analytics/Football Database/R Code")
library(XML)
library(RCurl)
library(dplyr)


year=seq(2011,2021,by=1)
year=as.character(year)


final_combine=data.frame()
for(zz in 1:length(year)){

  url_nfl <- paste("https://www.pro-football-reference.com/draft/",year[zz],"-combine.htm",sep="")
  tabs_nfl <- getURL(url_nfl)
  read_nfl <- readHTMLTable(tabs_nfl, stringsAsFactors = F)
  
  #Clean Data
  combine <- read_nfl$combine
  combine <- combine[!(combine$Pos %in% c("Pos")),]
  combine$College <- NULL
  combine <- combine %>%
    mutate(Ht_adj = case_when(grepl('5-4', Ht) ~ 64,
                                  grepl('5-5', Ht) ~ 65,
                                  grepl('5-6', Ht) ~ 66,
                                  grepl('5-7', Ht) ~ 67,
                                  grepl('5-8', Ht) ~ 68,
                                  grepl('5-9', Ht) ~ 69,
                                  grepl('5-10', Ht) ~ 70,
                                  grepl('5-11', Ht) ~ 71,
                                  grepl('6-0', Ht) ~ 72,
                                  grepl('6-1', Ht) ~ 73,
                                  grepl('6-2', Ht) ~ 74, 
                                  grepl('6-3', Ht) ~ 75,
                                  grepl('6-4', Ht) ~ 76,
                                  grepl('6-5', Ht) ~ 77,
                                  grepl('6-6', Ht) ~ 78,
                                  grepl('6-7', Ht) ~ 79,
                                  grepl('6-8', Ht) ~ 80,
                                  grepl('6-9', Ht) ~ 81,
                                  grepl('6-10', Ht) ~ 82,
                                  TRUE ~ 0))
  names(combine)[names(combine) == "40yd"] <- "Forty"
  names(combine)[names(combine) == "Broad Jump"] <- "Broad"
  names(combine)[names(combine) == "3Cone"] <- "Three_Cone"
  names(combine)[names(combine) == "Drafted (tm/rnd/yr)"] <- "Drafted"
  combine$Ht_adj[combine$Ht_adj == 0] <- NA 
  combine$Ht[combine$Ht == ""] <- NA   
  combine$Wt[combine$Wt == ""] <- NA
  combine$Forty[combine$Forty == ""] <- NA
  combine$Vertical[combine$Vertical == ""] <- NA
  combine$Bench[combine$Bench == ""] <- NA
  combine$Broad[combine$Broad == ""] <- NA
  combine$Three_Cone[combine$Three_Cone == ""] <- NA  
  combine$Shuttle[combine$Shuttle == ""] <- NA
  combine$Year <- year[zz]
  combine$Wt <- as.numeric(combine$Wt)
  combine$Forty <- as.numeric(combine$Forty)
  combine$Vertical <- as.numeric(combine$Vertical)
  combine$Bench <- as.numeric(combine$Bench)
  combine$Broad <- as.numeric(combine$Broad)
  combine$Three_Cone <- as.numeric(combine$Three_Cone)
  combine$Shuttle <- as.numeric(combine$Shuttle)
  combine$Year <- as.factor(combine$Year)
  

  combine <- combine[, c("Player", "Pos", "School", "Ht", "Wt", "Ht_adj", "Forty", "Vertical", "Bench", "Broad",
                          "Three_Cone", "Shuttle", "Drafted", "Year")]
  
  final_combine=rbind(final_combine,combine)

}

save(final_combine,file="~/Documents/Football Analytics/Football Database/R Code/combine.RData")




