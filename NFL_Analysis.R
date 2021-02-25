library(tidyverse)

setwd("~/Desktop/Applied Predictive Analytics")

NFL <- read.csv("Data/NFL Play by Play 2009-2016 (v3).csv", fileEncoding = "UTF-8-BOM")

NFL2009 <- filter(NFL, Season == 2009)

#save(NFL2009, file = "Data/NFL2009.RData")

#load("Data/NFL2009.RData")

missing_values <- sapply(NFL, function(x) sum(is.na(x)))

missing_values

NFL <- data.frame(lapply(NFL, as.character), stringsAsFactors=FALSE)

typeof(NFL$sp)
typeof(NFL$PosTeamScore)

NFL$Touchdown <- as.integer(NFL$Touchdown)
NFL$sp <- as.integer(NFL$sp)
NFL$PosTeamScore <- as.integer(NFL$PosTeamScore)

NFL <- mutate(NFL, 
                  HomeTeamScore = ifelse(posteam == HomeTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore),
                  AwayTeamScore = ifelse(posteam == AwayTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore)
)

typeof(NFL$HomeTeamScore)

NFL$HomeTeamScore <- as.integer(NFL$HomeTeamScore)

NFL <- NFL %>% group_by(GameID)  %>% mutate(FinalHomeScore = max(HomeTeamScore, na.rm = T))

typeof(NFL$AwayTeamScore)

NFL$AwayTeamScore <- as.integer(NFL$AwayTeamScore)

NFL <- NFL %>% group_by(GameID)  %>% mutate(FinalAwayScore = max(AwayTeamScore, na.rm = T))

NFL <- mutate(NFL, HomeWin = ifelse(FinalHomeScore > FinalAwayScore, 1, 0))

typeof(NFL$HomeWin)

summary(NFL)


count(NFL$HomeWin)

