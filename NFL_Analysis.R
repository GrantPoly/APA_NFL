library(tidyverse)

setwd("~/Desktop/Applied Predictive Analytics")

NFL <- read.csv("Data/NFL Play by Play 2009-2016 (v3).csv", fileEncoding = "UTF-8-BOM")

NFL2009 <- filter(NFL, Season == 2009)

#save(NFL2009, file = "Data/NFL2009.RData")

#load("Data/NFL2009.RData")

missing_values <- sapply(NFL, function(x) sum(is.na(x)))

missing_values

count(NFL2009)/count(NFL)

FirstGame <- filter(NFL2009, GameID == 2009091000)

FirstGame <- data.frame(lapply(FirstGame, as.character), stringsAsFactors=FALSE)

typeof(FirstGame$sp)
typeof(FirstGame$PosTeamScore)

FirstGame$Touchdown <- as.integer(FirstGame$Touchdown)
FirstGame$sp <- as.integer(FirstGame$sp)
FirstGame$PosTeamScore <- as.integer(FirstGame$PosTeamScore)

FirstGame <- mutate(FirstGame, 
                    HomeTeamScore = ifelse(posteam == HomeTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore),
                    AwayTeamScore = ifelse(posteam == AwayTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore)
                    )
