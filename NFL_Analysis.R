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

NFL2009 <- data.frame(lapply(NFL2009, as.character), stringsAsFactors=FALSE)                         
                         
NFL2009$Touchdown <- as.integer(NFL2009$Touchdown)
NFL2009$sp <- as.integer(NFL2009$sp)
NFL2009$PosTeamScore <- as.integer(NFL2009$PosTeamScore)

NFL2009 <- mutate(NFL2009, 
                    HomeTeamScore = ifelse(posteam == HomeTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore),
                    AwayTeamScore = ifelse(posteam == AwayTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore)
)

#Fix Team Issues

NFL2009[NFL2009==""]<-NA

NFL2009 <- fill(data = NFL2009, posteam)                         
                         
typeof(NFL2009$HomeTeamScore)

NFL2009$HomeTeamScore <- as.integer(NFL2009$HomeTeamScore)

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(FinalHomeScore = max(HomeTeamScore, na.rm = T))

typeof(NFL2009$AwayTeamScore)

NFL2009$AwayTeamScore <- as.integer(NFL2009$AwayTeamScore)

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(FinalAwayScore = max(AwayTeamScore, na.rm = T)) 
                         
NFL2009 <- mutate(NFL2009, HomeWin = ifelse(FinalHomeScore > FinalAwayScore, 1, 0))                        
                         
typeof(NFL2009$AirYards)

NFL2009$AirYards <- as.integer(NFL2009$AirYards)

typeof(NFL2009$YardsAfterCatch)

NFL2009$YardsAfterCatch <- as.integer(NFL2009$YardsAfterCatch)

NFL2009 <- mutate(NFL2009, HomePassYards = ifelse(posteam == HomeTeam & PassOutcome == "Complete", AirYards + YardsAfterCatch,0))

NFL2009 <- mutate(NFL2009, AwayPassYards = ifelse(posteam == AwayTeam & PassOutcome == "Complete", AirYards + YardsAfterCatch,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomePass = sum(HomePassYards, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayPass = sum(AwayPassYards, na.rm=T))

NFL2009 <- mutate(NFL2009, HomeRushYards = ifelse(posteam == HomeTeam & RushAttempt == 1, Yards.Gained,0))

NFL2009 <- mutate(NFL2009, AwayRushYards = ifelse(posteam == AwayTeam & RushAttempt == 1, Yards.Gained,0))

NFL2009$HomeRushYards <- as.integer(NFL2009$HomeRushYards)

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomeRush = sum(HomeRushYards, na.rm = T))

NFL2009$AwayRushYards <- as.integer(NFL2009$AwayRushYards)

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayRush = sum(AwayRushYards, na.rm=T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomeYards = HomeRush+HomePass)

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayYards = AwayRush+AwayPass)
                         
NFL2009 <- mutate(NFL2009, Overtime = ifelse(qtr == 5, 1,0))
                         
                         
                         
NFL2009$PassAttempt <- as.integer(NFL2009$PassAttempt)

NFL2009 <- mutate(NFL2009, HomePassCount = ifelse(posteam == HomeTeam & PassOutcome == "Complete", 1,0))

NFL2009 <- mutate(NFL2009, AwayPassCount = ifelse(posteam == AwayTeam & PassOutcome == "Complete", 1,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomePasses = sum(HomePassCount, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayPasses = sum(AwayPassCount, na.rm=T))

NFL2009 <- mutate(NFL2009, HomePassAttemptCount = ifelse(posteam == HomeTeam & PassAttempt == 1, 1,0))

NFL2009 <- mutate(NFL2009, AwayPassAttemptCount = ifelse(posteam == AwayTeam & PassAttempt == 1, 1,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomePassAttempts = sum(HomePassAttemptCount, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayPassAttempts = sum(AwayPassAttemptCount, na.rm=T))

#Add Play Counts

NFL2009 <- mutate(NFL2009, HomePlayCount = ifelse(posteam == HomeTeam & PlayAttempted == 1, 1,0))

NFL2009 <- mutate(NFL2009, AwayPlayCount = ifelse(posteam == AwayTeam & PlayAttempted == 1, 1,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomePlays = sum(HomePlayCount, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayPlays = sum(AwayPlayCount, na.rm=T))

#Add First Downs

NFL2009 <- mutate(NFL2009, HomeFirstDownCount = ifelse(posteam == HomeTeam & FirstDown == 1, 1,0))

NFL2009 <- mutate(NFL2009, AwayFirstDownCount = ifelse(posteam == AwayTeam & FirstDown == 1, 1,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomeFirstDowns = sum(HomeFirstDownCount, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayFirstDowns = sum(AwayFirstDownCount, na.rm=T))

#Add Drive Counts

NFL2009 <- mutate(NFL2009, HomeDrivesCount = ifelse(posteam == HomeTeam, Drive,0))

NFL2009 <- mutate(NFL2009, AwayDrivesCount = ifelse(posteam == AwayTeam, Drive,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomeDrives = length(unique(HomeDrivesCount)) - 1)

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayDrives = length(unique(AwayDrivesCount)) - 1)
                         
NFL2009 <- mutate(NFL2009, HomeRushAttemptCount = ifelse(posteam == HomeTeam & RushAttempt == 1, 1,0))

NFL2009 <- mutate(NFL2009, AwayRushAttemptCount = ifelse(posteam == AwayTeam & RushAttempt == 1, 1,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomeRushAttempts = sum(HomeRushAttemptCount, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayRushAttempts = sum(AwayRushAttemptCount, na.rm=T))
                         
                         
#Add Possession Time

NFL2009$PlayTimeDiff <- as.integer(NFL2009$PlayTimeDiff)

NFL2009 <- mutate(NFL2009, HomePossessionTime = ifelse(posteam == HomeTeam, PlayTimeDiff,0))

NFL2009 <- mutate(NFL2009, AwayPossessionTime = ifelse(posteam == AwayTeam, PlayTimeDiff,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomePossession = sum(HomePossessionTime, na.rm = T))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayPossession = sum(AwayPossessionTime, na.rm = T))                         

#Add Turnovers

NFL2009$InterceptionThrown <- as.integer(NFL2009$InterceptionThrown)

NFL2009 <- mutate(NFL2009, HomeTurnoverCount = ifelse(posteam == HomeTeam & (InterceptionThrown == 1 | !is.na(RecFumbTeam)), 1,0))

NFL2009 <- mutate(NFL2009, AwayTurnoverCount = ifelse(posteam == AwayTeam & (InterceptionThrown == 1 | !is.na(RecFumbTeam)), 1,0))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(HomeTurnovers = sum(HomeTurnoverCount))

NFL2009 <- NFL2009 %>% group_by(GameID)  %>% mutate(AwayTurnovers = sum(AwayTurnoverCount))                         
                         
#Test Code
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


Passing <- group_by(NFL2009, GameID) %>% 
  summarise(HomeAttempts = max(HomePassAttempts), AwayAttempts = max(AwayPassAttempts), HomePassTotal = 
              max(HomePasses), AwayPassTotal = max(AwayPasses), Home = getmode(HomeTeam), Away = getmode(AwayTeam),
              HomeFirsts = max(HomeFirstDowns), AwayFirsts = max(AwayFirstDowns), GameDate = getmode(Date),
              HomeTime = max(HomePossession), AwayTime = max(AwayPossession), Hometurns = max(HomeTurnovers),
              Awayturns = max(AwayTurnovers))                  

NFL_Game <- group_by(NFL2009, GameID) %>% 
  summarise( GameDate = getmode(Date), Home = getmode(HomeTeam), Away = getmode(AwayTeam), HomePlays = max(HomePlays), 
             AwayPlays = max(AwayPlays), HomeFirsts = max(HomeFirstDowns), AwayFirsts = max(AwayFirstDowns),
             Overtime = max(Overtime), HomeDrives = max(HomeDrives), AwayDrives = max(AwayDrives), 
             HomeScore = max(FinalHomeScore), AwayScore = max(FinalAwayScore), HomeWin = max(HomeWin),
             HomePass = max(HomePass), AwayPass = max(AwayPass),  HomePassTotal = max(HomePasses), 
             AwayPassTotal = max(AwayPasses),HomePassAttempts = max(HomePassAttempts), AwayPassAttempts = max(AwayPassAttempts),
             HomeRush = max(HomeRush), AwayRush = max(AwayRush), HomeRushAttempts = max(HomeRushAttempts),
             AwayRushAttempts = max(AwayRushAttempts), HomeYards = max(HomeYards), AwayYards = max(AwayYards),
             HomePossession = (max(HomePossession)/60), AwayPossession = (max(AwayPossession)/60),
             HomeTurnovers = max(HomeTurnovers), AwayTurnovers = max(AwayTurnovers)
            )       

NFL_Game <- mutate(NFL_Game, HomePassPercent = (HomePassTotal/HomePassAttempts)*100, AwayPassPercent 
                   = (AwayPassTotal/AwayPassAttempts)*100, HomeYardsPerPass = HomePass/HomePassAttempts, 
                   AwayYardsPerPass = AwayPass/AwayPassAttempts, HomeYardsPerRush = HomeRush/HomeRushAttempts,
                   AwayYardsPerRush = AwayRush/AwayRushAttempts, YardsPlayHome = HomeYards/HomePlays, 
                   YardsPlayAway = AwayYards/AwayPlays)                         
                         
                         
                         
