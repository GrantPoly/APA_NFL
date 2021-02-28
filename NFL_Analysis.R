library(tidyverse)

#setwd("~/Desktop/Applied Predictive Analytics")

NFL <- read.csv("Data/NFL Play by Play 2009-2016 (v3).csv", fileEncoding = "UTF-8-BOM")

#save(NFL_Game, file = "Data/NFL.RData")

#load("Data/NFL_Game.RData")

missing_values <- sapply(NFL, function(x) sum(is.na(x)))

missing_values

count(NFL)/count(NFL)

FirstGame <- filter(NFL, GameID == 2009091000)

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

NFL <- data.frame(lapply(NFL, as.character), stringsAsFactors=FALSE)                         

NFL$Touchdown <- as.integer(NFL$Touchdown)
NFL$sp <- as.integer(NFL$sp)
NFL$PosTeamScore <- as.integer(NFL$PosTeamScore)

NFL <- mutate(NFL, 
                  HomeTeamScore = ifelse(posteam == HomeTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore),
                  AwayTeamScore = ifelse(posteam == AwayTeam, PosTeamScore + Touchdown*6 + ifelse(FieldGoalResult == "Good",3,0), DefTeamScore)
)

#Fix Team Issues

NFL[NFL==""]<-NA

NFL <- fill(data = NFL, posteam)                         

typeof(NFL$HomeTeamScore)

NFL$HomeTeamScore <- as.integer(NFL$HomeTeamScore)

NFL <- NFL %>% group_by(GameID)  %>% mutate(FinalHomeScore = max(HomeTeamScore, na.rm = T))

typeof(NFL$AwayTeamScore)

NFL$AwayTeamScore <- as.integer(NFL$AwayTeamScore)

NFL <- NFL %>% group_by(GameID)  %>% mutate(FinalAwayScore = max(AwayTeamScore, na.rm = T)) 

NFL <- mutate(NFL, HomeWin = ifelse(FinalHomeScore > FinalAwayScore, 1, 0))                        

typeof(NFL$AirYards)

NFL$AirYards <- as.integer(NFL$AirYards)

typeof(NFL$YardsAfterCatch)

NFL$YardsAfterCatch <- as.integer(NFL$YardsAfterCatch)

NFL <- mutate(NFL, HomePassYards = ifelse(posteam == HomeTeam & PassOutcome == "Complete", AirYards + YardsAfterCatch,0))

NFL <- mutate(NFL, AwayPassYards = ifelse(posteam == AwayTeam & PassOutcome == "Complete", AirYards + YardsAfterCatch,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomePass = sum(HomePassYards, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayPass = sum(AwayPassYards, na.rm=T))

NFL <- mutate(NFL, HomeRushYards = ifelse(posteam == HomeTeam & RushAttempt == 1, Yards.Gained,0))

NFL <- mutate(NFL, AwayRushYards = ifelse(posteam == AwayTeam & RushAttempt == 1, Yards.Gained,0))

NFL$HomeRushYards <- as.integer(NFL$HomeRushYards)

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomeRush = sum(HomeRushYards, na.rm = T))

NFL$AwayRushYards <- as.integer(NFL$AwayRushYards)

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayRush = sum(AwayRushYards, na.rm=T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomeYards = HomeRush+HomePass)

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayYards = AwayRush+AwayPass)

NFL <- mutate(NFL, Overtime = ifelse(qtr == 5, 1,0))



NFL$PassAttempt <- as.integer(NFL$PassAttempt)

NFL <- mutate(NFL, HomePassCount = ifelse(posteam == HomeTeam & PassOutcome == "Complete", 1,0))

NFL <- mutate(NFL, AwayPassCount = ifelse(posteam == AwayTeam & PassOutcome == "Complete", 1,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomePasses = sum(HomePassCount, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayPasses = sum(AwayPassCount, na.rm=T))

NFL <- mutate(NFL, HomePassAttemptCount = ifelse(posteam == HomeTeam & PassAttempt == 1, 1,0))

NFL <- mutate(NFL, AwayPassAttemptCount = ifelse(posteam == AwayTeam & PassAttempt == 1, 1,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomePassAttempts = sum(HomePassAttemptCount, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayPassAttempts = sum(AwayPassAttemptCount, na.rm=T))

#Add Play Counts

NFL <- mutate(NFL, HomePlayCount = ifelse(posteam == HomeTeam & PlayAttempted == 1, 1,0))

NFL <- mutate(NFL, AwayPlayCount = ifelse(posteam == AwayTeam & PlayAttempted == 1, 1,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomePlays = sum(HomePlayCount, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayPlays = sum(AwayPlayCount, na.rm=T))

#Add First Downs

NFL <- mutate(NFL, HomeFirstDownCount = ifelse(posteam == HomeTeam & FirstDown == 1, 1,0))

NFL <- mutate(NFL, AwayFirstDownCount = ifelse(posteam == AwayTeam & FirstDown == 1, 1,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomeFirstDowns = sum(HomeFirstDownCount, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayFirstDowns = sum(AwayFirstDownCount, na.rm=T))

#Add Drive Counts

NFL <- mutate(NFL, HomeDrivesCount = ifelse(posteam == HomeTeam, Drive,0))

NFL <- mutate(NFL, AwayDrivesCount = ifelse(posteam == AwayTeam, Drive,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomeDrives = length(unique(HomeDrivesCount)) - 1)

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayDrives = length(unique(AwayDrivesCount)) - 1)

NFL <- mutate(NFL, HomeRushAttemptCount = ifelse(posteam == HomeTeam & RushAttempt == 1, 1,0))

NFL <- mutate(NFL, AwayRushAttemptCount = ifelse(posteam == AwayTeam & RushAttempt == 1, 1,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomeRushAttempts = sum(HomeRushAttemptCount, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayRushAttempts = sum(AwayRushAttemptCount, na.rm=T))


#Add Possession Time

NFL$PlayTimeDiff <- as.integer(NFL$PlayTimeDiff)

NFL <- mutate(NFL, HomePossessionTime = ifelse(posteam == HomeTeam, PlayTimeDiff,0))

NFL <- mutate(NFL, AwayPossessionTime = ifelse(posteam == AwayTeam, PlayTimeDiff,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomePossession = sum(HomePossessionTime, na.rm = T))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayPossession = sum(AwayPossessionTime, na.rm = T))                         

#Add Turnovers

NFL$InterceptionThrown <- as.integer(NFL$InterceptionThrown)

NFL <- mutate(NFL, HomeTurnoverCount = ifelse(posteam == HomeTeam & (InterceptionThrown == 1 | !is.na(RecFumbTeam)), 1,0))

NFL <- mutate(NFL, AwayTurnoverCount = ifelse(posteam == AwayTeam & (InterceptionThrown == 1 | !is.na(RecFumbTeam)), 1,0))

NFL <- NFL %>% group_by(GameID)  %>% mutate(HomeTurnovers = sum(HomeTurnoverCount))

NFL <- NFL %>% group_by(GameID)  %>% mutate(AwayTurnovers = sum(AwayTurnoverCount))                         

#Test Code
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


Passing <- group_by(NFL, GameID) %>% 
  summarise(HomeAttempts = max(HomePassAttempts), AwayAttempts = max(AwayPassAttempts), HomePassTotal = 
              max(HomePasses), AwayPassTotal = max(AwayPasses), Home = getmode(HomeTeam), Away = getmode(AwayTeam),
            HomeFirsts = max(HomeFirstDowns), AwayFirsts = max(AwayFirstDowns), GameDate = getmode(Date),
            HomeTime = max(HomePossession), AwayTime = max(AwayPossession), Hometurns = max(HomeTurnovers),
            Awayturns = max(AwayTurnovers))                  

NFL_Game <- group_by(NFL, GameID) %>% 
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

NFL_Game <- relocate(NFL_Game, HomeWin, .after = last_col())

out4=glm(HomeWin~., family = binomial(link='logit'), maxit = 100, data=NFL_Game[c(5:38)])
out4$coefficients

out5=glm(HomeWin~HomePass+AwayPass+Overtime+HomeRush+AwayRush+HomePossession+AwayPossession+HomeTurnovers+AwayTurnovers+
           YardsPlayHome+YardsPlayAway, family = binomial(link='logit'), maxit = 100, data=NFL_Game)
summary(out5)

predict(out5, NFL_Game[1:10,5:38], type="response")
