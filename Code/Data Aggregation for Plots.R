library(tidyverse)
library(ggpubr)

# DF for returner on each play
returnerIds = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/returnerIds.csv")

# Predicted return yards for each return play (1000 predictions per play)
predMatrix = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/pred20.csv") %>%
  as.matrix() %>%
  round()

# Loads 2020 data
test20 = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/test20.csv")

# Kickoff team data
kickoffTeamData = read_csv("/Users/maxstjohn/Desktop/Data/plays.csv") %>%
  select(gameId, playId, possessionTeam) %>%
  rename("kickoffTeam" = "possessionTeam")

games = read_csv("/Users/maxstjohn/Desktop/Data/games.csv") %>%
  select(gameId, homeTeamAbbr, visitorTeamAbbr)

# Finds return team names for each return
returnTeamNames = inner_join(games,
                             kickoffTeamData) %>%
  mutate(returnTeam = ifelse(homeTeamAbbr == kickoffTeam, visitorTeamAbbr, homeTeamAbbr)) %>%
  select(gameId, playId, returnTeam)

# Loads colors and logos df
colorsLogos = nflfastR::teams_colors_logos


# Data about each return
returnData = test20 %>%
  select(gameId, playId, returnerX, returnYards) %>%
  mutate(yardlineReceived = round(110 - returnerX),
         yardlineReached = round((110 - returnerX)) + returnYards,
         atLeast25 = ifelse(yardlineReached >= 25, 1, 0),
         returnedOutOfEndzone = ifelse(returnerX > 110, 1, 0)) %>%
  select(gameId, playId, yardlineReceived, yardlineReached, 
         atLeast25, returnedOutOfEndzone, returnYards)


# Finds mean and sd of each return distribution
meanList = c()
sdList = c()
for(i in 1:length(test20$gameId)){
  mean = mean(predMatrix[i,])
  sd = sd(predMatrix[i,])
  meanList = c(meanList, mean)
  sdList = c(sdList, sd)
}



# Joins DFs, calculates return yards over expectation for each play
allReturns2020Data = returnerIds %>%
  bind_cols(meanPredReturnYards = meanList, sdPredReturnYards = sdList) %>%
  inner_join(returnData) %>%
  mutate(retYOE = returnYards - meanPredReturnYards) %>%
  inner_join(returnTeamNames)

# Analyzes kickoffs from kickoff team perspective

# Summary data of each kickoff team
kickoffTeamSummary = inner_join(kickoffTeamData,
                                allReturns2020Data) %>%
  group_by(kickoffTeam) %>%
  summarise(n = n(), expectedReturnYards = sum(meanPredReturnYards), 
            actualReturnYards = sum(returnYards),
            expectedYPR = expectedReturnYards/n, actualYPR = actualReturnYards/n,
            retYOE_Total = sum(retYOE), retYOE_PerReturn = retYOE_Total/n,
            meanYardlineReceived = abs(round(mean(yardlineReceived))))

teamSummaryWithLogos = inner_join(kickoffTeamSummary,
                                  colorsLogos,
                                  by = c("kickoffTeam" = "team_abbr"))


# Creates dataframe for cum RetYOE plot
kickTeamRetYOE = inner_join(kickoffTeamData,
                            allReturns2020Data) %>%
  mutate(retYOE = round(retYOE)) %>%
  select(gameId, playId, kickoffTeam, retYOE) %>%
  group_by(kickoffTeam) %>%
  mutate(cumSumRetYOE = cumsum(retYOE), Returns = 1:n(), totalReturns = max(Returns),
         finalReturn = ifelse(Returns == totalReturns, 1, 0)) %>%
  ungroup()

kickTeamFinalReturnData = kickTeamRetYOE %>%
  filter(finalReturn == 1) %>%
  select(kickoffTeam, totalReturns, cumSumRetYOE) %>%
  rename("finalRetYOE" = "cumSumRetYOE")

kickTeamCumData = inner_join(kickTeamRetYOE,
                             kickTeamFinalReturnData) %>%
  inner_join(colorsLogos,
             by = c("kickoffTeam" = "team_abbr"))

colorValues = kickTeamCumData %>%
  select(kickoffTeam, team_color) %>%
  unique()

# Adds color values for return team analysis
allReturns2020Data = allReturns2020Data %>%
  inner_join(colorValues,
             by = c("returnTeam" = "kickoffTeam"))

# Returner summary
returnerTeamColors = allReturns2020Data %>%
  select(gameId, playId, returnTeam, returnerId) %>%
  unique() %>%
  select(-c(gameId, playId))
# Summary of player's returns (uses their nflId)
playerReturnSummary = allReturns2020Data %>%
  group_by(returnerId) %>%
  summarise(n = n(), expectedReturnYards = sum(meanPredReturnYards), actualReturnYards = sum(returnYards),
            expectedYPR = expectedReturnYards/n, actualYPR = actualReturnYards/n,
            retYOE_Total = sum(retYOE), retYOE_PerReturn = retYOE_Total/n) %>%
            # percentPast25 = round(sum(atLeast25)/n, 2)
  filter(n >= 10)


players = read_csv("/Users/maxstjohn/Desktop/Data/players.csv") %>%
  select(nflId, displayName)

# Summary of player's returns using player name
playerSummary = inner_join(players,
               playerReturnSummary,
               by = c("nflId" = "returnerId")) %>%
  arrange(-retYOE_PerReturn) %>%
  inner_join(returnerTeamColors,
             by = c("nflId" = "returnerId")) %>%
  unique() %>%
  filter(displayName != "Corey Ballentine" & returnTeam != "NYJ")
  
# Cum RetYOE for returners
continuousRetYOE = allReturns2020Data %>%
  mutate(retYOE = round(retYOE)) %>%
  select(gameId, playId, returnerId, retYOE) %>%
  group_by(returnerId) %>%
  mutate(cumSumRetYOE = cumsum(retYOE), Returns = 1:n(), totalReturns = max(Returns),
         finalReturn = ifelse(Returns == totalReturns, 1, 0)) %>%
  ungroup() 

continuousRetYOEPlayerNames = inner_join(players,
                                         continuousRetYOE,
                                         by = c("nflId" = "returnerId"))

finalReturnData = continuousRetYOEPlayerNames %>%
  filter(finalReturn == 1) %>%
  select(displayName, totalReturns, cumSumRetYOE) %>%
  rename("finalRetYOE" = "cumSumRetYOE")

returnCumData = inner_join(continuousRetYOEPlayerNames,
               finalReturnData) %>%
  inner_join(returnerTeamColors,
             by = c("nflId" = "returnerId")) %>%
  unique()



