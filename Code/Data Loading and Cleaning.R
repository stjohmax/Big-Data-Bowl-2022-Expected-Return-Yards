library(tidyverse)
library(caret)
library(glmnet)
# Loading Data
games = read_csv("/Users/maxstjohn/Desktop/Data/games.csv")
PFFScoutingData = read_csv("/Users/maxstjohn/Desktop/Data/PFFScoutingData.csv")
players = read_csv("/Users/maxstjohn/Desktop/Data/players.csv")
plays = read_csv("/Users/maxstjohn/Desktop/Data/plays.csv")
tracking2018 = read_csv("/Users/maxstjohn/Desktop/Data/tracking2018.csv")
tracking2019 = read_csv("/Users/maxstjohn/Desktop/Data/tracking2019.csv")
# tracking2020 = read_csv("/Users/maxstjohn/Desktop/Data/tracking2020.csv")

# Joining data into single tibble

# Joining plays and PFF data
plays_pff_joined = inner_join(plays,
                              PFFScoutingData)

# Joining plays, PFF, and game data
games_plays_pff_joined = inner_join(games,
                                    plays_pff_joined)

# Joining tracking data into single tibble
tracking_joined = bind_rows(tracking2018, tracking2019)

# Joining plays, PFF, and game data with tracking data
tracking_data = inner_join(games_plays_pff_joined,
                           tracking_joined
) %>%
  filter(specialTeamsPlayType == "Kickoff", specialTeamsResult == "Return", kickType == "D")

# Remove unmerged data
remove(games, games_plays_pff_joined, PFFScoutingData, plays, plays_pff_joined, tracking_joined, 
       tracking2018, tracking2019)


# Cleaning tracking data

# Reorienting plays so data has them moving left to right
tracking_data <- tracking_data %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

tracking_data <- tracking_data %>%
  mutate(o = ifelse(playDirection == "left", 360- o, o),
         dir = ifelse(playDirection == "left", 360- dir, dir))


kickoffReceivedFrameData = tracking_data %>%
  filter(event == "kick_received") %>%
  unique()
  # rename("kickReceivedFrame" = "frameId") %>%
  # inner_join(tracking_data) %>%
  # filter(kickReceivedFrame <= frameId)

remove(tracking_data)

# Finding returner data
receiveKickoffLocations = kickoffReceivedFrameData %>%
  filter(is.na(penaltyYards)) %>%
  rowwise() %>%
  group_by(gameId, playId, frameId) %>%
  summarise(returnerX = x[which(nflId == returnerId)], returnerY = y[which(nflId == returnerId)],
            returnerSpeed = s[which(nflId == returnerId)], returnerAcc = a[which(nflId == returnerId)])

# Adding team variable
tracking_data_add_kickoff_locations = inner_join(kickoffReceivedFrameData,
                                                 receiveKickoffLocations) %>%
  mutate(team = ifelse(team == "home", homeTeamAbbr, 
                       ifelse(team == "away", visitorTeamAbbr, team)),
         kickOrReturn = ifelse(possessionTeam == team , "Kick",
                               ifelse(team != "football","Return", "football")))

# Tracking data transformation for kick team
defenderData = tracking_data_add_kickoff_locations %>%
  filter(kickOrReturn == "Kick")

defenderDistances = defenderData %>%
  mutate(distToReturnerDef = (sqrt((returnerX - x)^2 + (returnerY - y)^2))) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(closestDefenderDistance = min(distToReturnerDef), avgDistanceDef = mean(distToReturnerDef),
            varDistanceDef = var(distToReturnerDef), xDef = mean(s), varSpeedDef = var(s),
            meanOrientation = mean(o), varOrientation = var(o)) %>%
  unique()

# Tracking data transformation for return team
returnTeamData = tracking_data_add_kickoff_locations %>%
  filter(kickOrReturn == "Return")

returnTeamSummary = returnTeamData %>%
  mutate(distToReturnerRet = (sqrt((returnerX - x)^2 + (returnerY - y)^2))) %>%
  filter(nflId != returnerId) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(closestTeammateDistanceRet = min(distToReturnerRet), avgDistanceRet = mean(distToReturnerRet), 
            varDistanceRet = var(distToReturnerRet), avgSpeedRet = mean(s), varSpeedRet = var(s),
            returnerX = returnerX, returnerY = returnerY, returnerSpeed = returnerSpeed,
            returnerAcc = returnerAcc) %>%
  unique()

combinedData_DefenderReturnSummary = inner_join(defenderDistances,
                                                returnTeamSummary)

remove(defenderDistances, returnTeamData, returnTeamSummary)

# Finding separation values between kick and return team
addSeparation <- function(x){
  returnData <- x %>%
    filter(kickOrReturn == "Return") %>%
    filter(nflId != returnerId) %>%
    dplyr::select(gameId, playId, frameId, nflId, returnX = x, returnY = y)
  
  kickData <- x %>%
    filter(kickOrReturn == "Kick") %>%
    dplyr::select(gameId, playId, frameId, kickId = nflId, kickX = x, kickY = y)
  
  merged <- inner_join(returnData, kickData,
                       by = c("gameId" = "gameId",
                              "playId" = "playId",
                              "frameId" = "frameId"))
  
  distanceToOppDf <- merged %>%
    mutate(distToOpponent = sqrt((returnX - kickX)^2 + (returnY - kickY)^2))
  
  return(distanceToOppDf)
  
}
separationData = addSeparation(tracking_data_add_kickoff_locations)

minSepReturn = separationData %>%
  group_by(gameId, playId, frameId, nflId) %>%
  filter(distToOpponent == min(distToOpponent)) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(meanMinSepToKickTeam = mean(distToOpponent))

minSepKick = separationData %>%
  group_by(gameId, playId, frameId, kickId) %>%
  filter(distToOpponent == min(distToOpponent)) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(meanMinSepToReturnTeam = mean(distToOpponent))

minSepMerged = inner_join(minSepReturn,
                          minSepKick)

combinedData_DefenderReturnSummary_AddSep = inner_join(combinedData_DefenderReturnSummary,
                                                       minSepMerged)

remove(separationData, minSepReturn, minSepKick, combinedData_DefenderReturnSummary)

# Finding how many kick team players have gotten past the first return team player
maxReturnX = tracking_data_add_kickoff_locations %>%
  filter(kickOrReturn == "Return") %>%
  group_by(gameId, playId, frameId) %>%
  summarise(maxX = min(x))

kickoffBehindReturnTeam = tracking_data_add_kickoff_locations %>%
  filter(kickOrReturn == "Kick") %>%
  select(gameId, playId, frameId, x) %>%
  inner_join(maxReturnX) %>%
  mutate(behindReturnTeam = ifelse(x > maxX, 1, 0),
         distanceToFrontMan = maxX - x) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(playersBehindFrontMan = sum(behindReturnTeam),
            meanDistToFrontMan = mean(distanceToFrontMan))

combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam = inner_join(combinedData_DefenderReturnSummary_AddSep,
                                                                           kickoffBehindReturnTeam)

remove(maxReturnX, kickoffBehindReturnTeam, combinedData_DefenderReturnSummary_AddSep)

# Gaps between kicking team
gapData = defenderData %>%
  select(gameId, playId, frameId, position, x, y) %>%
  filter(position != "K" & position != "P") %>%
  group_by(gameId, playId, frameId) %>%
  arrange(-y) %>%
  mutate(leadX = lead(x), leadY = lead(y))

combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam_AddGap = inner_join(combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam,
                                                                                  gapData)

remove(gapData, combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam)

# Kick and return intended directions
intendedDirections = tracking_data_add_kickoff_locations %>%
  select(gameId, playId, kickDirectionIntended, returnDirectionIntended) %>%
  unique()

intendedDirectionsDummy = intendedDirections %>%
  mutate(kickLeft = ifelse(kickDirectionIntended == "L", T, F),
         kickRight = ifelse(kickDirectionIntended == "R", T, F),
         returnLeft = ifelse(returnDirectionIntended == "L", T, F),
         returnRight = ifelse(returnDirectionIntended == "R", T, F),
         sameIntendedDirection = ifelse(kickDirectionIntended == returnDirectionIntended, T, F)) %>%
  select(-c(kickDirectionIntended, returnDirectionIntended))

combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam_AddGap_AddDirection = inner_join(
  combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam_AddGap, intendedDirectionsDummy
) %>%
  unique()
  
  
remove(intendedDirections, intendedDirectionsDummy, combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam_AddGap)

# Finding return yard results
returnYardageData = tracking_data_add_kickoff_locations %>%
  select(gameId, playId, yardlineNumber, playResult) %>%
  mutate(returnYards = 100 - (yardlineNumber + playResult)) %>%
  select(gameId, playId, returnYards) %>%
  unique()



combinedDataFinal = inner_join(combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam_AddGap_AddDirection,
                               returnYardageData)


remove(returnYardageData, combinedData_DefenderReturnSummary_AddSep_AddBehindReturnTeam_AddGap_AddDirection)

# Returner distance to sideline data
combinedDataAddSideline = combinedDataFinal %>%
  rowwise() %>%
  mutate(distToSideline = min(53.3 - returnerY, returnerY))


# Hangtime data
hangTimeData = tracking_data_add_kickoff_locations %>%
  dplyr::select(gameId, playId, hangTime) %>%
  unique()


# Selecting model data
modelData = combinedDataAddSideline %>%
  ungroup() %>%
  select(-c(frameId, position, x, y, leadX, leadY, kickLeft,
            kickRight, returnLeft, returnRight, sameIntendedDirection)) %>%
  unique() %>%
  na.omit() %>%
  inner_join(hangTimeData)

modelData = modelData %>%
  dplyr::select(gameId, playId, returnerX, returnerY, returnerSpeed, closestDefenderDistance, avgDistanceDef, meanMinSepToKickTeam, meanMinSepToReturnTeam, hangTime,
         returnYards)

