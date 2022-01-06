library(tidyverse)

returnerIds = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/returnerIds.csv")

players = read_csv("/Users/maxstjohn/Desktop/Data/players.csv") %>%
  select(nflId, displayName)

predMatrixTibble = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/pred20.csv") %>%
  as_tibble() %>%
  round()

test20 = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/test20.csv")

predMatrixBinded = bind_cols(returnerId = returnerIds$returnerId,
                             predMatrix, returnYards = test20$returnYards)


returnerIdUnique = unique(playerSummary$nflId)
percentileList = c()
for(i in 1:length(returnerIdUnique)){
  returnerData = predMatrixBinded %>%
    filter(returnerId == returnerIdUnique[i])
    
  avgRetYards = mean(returnerData$returnYards)
  
  predList = returnerData %>%
    select(-c(returnerId, returnYards)) %>%
    as.matrix() %>%
    as.list() %>%
    unlist()
  
  percentile = sum(avgRetYards >= predList)/length(predList)
  percentileList = c(percentileList, percentile)
}

percentileTibble = bind_cols(returnerId = returnerIdUnique,
                             percentile = percentileList)

playerPercentiles = inner_join(players,
                               percentileTibble,
                               by = c("nflId" = "returnerId")) %>%
  rename("returnerId" = "nflId") %>%
  inner_join(returnerTeamColors) %>%
  unique() %>%
  filter(displayName != "Corey Ballentine" & returnTeam != "NYJ") %>%
  inner_join(colorValues,
             by = c("returnTeam" = "kickoffTeam")) %>%
  mutate(percentile = percentile*100)

ggplot(playerPercentiles, aes(x = reorder(displayName, percentile), y = percentile,
                              group = returnTeam)) +
  geom_bar(stat = "identity", aes(fill = team_color)) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Returner Grades", x = "Returner", y = "Grade") +
  theme_bw()
  