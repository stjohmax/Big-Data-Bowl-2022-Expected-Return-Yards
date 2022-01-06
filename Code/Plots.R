library(reshape2)
library(ggrepel)

# Actual vs Predicted return yards per return (players)
ggplot(playerSummary, aes(x = expectedYPR, y = actualYPR)) +
  geom_segment(aes(x = 24, y = 24, xend = 29, yend = 29), linetype = "dashed") +
  geom_vline(xintercept = mean(playerSummary$expectedYPR), linetype = "dashed") +
  geom_hline(yintercept = mean(playerSummary$actualYPR), linetype = "dashed") +
  geom_point(aes(size = n, color = returnTeam)) +
  scale_color_manual(breaks = colorValues$kickoffTeam, values = colorValues$team_color) +
  geom_text_repel(aes(label = displayName),
                  box.padding = .6) +
  labs(title = "Expected vs Actual Yards per Return",
       x = "Expected Yards per Return", y = "Actual Yards per Return") +
  theme_bw() +
  theme(legend.position = "none")


library(ggimage)
# Actual vs Predicted return yards per return (teams) (https://jthomasmock.github.io/nfl_plotting_cookbook/#Asign_the_colors)
ggplot(teamSummaryWithLogos, aes(x = expectedYPR, y = actualYPR)) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.04, by = "width", asp = 1.618) +
  geom_segment(aes(x = 24, y = 24, xend = 29, yend = 29), linetype = "dashed") +
  geom_vline(xintercept = mean(teamSummaryWithLogos$expectedYPR), linetype = "dashed") +
  geom_hline(yintercept = mean(teamSummaryWithLogos$actualYPR), linetype = "dashed") +
  labs(title = "Expected vs Actual Yards per Return (Kicking Team Perspective)",
       x = "Expected Yards per Return", y = "Actual Yards per Return") +
  theme_bw()

# Cumulative return yards by returner
ggplot(returnCumData, aes(x = Returns, y = cumSumRetYOE, group = displayName,
                                        color = returnTeam
                                        )) +
  geom_line() +
  geom_label(aes(x = totalReturns, y = finalRetYOE, 
                 label = ifelse((totalReturns >= 15 | abs(finalRetYOE) >=50) & 
                                  displayName != "Danny Johnson", 
                                displayName, NA))) +
  scale_color_manual(breaks = colorValues$kickoffTeam, values = colorValues$team_color) +
  ylim(-125, 125) +
  labs(title = "Cumulative Return Yards Over Expected vs Return Number",
       x = "Return Number", y = "Cumulative Return Yards Over Expected") +
  scale_x_continuous(limits = c(1, 33)) +
  theme_bw() +
  theme(legend.position = "none")
  


x + geom_point(aes(x = finalReturnData$totalReturns, y = finalReturnData$cumSumRetYOE))

# Cumulative return yards by kickoff team
ggplot(kickTeamCumData, aes(x = Returns, y = cumSumRetYOE, group = kickoffTeam
)) +
  geom_line(aes(color = kickoffTeam)) +
  geom_image(aes(x = totalReturns, y = finalRetYOE,
                 image = team_logo_wikipedia), size = 0.04, by = "width", asp = 1.618) +
  scale_color_manual(breaks = colorValues$kickoffTeam, values = colorValues$team_color) +
  ylim(-150, 150) +
  xlim(0, 40) + 
  labs(title = "Cumulative Return Yards Over Expected vs Return Number",
       x = "Return Number", y = "Cumulative Return Yards Over Expected") +
  theme_bw() +
  theme(legend.position = "none")

# Function returns plots of distribution of each return a player has
returnDistPlotArrange = function(returnerId){
  returnIndices = which(allReturns2020Data$returnerId == returnerId)
  
  dists = predMatrix[returnIndices,]
  returnYards = returnData$returnYards[returnIndices]
  
  
  plotList = list()
  for(i in 1:length(returnIndices)){
    data = tibble(predRetYards = dists[i,])
    plot = ggplot(data, aes(predRetYards)) +
      geom_density() +
      geom_vline(xintercept = returnYards[i], color = "red") +
      geom_vline(xintercept = mean(data$predRetYards), color = "blue",
                 linetype = "dashed") +
      theme_bw()
    
    plotList = append(plotList, list(plot))
    
  }
  
  return(ggarrange(plotlist = plotList, labels = "Hi"))
}

mickens = returnDistPlotArrange(44029)
mickensDists = annotate_figure(mickens, top = text_grob("Jaydon Mickens Return Yard Distributions",
                                                        face = "bold", size = 20)) 






