actualReturnYards = returnData$returnYards

allScores = c()
for(i in 1:length(test20$gameId)){
  data = predMatrix[i,]
  returnYards = actualReturnYards[i]
  freqList = c()
  for(j in 1:110){
    freq = length(which(data <= j))/1000
    freqList = c(freqList, freq)
  }
  scoreList = c()
  for(k in 1:110){
    prob = freqList[k]
    actual = ifelse(k - returnYards >= 0, 1, 0)
    score = (prob - actual)^2
    scoreList = c(scoreList, score)
  }
  allScores = c(allScores, sum(scoreList))
  scoreList = c()
  freqList = c()
}

CRPS = sum(allScores)/(110*714)

