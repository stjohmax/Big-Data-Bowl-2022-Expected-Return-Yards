library(tidyverse)
library(caret)
library(glmnet)
library(xgboost)

# Removing some variables from model data
train <- read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/modelData1819.csv") %>%
  select(-c(...1, playersBehindFrontMan, meanDistToFrontMan, distToSideline, returnerAcc,
            meanOrientation, varOrientation))

test = read_csv("/Users/maxstjohn/Desktop/Projects/BDB2022/test20.csv") %>%
  select(-c(...1, playersBehindFrontMan, meanDistToFrontMan, distToSideline, returnerAcc,
            meanOrientation, varOrientation))

set.seed(11)

# Building 1000 xgBoost models
xgbList = list()
for(i in 1:1000){
  # Randomly samples training data with replacement to select training set
  sample = sample(1:length(train$gameId), length(train$gameId), replace = T)
  
  trainLoop = train[sample,]
  
  trainLoopX = trainLoop %>%
    dplyr::select(-c(gameId, playId, returnYards)) %>%
    as.matrix()
  
  trainLoopY = trainLoop %>%
    dplyr::select(returnYards) %>%
    as.matrix()
  
  return_xg <- xgboost(data = trainLoopX, 
                       label = trainLoopY, 
                       max.depth = 6, #depth of the trees
                       eta = .3, #Learning rate
                       nrounds = 100,#no. passes through the data
                       objective = "reg:squarederror",
                       verbose = 2)
  xgbList = append(xgbList, list(return_xg))
  print(i)
}

# Predict return yards for each return for all 1000 models
predList = c()
predMatrix = matrix(nrow = length(test$gameId), ncol = 1000)
testX = test %>%
  select(-c(gameId, playId, returnYards)) %>%
  as.matrix()
testY = test %>%
  select(returnYards) %>%
  as.matrix()

for(i in 1:length(test$closestDefenderDistance)){
  data = testX[i,]
  for(j in 1:1000){
    xgb = xgbList[[j]]
    pred = predict(xgb, newdata = matrix(data, ncol = 16, nrow = 1))
    predList = c(predList, pred)
  }
  predMatrix[i,] = predList
  predList = c()
  print(i)
}
