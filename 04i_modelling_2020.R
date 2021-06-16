# Further modeling, after some very long pause
# this is done in May-2020, during the COVID-19 lockdown, and also during Ramadan


#  Load libraries ---------------------------------------------------------

library(tidyverse)
library(caret)
library(ROSE)
library(xgboost)
library(StatRank)
library(tictoc)
library(broom)



# Define functions --------------------------------------------------------


#' extractData
#' This function is used to extract train data & test data from a df that has both binded together
#' @param df the input dataframe
#' @param target is either 'train' or 'test'
#' 
#' @return a df
#' @export
#'
#' @examples
extractData <- function(df, target) {
  resultdf <- df %>%
    filter(dataset==target) %>%
    select(-age,-days_book_signup,-dataset,-made_booking,-destination_language,
           -lat_destination,-distance_km,-book_month,-book_season)
  #userIds <- resultdf$id
  #resultdf <- select(resultdf,-id)
  #return(list(data=resultdf, userid=userIds))
  return(resultdf)
  }

preProcessDataOLD <- function(df) {
  data <- select(df, -country_destination, -id)
  userid <- select(df, id)
  label <- select(df, country_destination)
  # preprocessing
  preProc <- preProcess(df, method=c("medianImpute","zv","center","scale"))
  data <- predict(preProc,data)
  # One hot encoding
  dummies <- dummyVars(~ ., data = data)
  data <- predict(dummies, newdata = data)
  return(list(features = data,
              label = label$country_destination,
              userid = userid,
              levels = levels(label$country_destination)))
}

preProcessData <- function(df, method = NULL, type = 'train', preProc = NULL, dummies = NULL) {
  if (type == 'train') {
    preProc <- NULL
    data <- select(df, -country_destination, -id)
    userid <- select(df, id)
    label <- select(df, country_destination)
    # preprocessing
    if (!is.null(method)) {
      preProc <- preProcess(df, method = method)
      data <- predict(preProc,data)
    }
    # One hot encoding
    dummies <- dummyVars(~ ., data = data)
    data <- predict(dummies, newdata = data, sparse = TRUE)
    return(list(features = data,
                label = label$country_destination,
                userid = userid,
                levels = levels(label$country_destination),
                dummies = dummies,
                preProc = preProc))
  }
  else {
    data <- select(df, -country_destination, -id)
    userid <- select(df, id)
    label <- select(df, country_destination)
    if (!is.null(preProc)) {
      data <- predict(preProc,data)
    }
    data <- predict(dummies, newdata = data, sparse = TRUE)
    return(list(features = data,
                label = label$country_destination,
                userid = userid))
  }
}

calcAccuracy <- function(y, X=NULL, y_preds = NULL, clf = NULL) {
  if (is.null(y_preds) & is.null(clf)) {
    print('Both clf and y_preds are NULL')
    return (NULL)
  }
  else if (! is.null(clf)) {
    y_preds <- factor(predict(clf, X) + 1, labels = data$levels)
  } 
  accuracy <- sum(y_preds==y)/length(y)
  return(accuracy)
}

calcSoftprobAcc <- function(y, X=NULL, y_preds = NULL, clf = NULL, nclass = NULL) {
  if (is.null(y_preds) & is.null(clf)) {
    print('Both clf and y_preds are NULL')
    return (NULL)
  }
  else if (! is.null(clf)) {
    y_preds <- matrix(predict(clf, X), ncol=nclass, byrow = TRUE)
  }
  orderedPreds <- apply(y_preds, 1, order, decreasing = TRUE)
  #topPreds <- factor(orderedPreds[1,], labels=data$levels)
  topPred <- orderedPreds[1,]
  accuracy <- sum(topPred == y)/length(y)
  return(accuracy)
}

nDCG <- function(Predicted, GroundTruth) {
  Relevance <- Predicted == GroundTruth
  i <- seq(1,length(Predicted))
  sum((2^Relevance-1)/log2(i+1))
}



#' Calculates the nDCG score for a group of predicted values
#' y_preds is a matrix of probabilities
#' @param y The true labels
#' @param X The X matrix. If provided, then y will be predicted
#' @param y_preds 
#' @param clf The clf to use to predict y from X
#' @param nclass The number of classes
calcNDCG<- function(y, X=NULL, y_preds = NULL, clf = NULL, nclass = NULL) {
  if (is.null(y_preds) & is.null(clf)) {
    print('Both clf and y_preds are NULL')
    return (NULL)
  }
  else if (! is.null(clf)) {
    y_preds <- matrix(predict(clf, X), ncol=nclass, byrow = TRUE)
  }
  orderedPreds <- apply(y_preds, 1, order, decreasing = TRUE)
  top5Preds <- orderedPreds[1:5,] %>%
    matrix(nrow = 5, byrow = FALSE)
  l <- split(orderedPreds, col(orderedPreds))
  scores <- mapply(nDCG, l, as.integer(y), USE.NAMES = FALSE)
  return(mean(scores))
}


printScores <- function (trainy, trainPreds, testy, testPreds, nclass) {
  # train & test accuracy
  trainAcc <- calcSoftprobAcc(y = as.numeric(trainy), y_preds=as.matrix(trainPreds), nclass = nclass)
  testAcc <- calcSoftprobAcc(y = as.numeric(testy), y_preds=as.matrix(testPreds), nclass = nclass)
  sprintf('Train accuracy:%.2f%%. Test accuracy:%.2f%%.\n', trainAcc*100, testAcc*100) %>% cat(sep = "")
  # train & test nDCG
  trainNDCG <- calcNDCG(y = as.numeric(trainy), y_preds = as.matrix(trainPreds))
  testNDCG <- calcNDCG(y = as.numeric(testy), y_preds = as.matrix(testPreds))
  sprintf('Train nDCG:%.2f%%. Test nDCG:%.2f%%.\n', trainNDCG*100, testNDCG*100) %>% cat(sep = "")
}

#' getTopPred
#' Given a vector of probabilities, this function just gets the 
#' highest probability prediction
#'
#' @param softProps 
#'  a matrix of dims nxm, where n = no. observations, and m = no. of levels
#' @param levels 
#'  the level names to use for coersion to factors
#'
#' @return
#'  returns a vector of size n, of type factor, that lists the top prediction
#' @export
#'
#' @examples
getTopPred <- function(softProps, levels) {
  orderedPreds <- apply(softProps, 1, order, decreasing = TRUE)
  topPred <- as.factor(orderedPreds[1,])
  levels(topPred) <- levels
  return(topPred)
}

# Modelling ---------------------------------------------------------------


load("allUsers.RData")


kaggleTest <- extractData(allUsersExpanded, 'test')
dataScenario01 <- extractData(allUsersExpanded, 'train')
dataScenario02 <- extractData(allUsersExpanded, 'train') %>% 
  drop_na()
dataScenario03 <- dataScenario02 %>% 
  select(-deviceCnt, -avgSecs)

data <- dataScenario03 # no other processing
data <- kaggleTrain01

# train test split
set.seed(45)
trainIndex <- createDataPartition(y=data$country_destination, p=0.8, list=FALSE)
trainData <-data[trainIndex,]
testData <-data[-trainIndex,]

# Pick a small sample from the data, for sake of fast performance
sampleIndex <- createDataPartition(y=trainData$country_destination, p=1, list=FALSE)
sampleData <- trainData[sampleIndex,]


data <- preProcessData(sampleData, method=c('zv', 'nzv', 'center', 'scale', 'pca'),
                       type='train')
test <- preProcessData(testData, type = 'test', dummies = data$dummies, preProc = data$preProc)


# Training with caret -----------------------------------------------------

# combine features & labels into a single df
dataFinal <- cbind(data.frame(data$features), label=data$label)
testFinal <- cbind(data.frame(test$features), label=test$label)

# Random search
myControl <- trainControl(method = "cv",
                          number = 5,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          search = 'random',
                          sampling = 'smote')

# adaptive resampling
myControl <- trainControl(method = "adaptive_cv",
                          number = 5,
                          repeats = 3,
                          adaptive = list(min = 2,
                                          alpha = 0.05,
                                          method = "gls",
                                          complete = TRUE),
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          search = 'random',
                          sampling = 'smote')

# training
set.seed(15)
tic()
xgboost.fit <- train(label ~ .,
                     data = dataFinal,
                     method = "xgbTree",
                     trControl = myControl,
                     verbose = TRUE,
                     nthread = 12,
                     tuneLength = 64)

toc()

xgboost.fit
saveRDS(xgboost.fit, file = 'XGBoost_NoResampling.rds')


glance(xgboost.fit)
#plot(xgboost.fit, highlight=TRUE)

# predict train & test
trainPreds = predict(xgboost.fit, dataFinal, type='prob')
testPreds = predict(xgboost.fit, testFinal, type='prob')

printScores(trainy = dataFinal$label, trainPreds =  trainPreds,
            testy = testFinal$label, testPreds = testPreds,
            nclass = nclass)

testFinalDf <- testFinal %>%
  mutate(pred = getTopPred(testPreds, levels=data$levels),
         id = testData$id)


ggplot(testFinalDf, aes(x=id, y=label, col=pred)) +
  geom_point(size = 0.7, position='jitter', alpha = 0.6) +
  scale_x_discrete(labels=NULL, breaks=NULL)


testFinalDf %>% 
  select(id, label, pred) %>%
  gather(pred, label, key = 'type', value = 'destination', factor_key = TRUE) %>%
  mutate(destination = factor(destination, levels = data$levels)) %>%
  ggplot(aes(x=id, y=destination, fill=type)) +
    geom_tile() +
    scale_x_discrete(labels=NULL, breaks=NULL)

confusionMatrix(testFinalDf$pred, testFinalDf$label)


# Manual grid search ------------------------------------------------------

hyperParams <- expand.grid(eta = c(0.6, 0.8),
                           colsample_bytree = c(0.1, 0.5),
                           max_depth = c(4, 8),
                           gamma = c(1, 10),
                           min_child_weight = c(4, 16),
                           nrounds = c(100, 200),
                           subsample = 1)





myControl <- trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE,
                         verboseIter = TRUE)

# training
set.seed(15)
tic()
xgboost.fit <- train(Label ~ .,
                    data = sampleData,
                    method = "xgbTree",
                    tuneGrid = hyperParams,
                    trControl = myControl,
                    verbose = TRUE,
                    nthread = 12,
                    metric = 'Kappa')

toc()

xgboost.fit
plot(xgboost.fit, highlight=TRUE)

# predict train & test
trainPreds = predict(xgboost.fit, sampleData[-which(names(sampleData)== 'Label')], type='prob')
testPreds = predict(xgboost.fit, testData, type='prob')

# train & test accuracy
calcSoftprobAcc(sampleData$Label, y_preds=as.matrix(trainPreds),nclass = nclass)
calcSoftprobAcc(y=testLabel, y_preds=as.matrix(testPreds), nclass=nclass)

# train & test nDCG
calcNDCG(sampleData$Label, y_preds=as.matrix(trainPreds))
calcNDCG(testLabel, y_preds=as.matrix(testPreds))

# adaptive resampling
myControl <- trainControl(method = "adaptive_cv",
                          number = 5,
                          repeats = 3,
                          adaptive = list(min = 2,
                                          alpha = 0.05,
                                          method = "gls",
                                          complete = TRUE),
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          search = 'random')

# training
set.seed(15)
tic()
xgboost.fit <- train(Label ~ .,
                     data = sampleData,
                     method = "xgbTree",
                     trControl = myControl,
                     verbose = TRUE,
                     nthread = 11,
                     tuneLength = 32)

toc()

xgboost.fit

# predict train & test
trainPreds = predict(xgboost.fit, sampleData[-which(names(sampleData)== 'Label')], type='prob')
testPreds = predict(xgboost.fit, testData, type='prob')

# train & test accuracy
calcSoftprobAcc(sampleData$Label, y_preds=as.matrix(trainPreds),nclass = nclass)
calcSoftprobAcc(y=testLabel, y_preds=as.matrix(testPreds), nclass=nclass)

# train & test nDCG
calcNDCG(sampleData$Label, y_preds=as.matrix(trainPreds))
calcNDCG(testLabel, y_preds=as.matrix(testPreds))


# Training with XGBoost Methods -------------------------------------------

# create a dMatrix
dtrain <- xgb.DMatrix(sample$X, label=as.numeric(sample$y)-1)
dtest <- xgb.DMatrix(testData, label=as.numeric(testLabel)-1)

# Using softmax objective
nclass <- 12
params <- list(max_depth=1, eta=0.3, objective="multi:softmax", num_class = nclass,
               min_child_weight=1, gamma=0, colsample_bytree=0.6, subsample=0.75)
tic()
clf1 <- xgboost(data = dtrain, params = params, nthread = 12, nrounds = 100, verbose = 1)
toc()

calcAccuracy(sample$X, sample$y, clf1)
calcAccuracy(testData, testLabel, clf1)

# Using softprob objective
params$objective <- "multi:softprob"

tic()
clf2 <- xgboost(data = dtrain, params = params, nthread = 12, nrounds = 100, verbose = 1)
toc()


trainPreds = matrix(predict(clf2, sample$X), ncol = nclass, byrow = TRUE)
testPreds = matrix(predict(clf2, testData), ncol = nclass, byrow = TRUE)

calcSoftprobAcc(sample$y, sample$X, clf = clf2, nclass = nclass)
calcSoftprobAcc(sample$y, y_preds=trainPreds)
calcSoftprobAcc(y=testLabel, X=testData, clf=clf2, nclass=nclass)

calcNDCG(sample$y, X=sample$X, clf=clf2, nclass=nclass)
calcNDCG(sample$y, y_preds=trainPreds)
calcNDCG(testLabel, X=testData, clf=clf2, nclass=nclass)
calcNDCG(testLabel, y_preds=testPreds)


importanceMatrix <- xgb.importance(model = bstDMatrix)
print(importanceMatrix)
xgb.plot.importance(importance_matrix = importanceMatrix)

xgb.plot.tree(model = bstDMatrix) # very slow & craches

# using xgb.cv
tic()
cv <- xgb.cv(data = dtrain, nrounds = 30, nthread = 10, nfold = 5, metrics = list("merror"),
             max_depth = 5, eta = 0.1, objective = "multi:softmax", num_class = 12)
toc()
print(cv)
print(cv, verbose=TRUE)
plot(cv$evaluation_log)

ggplot(cv$evaluation_log, aes(x=iter, y=test_merror_mean)) +
  geom_line()


# using xgb.train
watchlist <- list(TrainSet=dtrain, TestSet=dtest)
bst <- xgb.train(data=dtrain, params = params, nrounds=300, watchlist=watchlist, nthread=10)

preds <- predict(bst, sample$X)
accuracy <- sum(preds+1==sample$y)/length(sample$y)
print(accuracy)


calcAccuracy(testData, testLabel, bst)
