# Further modeling, after some very long pause.
# this is done in May-2020, during the COVID-19 lock down, and also during Ramadan.

# This is a more tidy version of 04i_modelling_2020.R
# It uses only caret

#  Load libraries ---------------------------------------------------------

library(tidyverse)
library(caret)
library(ROSE)
library(xgboost)
library(StatRank)
library(tictoc)


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


#' preProcessData
#' Data pre-processing. That's namely caret::dummyVars & caret::preProc with the supplied method
#' @param train_df 
#' @param test_df 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
#' @todo Currently I ignore the date_first_booking, I drop it. I might use it later and set it to
#' a fixed value in the training data
#' @info This is bad practice, as I need to provide train & test data together. It should be separated
preProcessDataBAD <- function(train_df, test_df, method = NULL) {
    # remove id & response features
    train_id <- select(train_df, id)
    test_id <- select(test_df, id)
    train_label <- select(train_df, country_destination)
    train_df1 <- train_df %>%
      select(-id, -country_destination, -date_first_booking)
    test_df1 <- test_df %>%
      select(-id, -date_first_booking)
    
    # One hot encoding
    dummies <- dummyVars(~ ., data = train_df1)
    train_df2 <- predict(dummies, newdata = train_df1, sparse = TRUE)
    test_df2 <- predict(dummies, newdata = test_df1, sparse = TRUE)
    
    # other pre-processing
    if (!is.null(method)) {
      preProc <- preProcess(train_df2, method = method)
      train_df3 <- predict(preProc, train_df2)
      test_df3 <- predict(preProc, test_df2)
    }
    else {
      preProc <- NULL
      train_df3 <- train_df2
      test_df3 <- test_df2
    }
    
    train_df3 <- cbind(data.frame(train_df3), label=train_label)
    
    return(list(train = train_df3,
                train_id = train_id,
                test = test_df3,
                test_id = test_id,
                levels = levels(train_label$country_destination),
                dummyVarsObj = dummies,
                preProcObj = preProc))
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



kagglePredict <- function(y_preds, userid, levels) {
  s1 <- bind_cols(userid, y_preds) %>%
    gather("country","Prob",2:13) %>%
    group_by(id) %>%
    arrange (id, desc(Prob)) %>%
    top_n(5,Prob) %>%
    ungroup %>%
    select(id, country)
  return(s1)
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

scenarioFile = 'KaggleTrain_PreProcSc01_WithSessions.RData'

load(scenarioFile) # load kaggle train
load(sub('Train', 'Test', scenarioFile)) #load kaggle test


# train test split
set.seed(45)
trainIndex <- createDataPartition(y=kaggleTrain01$country_destination, p=0.8, list=FALSE)
trainData <-kaggleTrain01[trainIndex,]
testData <-kaggleTrain01[-trainIndex,]

# Pick a small sample from the data, for sake of fast performance
sampleIndex <- createDataPartition(y=trainData$country_destination, p=1, list=FALSE)
sampleData <- trainData[sampleIndex,]


trainProcess <- preProcessData(sampleData,
                               method=c('zv', 'nzv', 'center', 'scale', 'pca', 'knnImpute'),
                               type='train')
save(trainProcess, file = 'trainProcess_WithSessions_allData.RData', compress = 'bzip2')

testProcess <- preProcessData(testData, type = 'test',
                              dummies = trainProcess$dummies,
                              preProc = trainProcess$preProc)
save(testProcess, file = 'testProcess_WithSessions_allData.RData', compress = 'bzip2')

KaggleTestProcess <- preProcessData(kaggleTest01, type = 'test',
                              dummies = trainProcess$dummies,
                              preProc = trainProcess$preProc)
save(KaggleTestProcess, file = 'kaggleTestProcess_WithSessions_allData.RData', compress = 'bzip2')

# Training with caret -----------------------------------------------------

# combine features & labels into a single df
trainDataFinal <- cbind(data.frame(trainProcess$features), label=trainProcess$label)
testDataFinal <- cbind(data.frame(testProcess$features), label=testProcess$label)
kaggleDataFinal <- cbind(data.frame(KaggleTestProcess$features), label=KaggleTestProcess$label)

skim(trainDataFinal)
skim(testDataFinal)

table(trainDataFinal$label)*100 / length(trainDataFinal$label)

# Random search
myControl <- trainControl(method = "cv",
                          number = 5,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          search = 'random',
                          sampling = 'smote',
                          returnResamp = 'all')

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
                     data = trainDataFinal,
                     method = "xgbTree",
                     trControl = myControl,
                     verbose = TRUE,
                     nthread = 4,
                     tuneLength = 128,
                     metric = 'Kappa')

toc()

xgboost.fit

saveRDS(xgboost.fit, file = 'XGBoost_PreProc01_allData_Acc.rds')
xgboost.fit <- readRDS('XGBoost_PreProc01_allData_Acc.rds')

# predict train & test
trainPreds = predict(xgboost.fit, trainDataFinal, type='prob')
testPreds = predict(xgboost.fit, testDataFinal, type='prob')

printScores(trainy = trainDataFinal$label, trainPreds =  trainPreds,
            testy = testDataFinal$label, testPreds = testPreds,
            nclass = nclass)


# Predict on Kaggle data
kagglePreds <- predict(xgboost.fit, kaggleDataFinal, type = 'prob')

kaggleOut <- kagglePredict(kagglePreds, KaggleTestProcess$userid)
write.csv(kaggleOut, '2020Jul_DataSc01_WithSessions02_allData.csv', quote = FALSE, row.names = FALSE)

# explore the predicted data
testDataWithPred <- testData %>%
  mutate(pred = getTopPred(testPreds, levels=trainProcess$levels))
  
testDataWithPred %>% 
  select(id, country_destination, pred) %>%
  gather(pred, country_destination, key = 'type', value = 'destination', factor_key = TRUE) %>%
  mutate(destination = factor(destination, levels = trainProcess$levels)) %>%
  ggplot(aes(x=id, y=destination, fill=type)) +
    geom_tile() +
    scale_x_discrete(labels=NULL, breaks=NULL)


ggplot(testDataWithPred, aes(x=id, y=country_destination, col=pred)) +
  geom_point(size = 0.7, position='jitter', alpha = 0.6) +
  scale_x_discrete(labels=NULL, breaks=NULL)



confusionMatrix(testDataWithPred$pred, testDataWithPred$country_destination)
  



# a redo with caret -------------------------------------------------------

# load data that is pre-processed using caret
load('trainProcess_WithSessions_allData.RData') 
load('testProcess_WithSessions_allData.RData')
load('kaggleTestProcess_WithSessions_allData.RData')

# combine features & labels into a single df
trainDataFinal <- cbind(data.frame(trainProcess$features), label=trainProcess$label)
testDataFinal <- cbind(data.frame(testProcess$features), label=testProcess$label)
kaggleDataFinal <- cbind(data.frame(KaggleTestProcess$features), label=KaggleTestProcess$label)

myControl <- trainControl(method = "cv",
                          number = 10,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          search = 'random',
                          sampling = 'smote',
                          returnResamp = 'all')

# model training
set.seed(15)
tic()
xgboost.fit <- train(label ~ .,
                     data = trainDataFinal,
                     method = "xgbTree",
                     trControl = myControl,
                     verbose = TRUE,
                     nthread = 4,
                     tuneLength = 32,
                     metric = 'Kappa')

toc()

xgboost.fit

# predict train & test
trainPreds = predict(xgboost.fit, trainDataFinal, type='prob')
testPreds = predict(xgboost.fit, testDataFinal, type='prob')

printScores(trainy = trainDataFinal$label, trainPreds =  trainPreds,
            testy = testDataFinal$label, testPreds = testPreds,
            nclass = nclass)


# model training
set.seed(15)
tic()
xgboost.fit <- train(label ~ .,
                     data = trainDataFinal,
                     method = "xgbTree",
                     nthread = 12,
                     tuneGrid = data.frame(
                       eta = 0.3,
                       max_depth = 6,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 1,
                       nrounds = 15),
                     trControl = trainControl(method = "none"))
                     
toc()

xgboost.fit

# predict train & test
trainPreds = predict(xgboost.fit, trainDataFinal, type='prob')
testPreds = predict(xgboost.fit, testDataFinal, type='prob')

printScores(trainy = trainDataFinal$label, trainPreds =  trainPreds,
            testy = testDataFinal$label, testPreds = testPreds,
            nclass = nclass)

# using pre processing inside caret
# load non-processed data
load('Scenario_01_with_sessions_train.RData')
load('Scenario_01_with_sessions_test.RData')

# model training
set.seed(15)
tic()
xgboost.fit <- train(label ~ .,
                     data = trainDataFinal,
                     method = "xgbTree",
                     nthread = 12,
                     tuneGrid = data.frame(
                       eta = 0.3,
                       max_depth = 6,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 1,
                       nrounds = 15),
                     trControl = trainControl(method = "none"))

toc()

xgboost.fit


# modelling using xgboost DMatrix -----------------------------------------

# load data that is pre-processed using caret
load('trainProcess_WithSessions_allData.RData') 
load('testProcess_WithSessions_allData.RData')
load('kaggleTestProcess_WithSessions_allData.RData')

# create a dMatrix
dtrain <- xgb.DMatrix(trainProcess$features, label=as.numeric(trainProcess$label)-1)
dtest <- xgb.DMatrix(testProcess$features, label=as.numeric(testProcess$label)-1)

# Using softmax objective
nclass <- 12
params <- list(max_depth=13,
               eta=0.01,
               objective="multi:softprob",
               num_class = nclass,
               min_child_weight=1,
               gamma=0,
               colsample_bytree=0.35,
               subsample=0.35)
tic()
clf1 <- xgboost(data = dtrain, params = params, nthread = 12, nrounds = 400, verbose = 1)
toc()

trainPreds = matrix(predict(clf1, dtrain), ncol = nclass, byrow = TRUE)
testPreds = matrix(predict(clf1, dtest), ncol = nclass, byrow = TRUE)

printScores(trainy = trainProcess$label, trainPreds =  trainPreds,
            testy = testProcess$label, testPreds = testPreds,
            nclass = nclass)



importanceMatrix <- xgb.importance(model = clf1)
print(importanceMatrix)
xgb.plot.importance(importance_matrix = importanceMatrix)

xgb.plot.tree(model = bstDMatrix) # very slow & craches

