
#  Load libraries ---------------------------------------------------------

library(tidyverse)
library(caret)
library(tictoc)


# Functions ---------------------------------------------------------------

preProcessData <- function(df, method = NULL) {
  data <- select(df, -country_destination)
  #userid <- select(df, id)
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
              #userid = userid,
              levels = levels(label$country_destination)))
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
  topPreds <- factor(orderedPreds[1,], labels=data$levels)
  accuracy <- sum(topPreds == y)/length(y)
  return(accuracy)
}

nDCG <- function(Predicted, GroundTruth) {
  Relevance <- Predicted == GroundTruth
  i <- seq(1,length(Predicted))
  sum((2^Relevance-1)/log2(i+1))
}


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

# Load data ---------------------------------------------------------------

load('Scenario_01_train.RData')
load('Scenario_01_test.RData')

data0 <- kaggleTrain$data

dummies <- dummyVars(~ gender, data = data0)
data0a <- predict(dummies, newdata = data0)

data1 <- data0 %>% filter_all(any_vars(!is.na(.)))

preProc <- preProcess(data0, method = 'medianImpute')
data2 <- predict(preProc, data1)
