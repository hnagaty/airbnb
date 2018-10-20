# AdaBoost

library(caret)
#library(naivebayes)
library(doParallel)
#library(ranger)
library(adabag)


registerDoParallel(cores=4) #multi-core like functionality

# select train data & remove irrelvant features
trainData <- allUsers %>%
  filter(dataset=="train") %>%
  select(-id,-age,-made_booking,-days_book_signup,-dataset,-book_month,-book_season)
summary(trainData)

# train test split
inTrain <- createDataPartition(y=trainData$country_destination,p=0.8,list=FALSE)
trainSplit <-trainData[inTrain,]
testSplit <- trainData[-inTrain,]
testLabels <- testSplit$country_destination
testSplit <- select(testSplit,-country_destination)

myControl <- trainControl(method = "cv", number = 5,
                          classProbs = TRUE,
                          sampling = "down", # imbalanced classes
                          verboseIter = TRUE)

model <- train(country_destination~., data=trainSplit,
               method = "AdaBoost.M1",
               trControl = myControl,
               metric = "Kappa",
               preProcess = c("zv","nzv"))

model
plot(model)
predictions.nb <- predict(model,testData,type="prob")
confusionMatrix(predictions.nb,testLabels)


# Variable importance
sort(importance(ranger.fit),decreasing=TRUE) # ranger function
varImp(model) # caret function
