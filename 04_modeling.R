library(caret)
library(ranger)
library(glmnet)
library(xgboost)
library(doParallel)

#cl <- makeCluster(7)
registerDoParallel(cores=4)

#objective=multi:softprob

# xgboost for destination country
# doens't use all cores by default

# With xgboost, I should
# Dummification of categorical values
# Convert target classes to numerical values




myData <- trainUsers %>%
  select(-id,-age,-made_booking,-days_book_signup,-book_month,-book_season)
summary(myData)

inTrain <- createDataPartition(y=myData$country_destination,p=0.8,list=FALSE)
trainData <-myData[inTrain,]
testData <- myData[-inTrain,]
testLabels <- testData$country_destination
testData <- select(testData,-country_destination)

#without caret
#xgboost.fit <- ranger(country_destination~.,data=trainData,importance = "impurity")

#with caret
xgboost.fit <- train(country_destination~.,
                    data=trainData,
                    method="xgbTree",
                    verbose=TRUE)
save(xgboost.fit,file="xgboostFit.RData")

predictions.ranger <- predict(ranger.fit,testData)
confusionMatrix(predictions.ranger$predictions,testLabels)
plot(ranger.fit)
ranger.fit

# Variable importance
sort(importance(ranger.fit),decreasing=TRUE) # ranger function
varImp(ranger.fit) # caret function

