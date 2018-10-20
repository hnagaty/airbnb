library(caret)
library(ranger)
library(glmnet)
library(xgboost)


# forest tree for destination country
# it was very slow and I aborted it


myData <- trainUsers %>%
  select(-id,-age,-made_booking,-days_book_signup)
summary(myData)

inTrain <- createDataPartition(y=myData$country_destination,p=0.8,list=FALSE)
trainData <-myData[inTrain,]
testData <- myData[-inTrain,]
testLabels <- testData$country_destination
testData <- select(testData,-country_destination)

ranger.fit <- ranger(country_destination~.,data=trainData,importance = "impurity")
predictions <- predict(ranger.fit,testData)

ranger.fit <- train(country_destination~.,
                    data=trainData,
                    method="ranger")
save(ranger.fit,file="rangetFit.RData")

predictions.ranger <- predict(ranger.fit,testData)
confusionMatrix(predictions.ranger$predictions,testLabels)
plot(ranger.fit)
ranger.fit

# Variable importance
sort(importance(ranger.fit),decreasing=TRUE) # ranger function
varImp(ranger.fit) # caret function
