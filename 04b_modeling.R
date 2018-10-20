# forest tree model for binary classification (reserved vs non reserved)

library(caret)
library(ranger)
library(xgboost)
library(adabag)
#library(glmnet)
#library(xgboost)


#####
# I should leave book month and book season in the trials
####


trainData <- allUsers %>%
  filter(dataset=="train") %>%
  select(-id,-age,-country_destination,-days_book_signup,-dataset,-book_month,-book_season)
summary(trainData)


# train test split
inTrain <- createDataPartition(y=trainData$made_booking,p=0.8,list=FALSE)
trainSplit <-trainData[inTrain,]
testSplit <- trainData[-inTrain,]
testLabels <- testSplit$made_booking
testSplit <- select(testSplit,-made_booking)


#without caret
ranger.fit <- ranger(made_booking~.,data=trainSplit,importance = "impurity",probability = TRUE)
save(ranger.fit,file="ranger.fitBinary.RData")
ranger.fit <- load("rangerFitBi.RData")
print(ranger.fit)
print(ranger.fit$forest)
sort(importance(ranger.fit),decreasing=TRUE)

predictions <- predict(ranger.fit,testSplit)
predictionsClass <- as.factor(colnames(predictions$predictions)[max.col(predictions$predictions)])
confusionMatrix(predictionsClass,testLabels)


#with caret
myControl <- trainControl(method = "cv",  number = 2,
                          classProbs = TRUE,
                          verboseIter = TRUE)

ranger.fit <- train(made_booking~.,
                    data=trainData,
                    method = "xgbTree",
                    trControl = myControl,
                    metric = "Accuracy",
                    preProcess = c("zv","nzv"))

save(ranger.fit,file="xgBoostBinary.RData")

print(ranger.fit)
plot(ranger.fit)

# Variable importance
# ranger function
varImp(ranger.fit) # caret function
predictionsClass <- predict(ranger.fit,testSplit)
confusionMatrix(predictionsClass,testLabels)
predictions <- predict(ranger.fit,testSplit,type = "prob")

# Notes
# Using booking month and booking season dramatically improved the
# results. Those can not be useed however in the competition setting, because the
# booking date is not provided in the test data. However, in a real life
# scenario, it can be used where the booking date can be set to the date at which the
# prediction is made.

# training a 2 class classifier is signifiantly faster than training a multiclass one
# It seems that ranger uses parrallel computing by default. I found that all cores are busy during the training
# In contrast, xgboost only used a single core while training
# I coulnd't find a way in ranger to extract the probablities (i need them to make 5 sorted destinations 
# for each use). Consequently, I'm going for xgboost.

