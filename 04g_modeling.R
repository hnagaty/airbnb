# logistic regression for destination country

library(caret)
library(ranger)
library(glmnet)
library(xgboost)
library(doParallel)

# xgboot doens't use all cores by default
# So I have to use doParallel()
#cl <- makeCluster(7) #snow like functionality
registerDoParallel(cores=2) #multi-core like functionality
#stopCluster(cl)

#objective=multi:softprob



# With xgboost, I should
# Dummification of categorical values
# Convert target classes to numerical values

# I didn't do CV/resampling because this is just a high level trial of the model functionality & output
#  seems that xgboost has built in CV methods 

myData <- allUsers %>%
  #filter(dataset="train") %>%
  select(-id,-age,-made_booking,-days_book_signup,-book_month,-book_season)
summary(myData)

factCols <- c("gender","ageRange","signup_method","signup_flow","language","affiliate_channel",
              "affiliate_provider","first_affiliate_tracked","signup_app",
              "first_device_type","first_browser",
              'signup_month','signup_season')

# One hot encoding for categorical variables
oneHot.df <- as.data.frame(model.matrix(~.+0,data = myData[,factCols]))
myData.OneHot <- bind_cols(myData[,-c(which(colnames(myData) %in% factCols))],oneHot.df)

trainSet <- myData.OneHot %>%
  filter(dataset=="train") %>%
  select(-dataset)

testSet <- myData.OneHot %>%
  filter(dataset=="test") %>%
  select(-dataset)


# Another way for one hot encoding
#OneHotTest <- dummyVars(~ gender+ageRange,data=myData)
#OneHotDf <- as.data.frame(predict(OneHotTest,myData))

inTrain <- createDataPartition(y=trainSet$country_destination,p=0.8,list=FALSE)
trainData <-trainSet[inTrain,]
testData <- trainSet[-inTrain,]
testLabels <- testData$country_destination
testData <- select(testData,-country_destination)

#without caret
#xgboost.fit <- ranger(country_destination~.,data=trainData,importance = "impurity")

#with caret
xgboost.fit <- train(country_destination~.,
                     data=trainData,
                     method="xgbTree",
                     preProcess=c("zv","nzv","medianImpute"),
                     verbose=TRUE,
                     objective=multi:softprob)
save(xgboost.fit,file="xgboostFit.RData")

predictions.ranger <- predict(ranger.fit,testData)
confusionMatrix(predictions.ranger$predictions,testLabels)
plot(ranger.fit)
ranger.fit

# Variable importance
sort(importance(ranger.fit),decreasing=TRUE) # ranger function
varImp(ranger.fit) # caret function
