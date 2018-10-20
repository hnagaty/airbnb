# glmnet for destination country
# trash is in here

library(caret)
library(ranger)
library(glmnet)
library(xgboost)
library(doParallel)
library(naivebayes)

# xgboot doens't use all cores by default
# So I have to use doParallel()
#cl <- makeCluster(7) #snow like functionality
registerDoParallel(cores=4) #multi-core like functionality
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


# One Hot encoding --------------------------------------------------------

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


# Modeling ----------------------------------------------------------------

inTrain <- createDataPartition(y=trainSet$country_destination,p=0.8,list=FALSE)
trainData <-trainSet[inTrain,]
testData <- trainSet[-inTrain,]
testLabels <- testData$country_destination
testData <- select(testData,-country_destination)

myControl <- trainControl(method = "cv", number = 4,
                          classProbs = TRUE,
                          verboseIter = TRUE)


glmnet.model <- train(country_destination~., data=trainData,
                      method = "naive_bayes",
                      trControl = myControl,
                      preProcess = c("center","scale"))

save(xgboost.fit,file="xgboostFit.RData")

predictions.nb <- predict(glmnet.model,testData,type="prob")
confusionMatrix(predictions.nb,testLabels)
plot(glmnet.model)
glmnet.model

# Variable importance
sort(importance(ranger.fit),decreasing=TRUE) # ranger function
varImp(glmnet.model) # caret function
