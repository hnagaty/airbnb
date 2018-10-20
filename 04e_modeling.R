# xgboost for destination country

library(caret)
library(xgboost)
#library(doParallel)

# xgboot doens't use all cores by default
# So I have to use doParallel()
#cl <- makeCluster(7) #snow like functionality
#registerDoParallel(cores=2) #multi-core like functionality ==> not needed as XGBoost has built it Parallel Computing
#stopCluster(cl)

#objective=multi:softprob



# With xgboost, I should
# Dummification of categorical values
# Convert target classes to numerical values

# I didn't do CV/resampling because this is just a high level trial of the model functionality & output
#  seems that xgboost has built in CV methods 

allData <- allUsers %>%
  select(-id,-age,-made_booking,-days_book_signup,-book_month,-book_season)
summary(allData)

factCols <- c("gender","ageRange","signup_method","signup_flow","language","affiliate_channel",
              "affiliate_provider","first_affiliate_tracked","signup_app",
              "first_device_type","first_browser",
              'signup_month','signup_season')

# One hot encoding for categorical variables
allDataHotEnc <- as.data.frame(model.matrix(~.+0,data = allData[,factCols]))
allDataHotEnc <- bind_cols(allData[,-c(which(colnames(allData) %in% factCols))],allDataHotEnc)

trainDataHotEnc <- allDataHotEnc %>%
  filter(dataset=="train") %>%
  select(-dataset)

testDataHotEnc <- allDataHotEnc %>%
  filter(dataset=="test") %>%
  select(-dataset)
rm(allDataHotEnc)

# Another way for one hot encoding
#OneHotTest <- dummyVars(~ gender+ageRange,data=myData)
#OneHotDf <- as.data.frame(predict(OneHotTest,myData))

set.seed(45)
inTrain <- createDataPartition(y=trainDataHotEnc$country_destination,p=0.8,list=FALSE)

trainSplit <-trainDataHotEnc[inTrain,]
trainLabels <- as.numeric(trainSplit$country_destination)-1
labelLevels <- levels(trainSplit$country_destination)
trainSplit <-as.matrix(select(trainSplit,-country_destination))
trainMatrix <- xgb.DMatrix(data = trainSplit,label = trainLabels)

testSplit <- trainDataHotEnc[-inTrain,]
testLabels <- as.numeric(testSplit$country_destination)-1
testSplit <-as.matrix(select(testSplit,-country_destination))
testMatrix <- xgb.DMatrix(data = testSplit,label = testLabels)
getinfo(trainMatrix,name="label")

#without caret
params <- list(booster = "gbtree", objective = "multi:softprob", eta=0.3,
               gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv.fit <- xgboost(params=params, data=trainMatrix,evaluation="ndcg",
                   nrounds = 100,num_class=12,verbose = TRUE,nfold=2)
xgb.save(xgb.fit,"xgbMultiHotEnc_WithoutCaret_CV")

xgb.fit <- xgb.load("xgbMultiHotEnc_WithoutCaret")

print(xgb.fit)

predictions <- predict(xgb.fit,testMatrix,reshape = TRUE)
str(predictions)
colnames(predictions) <- labelLevels
predictionsClass <- as.factor(colnames(predictions)[max.col(predictions)])
table(predictionsClass) #> nonsense results!!!

# Feature importance
featImp <- xgb.importance (feature_names = colnames(trainSplit),model = xgb.fit)
xgb.plot.importance (importance_matrix = featImp[1:20]) 
