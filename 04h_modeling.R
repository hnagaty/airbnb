# xgboost on destination
# no one hot encoding

library(caret)
library(ROSE)
library(xgboost)
library(StatRank)

trainData <- allUsersExpanded %>%
  filter(dataset=="train") %>%
  select(-age,-days_book_signup,-dataset,-made_booking,-destination_language,
         -lat_destination,-distance_km,-book_month,-book_season)
userIds <- trainData$id
trainData <- select(trainData,-id)
summary(trainData)

# train test split
set.seed(45)
inTrain <- createDataPartition(y=trainData$country_destination,p=0.8,list=FALSE)
trainSplit <-trainData[inTrain,]
testSplit <- trainData[-inTrain,]
trainUserIds <- userIds[inTrain]
testUserIds <- userIds[-inTrain]

testLabels <- testSplit$country_destination
testSplit <- select(testSplit,-country_destination)

#Pre Processing
preProc <- preProcess(trainSplit,method=c("medianImpute","zv","center","scale"))
trainSplitProc <- predict(preProc,trainSplit)
testSplitProc <- predict(preProc,testSplit)

# Upsampling
set.seed(9560)
table(trainSplitProc$country_destination) 
trainSplitProcResampled <- upSample(x = trainSplitProc[, -ncol(trainSplitProc)],
                     y = trainSplitProc$country_destination)                         
table(trainSplitProcResampled$country_destination) 

myControl <- trainControl(method = "cv",  number = 10,
                          classProbs = TRUE,
                          verboseIter = TRUE)

myGrid <- expand.grid( # ==> single parameter to speed up. Should be multi grid to explore hyper-parameters
  eta=0.6,
  max_depth=6,
  gamma=0,
  colsample_bytree=0.8,
  min_child_weight=1,
  subsample=0.75,
  nrounds=50)

set.seed(15)
xgboost.fit <- train(country_destination~.,
                    data=trainSplitProc,
                    method = "xgbTree",
                    trControl = myControl,
                    metric = "Kappa",
                    #preProcess=c("medianImpute","zv","nzv"),
                    tuneGrid=myGrid)

save(xgboost.fit,file="xgBoostMultiExpandFeaturesWithResamplingII_202005.RData")
load("xgBoostMultiExpandFeaturesWithResampling.RData_202005",verbose = TRUE)


print(xgboost.fit)
plot(xgboost.fit)

variableImportance <- varImp(xgboost.fit) # caret function
myLevels <- rownames(variableImportance$importance)[1:20]
variableImportance$importance %>%
  mutate(Feature=factor(rownames(.),levels=rownames(.))) %>%
  rename(Importance=Overall) %>%
  top_n(20,Importance) %>%
  ggplot(aes(x=Feature,y=Importance,fill=Feature)) +
    geom_col() +
    coord_flip() +
    theme(legend.position="none") +
    labs(title="Feature Importance",subtitle="XGBoost Model",x="Feature Name",y="Importance Score")
    

  
predictionsClass <- predict(xgboost.fit,testSplitProc)
confusionMatrix(predictionsClass,testLabels)
# the above model never identified a country other than US or NDF
# using class probabilities will get other recommendations

# Probability predictions
predictions <- predict(xgboost.fit,testSplitProc,type = "prob")
str(predictions)
testUserIds <- data.frame(testUserIds,testLabels,stringsAsFactors = FALSE)

topPrediction <- bind_cols(testUserIds,predictions) %>%
  gather("Destination","Prob",3:14) %>%
  rename(UserID=testUserIds) %>%
  group_by(UserID,testLabels) %>%
  arrange (UserID,desc(Prob)) %>%
  top_n(5,Prob) %>%
  mutate(pcnt=Prob/sum(Prob),cumPcnt=cumsum(pcnt)) %>%
  arrange (UserID,desc(Prob)) %>%
  filter (cumPcnt<0.85) %>%
  mutate(EstScore=dense_rank(desc(Prob))) %>%
  ungroup

forScoring <- topPrediction %>%
  mutate(actualScore=as.numeric(testLabels==Destination)) %>%
  select(UserID,EstScore,actualScore)

Evaluation.NDCG(forScoring$EstScore,forScoring$actualScore)



# Kaggle test set ---------------------------------------------------------

load("models/xgBoostMultiExpandFeaturesWithResampling.RData",verbose = TRUE)

testData <- allUsersExpanded %>%
  filter(dataset=="test") %>%
  select(-age,-days_book_signup,-dataset,-made_booking,-destination_language,
         -lat_destination,-distance_km,-book_month,-book_season,-country_destination)
testData <- select(testData,-id)

summary(testData)

testData <- testData[!duplicated(testData$id),]
testData <- testData[!is.na(testData$avgSecs),]


predictions <- predict(xgboost.fit,testData2,type = "prob",verbose=TRUE)
str(predictions)
userIds <- testData$id
testUserIds <- data.frame(userIds,stringsAsFactors = FALSE)

topPrediction <- bind_cols(testUserIds,predictions) %>% 
  gather("Destination","Prob",2:13) %>%
  rename(UserID=userIds) %>%
  group_by(UserID) %>%
  arrange (UserID,desc(Prob)) %>%
  top_n(5,Prob) %>%
  mutate(pcnt=Prob/sum(Prob),cumPcnt=cumsum(pcnt)) %>%
  arrange (UserID,desc(Prob)) %>%
  filter (cumPcnt<0.85) %>%
  mutate(EstScore=dense_rank(desc(Prob))) %>%
  ungroup %>%
  select(UserID,Destination)

write_csv(topPrediction,"kaggleout.csv")
