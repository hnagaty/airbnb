library(caret)
library(ranger)
library(glmnet)

# first trial with random forest

allData <- trainUsers %>%
  select(-id)

allData$age <- scale(allData$age)
allData$days_book_signup <- scale(allData$days_book_signup) #not the best scaling, as it's not normal distribution

ggplot(allData,aes(x=days_book_signup)) +
  geom_histogram(aes(y=..density..),col="black",fill="cyan") +
  geom_density(col="red")


summary(allData)
    
inTrain <- createDataPartition(y=allData$country_destination,p=0.8,list=FALSE)
trainData <- as.data.frame(allData[inTrain,])
testData <- allData[-inTrain,]

#ranger.fit <- ranger(country_destination~.,data=trainData)


ranger.fit <- train(country_destination~.,
                   data=trainData,
                   method="ranger",
                   tunelength=1,
                   na.action = na.omit)
count(trainData,is.na(country_destination))

