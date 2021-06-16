# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(lubridate)
library(vip)
library(tictoc)


# User defined functions --------------------------------------------------

#' nDCG
#' Calculates the nDCG relevance score for a single observation
#' @param Predicted a vector for the predicted classes
#' @param GroundTruth a single value for the ground truth class
#'
#' @return the nDCG score as a single numeric value
#' @export
#'
#' @examples
#' > pred <- c('FR', 'US', 'IT')
#' > nDCG(pred, 'FR')
#' [1] 1
#' > nDCG(pred, 'US')
#' [1] 0.6309298
nDCG <- function(Predicted, GroundTruth) {
  Relevance <- Predicted == GroundTruth
  i <- seq(1,length(Predicted))
  sum((2^Relevance-1)/log2(i+1))
}

#' calcSoftProbAcc
#' Calculates the accuracy when the input is a probability vector rather than a single value
#' @param y The true labels
#' @param X The X matrix. If provided, then y will be predicted
#' @param y_preds 
#' @param clf The clf to use to predict y from X
#' @param nclass The number of classes
#' @return numeric, the accuracy score
#' @export
#'
#' @examples
calcSoftprobAcc <- function(y, X=NULL, y_preds = NULL, clf = NULL, nclass = NULL) {
  if (is.null(y_preds) & is.null(clf)) {
    print('Both clf and y_preds are NULL')
    return (NULL)
  }
  else if (! is.null(clf)) {
    y_preds <- matrix(predict(clf, X), ncol=nclass, byrow = TRUE)
  }
  orderedPreds <- apply(y_preds, 1, order, decreasing = TRUE)
  #topPreds <- factor(orderedPreds[1,], labels=data$levels)
  topPred <- orderedPreds[1,]
  accuracy <- sum(topPred == y)/length(y)
  return(accuracy)
}


#' Calculates the nDCG score for a group of predicted values
#' y_preds is a matrix of probabilities
#' @param y The true labels
#' @param X The X matrix. If provided, then y will be predicted
#' @param y_preds 
#' @param clf The clf to use to predict y from X
#' @param nclass The number of classes
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


kagglePredict <- function(preds, userid) {
  s1 <- bind_cols(id = userid, preds) %>%
    gather("country","Prob",2:13) %>%
    mutate(country = sub('.pred_', '', country)) %>%
    group_by(id) %>%
    arrange (id, desc(Prob)) %>%
    top_n(5,Prob) %>%
    ungroup %>%
    select(id, country)
  return(s1)
}



printScores <- function (train_y, trainPreds, test_y, testPreds, nclass) {
  # train & test accuracy
  trainAcc <- calcSoftprobAcc(y = as.numeric(train_y), y_preds=as.matrix(trainPreds), nclass = nclass)
  testAcc <- calcSoftprobAcc(y = as.numeric(test_y), y_preds=as.matrix(testPreds), nclass = nclass)
  sprintf('Train accuracy:%.2f%%. Test accuracy:%.2f%%.\n', trainAcc*100, testAcc*100) %>% cat(sep = "")
  # train & test nDCG
  trainNDCG <- calcNDCG(y = as.numeric(train_y), y_preds = as.matrix(trainPreds))
  testNDCG <- calcNDCG(y = as.numeric(test_y), y_preds = as.matrix(testPreds))
  sprintf('Train nDCG:%.2f%%. Test nDCG:%.2f%%.\n', trainNDCG*100, testNDCG*100) %>% cat(sep = "")
}


#' Cyclic encoding for date columns in a dataframe or tibble, using 'month' period
#' #' It relies on lubridate::cyclic_encoding() for the actual encoding
#' This function just facilitates encoding several columns
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
encodeCyclicDates <- function(df) {
  df1 <- df %>%
    bind_cols(
      cyclic_encoding(df$date_account_created, 'month') %>% as_tibble() %>%
        rename(create_month_sin = sin.month, create_month_cos = cos.month)) %>%
    bind_cols(
      cyclic_encoding(df$date_first_active, 'month') %>% as_tibble() %>%
        rename(active_month_sin = sin.month, active_month_cos = cos.month)) %>%
    select(-date_account_created, -date_first_booking, -date_first_active)
  return(df1)  
}



# Using parslip wihout recipes --------------------------------------------
# Using caret pre-processing

# load data that is pre-processed using caret
load('trainProcess_WithSessions_allData.RData') 
load('testProcess_WithSessions_allData.RData')
load('kaggleTestProcess_WithSessions_allData.RData')

# combine features & labels into a single df
trainData <- cbind(data.frame(trainProcess$features), label=trainProcess$label)
testData <- cbind(data.frame(testProcess$features), label=testProcess$label)
kaggleTrainData <- cbind(data.frame(KaggleTestProcess$features), label=KaggleTestProcess$label)

clf <-
  boost_tree(trees = 30) %>%
  set_engine("xgboost", nthread = 12) %>%
  set_mode("classification")

clf
clf %>% translate()

clf.fit <- fit(clf, label ~ ., data = trainData)

clf.fit

# predict train & test
trainPreds = predict(clf.fit, new_data = trainData, type = 'prob')
testPreds = predict(clf.fit, new_data = testData, type='prob')

printScores(train_y = trainData$label, trainPreds =  trainPreds,
            test_y = testData$label, testPreds = testPreds,
            nclass = nclass)

# Predict on Kaggle data
kagglePreds <- predict(xgboost_fit, kaggleTrainData, type = 'prob')

kaggleOut <- kagglePredict(kagglePreds, KaggleTestProcess$userid)
write.csv(kaggleOut, '2020Jul_DataSc01_WithSessions02_allData_tidymodels.csv', quote = FALSE, row.names = FALSE)



# Using recipes for pre-processing the data -------------------------------

# load non-processed data
load('Scenario_01_with_sessions_train.RData')
load('Scenario_01_with_sessions_test.RData')

# cyclic encoding for the dates into month sin & cos
kaggleTrain <- encodeCyclicDates(kaggleTrain)
kaggleTest <- encodeCyclicDates(kaggleTest)

train_test_split <- initial_split(kaggleTrain, prop = 3/4, strata = 'country_destination')

airbnb_train <- training(train_test_split)
airbnb_test <- testing(train_test_split)

airbnb.recipe <- 
  recipe(country_destination ~ ., data = airbnb_train) %>% 
  update_role(id, new_role = "ID") %>%
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  #step_novel(all_nominal(), -has_role('ID'), -all_outcomes(), new_level = 'unknown') %>% 
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  step_adasyn(column = 'country_destination', -has_role('ID')) 


airbnb.recipe
summary(airbnb.recipe)

# just checking the data; this is not needed as I use a workflow
trained.recipe <- prep(airbnb.recipe, training = airbnb_train)
trained.recipe
train_data <-  bake(trained.recipe, new_data = airbnb_train)
test_data <- bake(trained.recipe, new_data = airbnb_test)
table(train_data$country_destination)

clf1 <- multinom_reg() %>% 
  set_engine('keras') %>% 
  set_mode('classification')


clf <-
  boost_tree() %>%
  set_engine("xgboost", nthread = 12, verbose = 1) %>%
  set_mode("classification")

clf <- clf2

clf %>% translate()

airbnb.wflow <- 
  workflow() %>% 
  add_model(clf) %>% 
  add_recipe(airbnb.recipe)

airbnb.wflow

clf.fit <- fit(airbnb.wflow, data = airbnb_train)

clf.fit
clf.fit %>% 
  pull_workflow_fit() 

# predict train & test
train_preds = predict(clf.fit, airbnb_train, type = 'prob')
test_preds = predict(clf.fit, new_data = airbnb_test, type='prob')

printScores(train_y = airbnb_train$country_destination, trainPreds =  train_preds,
            test_y = airbnb_test$country_destination, testPreds = test_preds,
            nclass = nclass)

# using resampling for data validation
set.seed(345)
folds <- vfold_cv(airbnb_train, v = 3, strata = country_destination)
folds
folds$splits[[1]]

clf_resample.fit <- airbnb.wflow %>% 
  fit_resamples(folds, metrics = metric_set(accuracy, kap),
                save_pred = TRUE) # the save_pred option didn't work

clf_resample.fit
collect_metrics(clf_resample.fit)
collect_predictions(clf_resample.fit)

# predict train & test
# I don't know how to make predictions with this

# Predict on Kaggle data
kaggle_preds <- predict(clf.fit, kaggleTest, type = 'prob')

kaggleOut <- kagglePredict(kaggle_preds, kaggleTest$id)
write.csv(kaggleOut, '2020Jul_DataSc01_WithSessions02_allData_tidymodels_keras.csv', quote = FALSE, row.names = FALSE)
  
# variable importance
clf.fit %>% 
  pull_workflow_fit() %>% 
  vip()

# using tunes for hyper-parameter grid search ========================================
# used dials for the auto-grid
gridtune.clf <-
  boost_tree(tree_depth = tune(),
             trees = tune(),
             learn_rate = tune()) %>%
  set_engine("xgboost", nthread = 12, verbose = 1) %>%
  set_mode("classification")
gridtune.clf

airbnb.wflow <- 
  workflow() %>% 
  add_model(gridtune.clf) %>% 
  add_recipe(airbnb.recipe)
airbnb.wflow

tree.grid <- grid_regular(trees(),
                          tree_depth(),
                          learn_rate(),
                          levels = 2)
tree.grid

set.seed(345)
folds <- vfold_cv(airbnb_train, v = 3, strata = country_destination)
folds

clf.resample <- airbnb.wflow %>% 
  tune_grid(resamples = folds,
            grid = tree.grid,
            metric = metric_set(accuracy, kap))

saveRDS(clf.resample, file = 'clf_resample_model.RData')

clf.resample %>% 
  collect_metrics()

clf.resample %>% 
  collect_metrics() %>% 
  mutate(tree_depth = factor(tree_depth), trees = factor(trees)) %>%
  ggplot(aes(learn_rate, mean, color = tree_depth)) +
  geom_line(alpha = 0.8) +
  geom_point(size = 2) +
  facet_grid(.metric ~ trees)


clf.resample %>% 
  show_best(metric = 'accuracy') 

best.clf <- clf.resample %>%
  select_best("accuracy")
best.clf

airbnb.final_wflow <- airbnb.wflow %>% 
  finalize_workflow(best.clf)

airbnb.final_wflow

# fit the best clf to all the training data
final.clf <- airbnb.final_wflow %>% 
  fit(data = airbnb_train)

final.clf %>% 
  pull_workflow_fit() %>% 
  vip()

train_preds <- final.clf %>% 
  predict(airbnb_train, type = 'prob')
test_preds <- final.clf %>% 
  predict(airbnb_test, type = 'prob')

printScores(train_y = airbnb_train$country_destination, trainPreds =  train_preds,
            test_y = airbnb_test$country_destination, testPreds = test_preds,
            nclass = nclass)

# using tunes, with other set of parameters ====================================
gridtune.clf <-
  boost_tree(tree_depth = tune(),
             trees = tune(),
             learn_rate = tune(),
             stop_iter = 100,
             sample_size = tune(),
             mtry = tune()) %>%
  set_engine("xgboost", nthread = 12, verbose = 1) %>%
  set_mode("classification")
gridtune.clf

airbnb.wflow <- 
  workflow() %>% 
  add_model(gridtune.clf) %>% 
  add_recipe(airbnb.recipe)
airbnb.wflow

regular.grid <- grid_regular(trees(),
                          tree_depth(),
                          learn_rate(),
                          finalize(mtry(), airbnb_train),
                          finalize(sample_size(), airbnb_train),
                          levels = 2)
regular.grid

random.grid <- grid_random(trees(),
                          tree_depth(),
                          learn_rate(),
                          finalize(mtry(), airbnb_train),
                          finalize(sample_size(), airbnb_train),
                          size = 32)
random.grid
random.grid$sample_size <- random.grid$sample_size / dim(airbnb_train)[1]
random.grid

entropy.grid <- grid_max_entropy(trees(),
                         tree_depth(),
                         learn_rate(),
                         finalize(mtry(), airbnb_train),
                         finalize(sample_size(), airbnb_train),
                         size = 64)
entropy.grid
entropy.grid$sample_size <- entropy.grid$sample_size / dim(airbnb_train)[1]


tree.grid <- entropy.grid

set.seed(345)
folds <- vfold_cv(airbnb_train, v = 3, strata = country_destination)
folds

tic()
set.seed(345)
clf.resample <- airbnb.wflow %>% 
  tune_grid(resamples = folds,
            grid = tree.grid,
            metric = metric_set(accuracy, kap))
toc()

saveRDS(clf.resample, file = 'clf_resample_model_entropy_grid.RData')
clf.resample <- readRDS('clf_resample_model_entropy_grid.RData')

clf.resample %>% 
  show_best(metric = 'accuracy') 

best.clf <- clf.resample %>%
  select_best("accuracy")
best.clf

airbnb.final_wflow <- airbnb.wflow %>% 
  finalize_workflow(best.clf)

airbnb.final_wflow 

final.clf <- airbnb.final_wflow %>% 
  fit(data = airbnb_train)

final.clf %>% 
  pull_workflow_fit() %>% 
  vip()

train_preds <- final.clf %>% 
  predict(airbnb_train, type = 'prob')
test_preds <- final.clf %>% 
  predict(airbnb_test, type = 'prob')

printScores(train_y = airbnb_train$country_destination, trainPreds =  train_preds,
            test_y = airbnb_test$country_destination, testPreds = test_preds,
            nclass = nclass)

# Predict on Kaggle data
kaggle_preds <- predict(final.clf, kaggleTest, type = 'prob')

kaggleOut <- kagglePredict(kaggle_preds, kaggleTest$id)
write.csv(kaggleOut, '2020Jul_DataSc01_WithSessions02_allData_tidymodels_randomsearch.csv', quote = FALSE, row.names = FALSE)
