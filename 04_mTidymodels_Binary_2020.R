
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(lubridate)
library(vip)
library(treesnip) #lightgbm
library(rlist) #dealing with lists
library(discrim) #naive bayes in tidymodels
library(probably)
library(caret) # for the confusionMatrix()

# User defined functions --------------------------------------------------

#' Cyclic encoding for date columns in a dataframe or tibble, using 'month' period
#' It relies on lubridate::cyclic_encoding() for the actual encoding
#'
#' @param col the column name to be encoded
#' @param df the dataframe
#'
#' @return a tibble with the encoded dates in cos.month & sin.month
#' @export
#'
#' @examples
#' EncodeCyclicDateSingleCol('date_account_created', kaggleTrain) %>% tail()
#' # A tibble: 6 x 2
#' date_account_created.sin.month date_account_created.cos.month
#'                            <dbl>                          <dbl>
#' 1                         -0.208                          0.978
#' 2                         -0.208                          0.978
#' 3                         -0.208                          0.978
#' 4                         -0.208                          0.978
#' 5                         -0.208                          0.978
#' 6                         -0.208                          0.978
#' 
EncodeCyclicDateSingleCol<- function(col, df) {
  encoded <- cyclic_encoding(df[[col]], 'month') %>% 
    as_tibble() %>% 
    rename_with( ~ paste(col, .x, sep = "."), ends_with(".month"))
  return(encoded)
}



#' Encode cyclic dates for several columns
#' The encoding is done for a 'month' period
#' The function removes the original date columns from the df
#' @param df The dataframe
#' @param ... the columns to encode
#'
#' @return the input dataframe after binding with the encoded dates
#' @export
#'
#' @examples
#' 
EncodeCyclicDates <- function(df, ...) {
  cols <- list(...)
  encoded <- map(cols, EncodeCyclicDateSingleCol, df = df)
  encoded_df <- bind_cols(df, encoded)
  encoded_df <- encoded_df %>% 
    select(- c(...))
  return(encoded_df)
}

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


#' Calculates the nDCG score for a group of predicted values
#' y_preds is a matrix of probabilities
#' @param y The true labels
#' @param X The X matrix. If provided, then y will be predicted
#' @param y_preds 
#' @param clf The clf to use to predict y from X
#' @param nclass The number of classes
CalcNDCG<- function(y, X=NULL, y_preds = NULL, clf = NULL, nclass = NULL) {
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
CalcSoftProbAcc <- function(y, X=NULL, y_preds = NULL, clf = NULL, nclass = NULL) {
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


CalcScoreMetrics <- function (train_y, train_preds, test_y, test_preds, nclass) {
  train.acc <- CalcSoftProbAcc(y = as.numeric(train_y), y_preds=as.matrix(train_preds), nclass = nclass)
  test.acc <- CalcSoftProbAcc(y = as.numeric(test_y), y_preds=as.matrix(test_preds), nclass = nclass)
  train.NDCG <- CalcNDCG(y = as.numeric(train_y), y_preds = as.matrix(train_preds))
  test.NDCG <- CalcNDCG(y = as.numeric(test_y), y_preds = as.matrix(test_preds))
  return(tibble(train.acc, test.acc, train.NDCG, test.NDCG))
}


#' Given a vector of probabilities, this function just gets the 
#' highest probability prediction
#' @param props 
#'  a matrix of dims nxm, where n = no. observations, and m = no. of levels
#' @return
#'  returns a vector of size n, of type factor, that lists the top prediction
#' @export
#' @examples
GetTopPred <- function(props) {
  levels <- sub(".pred_", "", colnames(props))
  ordered_preds <- apply(props, 1, order, decreasing = TRUE)
  top_pred <- as.factor(ordered_preds[1,])
  levels(top_pred) <- levels
  return(top_pred)
}


FitModelOLD <- function(clf,
                     data_recipe,
                     kaggle_train_data,
                     kaggle_test_data,
                     model_notes,
                     model_date) {
  
  train_test_split <- initial_split(kaggle_train_data, prop = 4/5, strata = 'country_destination')
  data_train <- training(train_test_split)
  data_test <- testing(train_test_split)
  
  wflow <- 
    workflow() %>% 
    add_model(clf) %>% 
    add_recipe(data_recipe)
  
  clf_fit <- fit(wflow, data = data_train)
  best_fit <- clf_fit %>% 
    pull_workflow_fit()
  fit_time <- best_fit$elapsed
  best_params <- best_fit$fit$params
  
  train_preds = predict(clf_fit, new_data = data_train, type = 'prob')
  test_preds = predict(clf_fit, new_data = data_test, type='prob')
  data_test <- data_test %>%
    mutate(pred = GetTopPred(test_preds))
  metrics <- CalcScoreMetrics(train_y = data_train$country_destination,
                              train_preds =  train_preds,
                              test_y = data_test$country_destination,
                              test_preds = test_preds,
                              nclass = nclass)
  p <- data_test %>%
    select(id, country_destination, pred) %>%
    rename(GroundTruth = country_destination, Prediction = pred) %>% 
    gather(Prediction, GroundTruth, key = 'Type', value = 'Destination', factor_key = TRUE) %>%
    mutate(Destination = factor(Destination, levels = dest_levels)) %>% 
    ggplot(aes(x=id, y=Destination, fill=Type)) +
    geom_tile(alpha = 0.6) +
    scale_x_discrete(labels=NULL, breaks=NULL)
  
  confusion_matrix <- conf_mat(data_test, country_destination, pred)
  
  return(list(wflow_fit = clf_fit,
              fit_time = fit_time,
              fit_params = best_params,
              metics = metrics,
              plot = p,
              confusion_matrix = confusion_matrix,
              model_notes = model_notes,
              model_create_date = model_date,
              model_fit_datetime = now()))
}


FitModel <- function(clf,
                     data_recipe,
                     model_notes,
                     model_date,
                     kaggle_train_data,
                     tune_n = 10) {
  
  print(paste("Now fitting", model_notes))
  
  train_test_split <- initial_split(kaggle_train_data, prop = 4/5, strata = 'country_destination')
  data_train <- training(train_test_split)
  data_test <- testing(train_test_split)
  
  wflow <- 
    workflow() %>% 
    add_model(clf) %>% 
    add_recipe(data_recipe)
  
  clf_fit <- fit(wflow, data = data_train)
  best_fit <- clf_fit %>% 
    pull_workflow_fit()
  fit_time <- best_fit$elapsed
  best_params <- best_fit$fit$params
  
  train_pred <- 
    predict(clf_fit, data_train) %>% 
    bind_cols(predict(clf_fit, data_train, type = "prob")) %>% 
    bind_cols(data_train %>% 
                select(booked))
  
  test_pred <- 
    predict(clf_fit, data_test) %>% 
    bind_cols(predict(clf_fit, data_test, type = "prob")) %>% 
    bind_cols(data_test %>% 
                select(booked))
  
  return(list(wflow_fit = clf_fit,
              fit_time = fit_time,
              fit_params = best_params,
              train_pred = train_pred,
              test_pred = test_pred,
              model_create_date = model_date,
              model_fit_datetime = now()))
  print(paste("Done fitting", model_notes))
}



# Common & initialization -------------------------------------------------

# load non-processed data
load('Scenario_01_with_sessions_train.RData')
load('Scenario_01_with_sessions_test.RData')

nclass <- 12
dest_levels <- levels(kaggleTrain$country_destination)

# cyclic encoding for the dates into month sin & cos
kaggle_train_data <- EncodeCyclicDates(kaggleTrain, 'date_account_created', 'date_first_active')
kaggle_test_data <- EncodeCyclicDates(kaggleTest, 'date_account_created', 'date_first_active')

# convert country_destination to binary; Booked YES/NO
kaggle_train_data$booked <- fct_collapse(kaggle_train_data$country_destination,
               YES = dest_levels[-match("NDF", dest_levels)],
               NO = 'NDF') %>% 
  fct_relevel("YES")

# recipes
data_recipe01 <- 
  recipe(booked ~ ., data = head(kaggle_train_data)) %>% 
  update_role(id, date_first_booking, country_destination, new_role = "ID") %>%
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())


data_recipe02 <-
  # na ommit instead of median impute
  recipe(booked ~ ., data = head(kaggle_train_data)) %>% 
  update_role(id, date_first_booking, country_destination, new_role = "ID") %>%
  step_naomit(all_predictors(), skip = TRUE) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

data_recipe03 <- # knn impute instead of median impute
  recipe(booked ~ ., data = head(kaggle_train_data)) %>% 
  update_role(id, date_first_booking, country_destination, new_role = "ID") %>%
  step_knnimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

data_recipe04 <- 
  # excluding age
  recipe(booked ~ ., data = head(kaggle_train_data)) %>% 
  update_role(id, date_first_booking, country_destination, age, new_role = "ID") %>%
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# no dummy vars
data_recipe06 <- 
  recipe(booked ~ ., data = head(kaggle_train_data)) %>% 
  update_role(id, date_first_booking, country_destination, new_role = "ID") %>%
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_zv(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# models
clf_01 <-
  boost_tree() %>%
  set_engine("xgboost", nthread = 12, verbose = 1) %>%
  set_mode("classification")

clf_02 <-
  boost_tree() %>%
  set_engine("lightgbm", nthread = 12, verbose = 1) %>%
  set_mode("classification")

clf_03 <-
  logistic_reg() %>%
  set_engine("glmnet", nthread = 12, verbose = 1) %>%
  set_mode("classification")

clf_04 <-
  logistic_reg() %>%
  set_engine("stan", verbose = 1) %>%
  set_mode("classification")

clf_05 <-
  naive_Bayes() %>%
  set_engine("klaR") %>%
  set_mode("classification")

clf_06 <-
  boost_tree() %>%
  set_engine("C5.0", nthread = 12, verbose = 1) %>%
  set_mode("classification")

models_list <- list(
  model1 = list(
    clf = clf_01,
    data_recipe = data_recipe01,
    model_notes = "XGBoost with median imputation",
    model_date = as.Date("2020-08-05")
  ),
  model2 = list(
    clf = clf_01,
    data_recipe = data_recipe02,
    model_notes = "XGBoost with omit of missing values",
    model_date = as.Date("2020-08-05")
  ),
  model3 = list(
    clf = clf_02,
    data_recipe = data_recipe01,
    model_notes = "LightGBM",
    model_date = as.Date("2020-08-05")
  ),
  model4 = list(
    clf = clf_03,
    data_recipe = data_recipe01,
    model_notes = "GLMNet",
    model_date = as.Date("2020-08-05")
  ),
  model5 = list(
    clf = clf_04,
    data_recipe = data_recipe01,
    model_notes = "STAN",
    model_date = as.Date("2020-08-05")
  ),
  model6 = list(
    clf = clf_05,
    data_recipe = data_recipe01,
    model_notes = "Naive Bayes",
    model_date = as.Date("2020-08-05")
  ),
  model7 = list(
    clf = clf_06,
    data_recipe = data_recipe01,
    model_notes = "C5.0",
    model_date = as.Date("2020-08-05")
  )
)

list.remove(models_list, "model2") -> models_list
list.remove(models_list, "model4") -> models_list
list.remove(models_list, "model6") -> models_list

model_list_transpose <- transpose(models_list)


# Fit using FitModel() ----------------------------------------------------

fit_results <- pmap(model_list_transpose, FitModel, kaggle_train_data = sample_data)

sample_split <- initial_split(kaggle_train_data, prop = 1/100, strata = 'country_destination')
sample_data <- training(sample_split)

fit_1 <- FitModel(clf = clf,
                  data_recipe = data_recipe,
                  kaggle_train_data = kaggle_train_data,
                  model_notes = model_comment,
                  model_date = model_date)

prob_threshold <- 0.5
fit_1$train_pred$pred <- ifelse(fit_1$train_pred$.pred_YES > prob_threshold, "YES", "NO") %>% 
  factor(levels = c('YES', 'NO'))
fit_1$test_pred$pred <- as.factor(ifelse(fit_1$test_pred$.pred_YES > prob_threshold, "YES", "NO")) %>% 
  factor(levels = c('YES', 'NO'))

fit_1$train_pred %>%
  accuracy(truth = booked, pred)

fit_1$test_pred %>%
  accuracy(truth = booked, pred)

fit_1$test_pred %>%
  conf_mat(truth = booked, .pred_class) %>% 
  autoplot(type = 'heatmap')

fit_1$test_pred %>%
  roc_curve(truth = booked, .pred_YES) %>% 
  autoplot()

fit_1$test_pred %>%
  roc_auc(truth = booked, .pred_YES)


# Using re-sampling for estimating the metrics -----------------------------

wflow <- 
  workflow() %>% 
  add_model(clf) %>% 
  add_recipe(data_recipe)

set.seed(345)
folds <- vfold_cv(kaggle_train_data, v = 10, strata = booked)
folds
folds$splits[[1]]

set.seed(456)
fit_rs <- 
  wflow %>% 
  fit_resamples(folds)

collect_metrics(fit_rs)



# auto tune the hyper-parameters ------------------------------------------

# train test split
set.seed(781)
train_test_split <- initial_split(kaggle_train_data, prop = 4/5, strata = 'country_destination')
data_train <- training(train_test_split)
data_test <- testing(train_test_split)

# the recipe
data_recipe <- data_recipe06
data_recipe

# xgboost classifier
autotune_clf <-
  boost_tree(tree_depth = tune(),
             trees = tune(),
             learn_rate = tune(),
             stop_iter = 100,
             sample_size = tune(),
             mtry = tune(),
             min_n = tune()) %>%
  set_engine("lightgbm", nthread = 12, verbose = 1) %>%
  set_mode("classification")
autotune_clf




#' xgboost the tune grid, the OLD BAD WAY
# entropy_grid <- grid_max_entropy(trees(),
#                                  tree_depth(),
#                                  learn_rate(),
#                                  finalize(mtry(), data_train),
#                                  finalize(sample_size(), data_train),
#                                  size = 8)
# entropy_grid$sample_size <- entropy_grid$sample_size / dim(data_train)[1]
# entropy_grid

# another syntax for constructing the grid
# from another site, not of this data
# this syntax is BETTER
entropy_grid <- parameters(autotune_clf) %>% 
  finalize(data_train) %>% 
  grid_random(size = 160)
entropy_grid$sample_size <- entropy_grid$sample_size/dim(data_train)[1]
entropy_grid

# the workflow
wflow <- 
  workflow() %>% 
  add_model(autotune_clf) %>% 
  add_recipe(data_recipe)
wflow


# cv folds/resamples
set.seed(345)
folds <- vfold_cv(data_train, v = 10, strata = booked)
folds

# the tuning step -- LONG TIME
# IGNORE if tuning is already done
#' ---------------------------------
clf_res <- wflow %>% 
  tune_grid(
    resamples = folds,
    grid = entropy_grid,
    control = control_grid(verbose = TRUE)
  )

clf_res

clf_res %>%
  collect_metrics()

clf_res %>%
  collect_metrics() %>% 
  ggplot(aes(mean, std_err)) +
  geom_text(aes(label = .config)) +
  geom_point(color = 'red', size = 2, alpha = 0.65) +
  facet_wrap(~ .metric)

best_clf <- clf_res %>% 
  select_best('roc_auc')
best_clf
#best_clf$learn_rate <- 0.001


saveRDS(clf_res, 'ClassifierResamples.rds')
saveRDS(best_clf, 'BestBinaryXGBoostClassifier.rds')

#' END OF IGNORE
#' ---------------------------------

best_clf <- readRDS("BestBinaryXGBoostClassifier.rds")

final_wf <- 
  wflow %>% 
  finalize_workflow(best_clf)
final_wf

final_clf <- final_wf %>% 
  fit(data = data_train)

final_clf %>% 
  pull_workflow_fit() %>% 
  vip()

final_fit <- final_wf %>% 
  last_fit(train_test_split)

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>% 
  roc_curve(booked, .pred_YES) %>% 
  autoplot()

# manually calculate metrics, and tune probability threshold
train_pred <- 
  predict(final_clf, data_train, type = "prob") %>% 
  bind_cols(data_train %>% 
              select(booked))

test_pred <- 
  predict(final_clf, data_test, type = "prob") %>% 
  bind_cols(data_test %>% 
              select(booked))

prob_threshold <- 0.5
train_pred$pred <- ifelse(train_pred$.pred_YES > prob_threshold, "YES", "NO") %>% 
  factor(levels = c('YES', 'NO'))
test_pred$pred <- as.factor(ifelse(test_pred$.pred_YES > prob_threshold, "YES", "NO")) %>% 
  factor(levels = c('YES', 'NO'))

train_pred %>%
  accuracy(truth = booked, pred)

test_pred %>%
  accuracy(truth = booked, pred)

test_pred %>%
  conf_mat(truth = booked, pred) %>% 
  autoplot(type = 'heatmap')

confusionMatrix(test_pred$pred, reference = test_pred$booked)

test_pred %>%
  roc_curve(truth = booked, .pred_YES) %>% 
  autoplot()

test_pred %>%
  roc_auc(truth = booked, .pred_YES)

threshold_data <- test_pred %>%
  threshold_perf(booked, .pred_YES, thresholds = seq(0, 1, by = 0.0005))
threshold_data

threshold_data %>%
  ggplot(aes(x = .threshold, y = .estimate, col = .metric)) +
  geom_line()

max_j_index_threshold <- threshold_data %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)
max_j_index_threshold



