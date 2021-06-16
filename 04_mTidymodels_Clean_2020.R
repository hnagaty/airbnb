
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(lubridate)
library(treesnip) #lightgbm
library(vip)


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
#' @param mode if normal then normal accuracy. If binary then binary accuracy "NDF vs NON-NDF"
#' @return numeric, the accuracy score
#' @export
#'
#' @examples
CalcSoftProbAcc <- function(y, X=NULL, y_preds = NULL, clf = NULL, nclass = NULL, mode = "normal") {
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
  if (mode == "normal") {
    accuracy <- sum(topPred == y)/length(y)
    return(accuracy)
  }
  else if (mode == "binary") {
    accuracy <- sum((topPred == 1 & y == 1) | (topPred != 1 & y != 1))/length(y)
    return(accuracy)
  }
}


CalcScoreMetrics <- function (train_y, train_preds, test_y, test_preds, nclass) {
  train.acc <- CalcSoftProbAcc(y = as.numeric(train_y),
                               y_preds=as.matrix(train_preds),
                               nclass = nclass,
                               mode = "normal")
  test.acc <- CalcSoftProbAcc(y = as.numeric(test_y),
                              y_preds=as.matrix(test_preds),
                              nclass = nclass,
                              mode = "normal")
  train.binary.acc = CalcSoftProbAcc(y = as.numeric(train_y),
                                     y_preds=as.matrix(train_preds),
                                     nclass = nclass,
                                     mode = "binary")
  test.binary.acc = CalcSoftProbAcc(y = as.numeric(test_y),
                                    y_preds=as.matrix(test_preds),
                                    nclass = nclass,
                                    mode = "binary")
  train.NDCG <- CalcNDCG(y = as.numeric(train_y), y_preds = as.matrix(train_preds))
  test.NDCG <- CalcNDCG(y = as.numeric(test_y), y_preds = as.matrix(test_preds))
  return(tibble(train.acc, test.acc, train.binary.acc, test.binary.acc, train.NDCG, test.NDCG))
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


FitModel <- function(clf,
                     data_recipe,
                     data,
                     model_notes,
                     model_date) {
  
  train_test_split <- initial_split(data, prop = 4/5, strata = 'country_destination')
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
              metrics = metrics,
              plot = p,
              confusion_matrix = confusion_matrix,
              model_notes = model_notes,
              model_create_date = model_date,
              model_fit_datetime = now()))
}


format_preds <- function(preds, userid) {
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


# I'm applying the fit here, whereas I tuned the model somewhere else ------

# load non-processed data
load('Scenario_01_with_sessions_train.RData')
load('Scenario_01_with_sessions_test.RData')

nclass <- 12
dest_levels <- levels(kaggleTrain$country_destination)



# cyclic encoding for the dates into month sin & cos
kaggle_train_data <- EncodeCyclicDates(kaggleTrain, 'date_account_created', 'date_first_active')
kaggle_test_data <- EncodeCyclicDates(kaggleTest, 'date_account_created', 'date_first_active')

data_recipe <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, date_first_booking, new_role = "ID") %>%
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_BoxCox(starts_with("device_type"), starts_with("action_detail")) %>% 
  step_zv(all_numeric())

# load previously tuned best parameter set
best_params <- readRDS("BestBinaryXGBoostClassifier_LightGBM_320.rds")
best_params

clf <-
  boost_tree(
    mtry = 0.6,
    trees = 3600L,
    min_n = 5L,
    tree_depth = 7L,
    learn_rate = 0.00642,
    sample_size = 0.5) %>%
  set_engine("catboost",
             logging_level = "Silent",
             thread_count = 12L,
             auto_class_weights = "SqrtBalanced") %>%
  set_mode("classification")

clf %>% translate()
model_comment <- 'Other params'
model_date <- today()

model_fit <- FitModel(clf, data_recipe, kaggle_train_data, model_comment, model_date)
model_fit$metrics
model_fit$fit_time

# random search

# train test split
train_test_split <- initial_split(kaggle_train_data, prop = 4/5, strata = 'country_destination')
data_train <- training(train_test_split)
data_test <- testing(train_test_split)


autotune_clf <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  sample_size = tune()) %>%
  set_engine("catboost",
             logging_level = "Silent",
             thread_count = 12L,
             auto_class_weights = "SqrtBalanced") %>%
  set_mode("classification")
autotune_clf

final_recipe <- data_recipe
final_recipe

tune_grid <- parameters(autotune_clf) %>% 
  finalize(data_train) %>% 
  grid_random(size = 96)
tune_grid$sample_size <- tune_grid$sample_size/dim(data_train)[1]
tune_grid$mtry <- tune_grid$mtry/dim(data_train)[2]
tune_grid

# the workflow
wflow <- 
  workflow() %>% 
  add_model(autotune_clf) %>% 
  add_recipe(final_recipe)
wflow


# cv folds/resamples
set.seed(345)
folds <- vfold_cv(data_train, v = 10, strata = country_destination)
folds

# the tuning step -- LONG TIME
# IGNORE if tuning is already done
#' ---------------------------------
clf_res <- wflow %>% 
  tune_grid(
    resamples = folds,
    grid = tune_grid,
    control = control_grid(verbose = TRUE)
  )
clf_res


best_clf <- clf_res %>% 
  select_best('accuracy')
best_clf

saveRDS(clf_res, 'ClassifierResamples_CatBoost96.rds')
saveRDS(best_clf, 'BestClassifier_CatBoost96.rds')


clf_res %>%
  show_best("accuracy") 

clf_res %>%
  collect_metrics() %>% 
  ggplot(aes(mean, std_err)) +
  geom_text(aes(label = .config)) +
  geom_point(color = 'red', size = 2, alpha = 0.65) +
  facet_wrap(~ .metric)

# finalise the model using the best parameters
final_wf <- wflow %>% 
  finalize_workflow(best_clf)
final_wf

final_clf <- wflow %>% 
  pull_workflow_spec() %>% 
  finalize_model(best_clf)
final_clf

# final check of metrics using the best model
final_fit <- final_wf %>% 
  last_fit(train_test_split)

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>% 
  roc_curve(country_destination, starts_with(".pred"), -".pred_class") %>% 
  autoplot()
# the roc curve is misleading here, because the predictions are not probabilities
# I don't know why there are not probabilities

final_fit %>%
  collect_predictions() %>% 
  conf_mat(country_destination, .pred_class) %>% 
  summary()


# train using my own FitModel()
my_fit <- FitModel(final_clf, data_recipe, kaggle_train_data, model_comment, model_date)
my_fit$metrics
my_fit$fit_time


# train the best model with all the data

final_clf <- final_wf %>% 
  fit(data = kaggle_train_data)

final_fit <- final_clf %>% 
  pull_workflow_fit() %>% as_tibble()


s <- final_fit %>%
  map("feature_importances") %>% 
  compact() %>% 
  pluck(1) 

data.frame(Feature = rownames(s), Importance = s) %>% 
  slice_max(Importance, n = 10) %>% 
  mutate(Prefix = str_extract(Feature, "(.*)-")) %>% 
  ggplot(aes(x=fct_reorder(Feature, Importance), y=Importance,
             fill = Prefix)) +
  geom_col() +
  coord_flip() + 
  labs(title = "Feature Importance",
       subtitle = "For CatBoost Model",
       x = "Importance",
       y = "Feature")


 
# END IS HERE ===========================================

wflow <- 
  workflow() %>% 
  add_model(clf) %>% 
  add_recipe(data_recipe)

clf_fit <- fit(wflow, data = kaggle_train_data)

best_fit <- clf_fit %>% 
  pull_workflow_fit()
best_fit
best_fit$elapsed



