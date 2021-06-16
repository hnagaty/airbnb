#' Trying to reproduce the problem that occurs when using a parrallel backend
#' This file uses the credit card sample data

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(doParallel)

all_cores <- detectCores(logical = FALSE) # Currently 'logical' is honoured only on macOS, Solaris and Windows
cl <- makePSOCKcluster(all_cores/2)
#cl <- makeForkCluster(all_cores/2)
registerDoParallel(cl)
getDoParWorkers()

data("credit_data")

# recipes
data_recipe <- 
  recipe(Status ~ ., data = credit_data) %>% 
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -all_outcomes()) %>% 
  step_other(all_nominal(), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
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
  set_engine("xgboost", nthread = 12, verbose = 1) %>%
  set_mode("classification")
autotune_clf

# train test split
set.seed(781)
train_test_split <- initial_split(credit_data, prop = 4/5, strata = 'Status')
data_train <- training(train_test_split)
data_test <- testing(train_test_split)


# the search grid
random_grid <- parameters(autotune_clf) %>% 
  finalize(data_train) %>% 
  grid_random(size = 6)
random_grid$sample_size <- random_grid$sample_size/dim(data_train)[1]
random_grid

# the workflow
wflow <- 
  workflow() %>% 
  add_model(autotune_clf) %>% 
  add_recipe(data_recipe)
wflow

# cv folds aka resamples
set.seed(345)
folds <- vfold_cv(data_train, v = 10, strata = Status)
folds

# the tuning step -- LONG TIME
# IGNORE if tuning is already done
#' ---------------------------------
clf_res <- wflow %>% 
  tune_grid(
    resamples = folds,
    grid = random_grid,
    control = control_grid(verbose = TRUE, pkgs = 'doParallel')
  )

clf_res

