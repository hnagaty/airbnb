#' Trying to reproduce the problem when using parrallel backend
#' This is using the airbnb dataset

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(lubridate)
library(doParallel)

all_cores <- detectCores(logical = FALSE) # Currently 'logical' is honoured only on macOS, Solaris and Windows
#cl <- makePSOCKcluster(all_cores/2)
cl <- makeForkCluster(all_cores/2)
registerDoParallel(cl)
getDoParWorkers()


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
  recipe(booked ~ ., data = kaggle_train_data) %>% 
  update_role(id, date_first_booking, country_destination, new_role = "ID") %>%
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'AllOthers') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# models
clf_01 <-
  boost_tree() %>%
  set_engine("xgboost", nthread = 12, verbose = 1) %>%
  set_mode("classification")

clf_02 <-
  boost_tree() %>%
  set_engine("C5.0", nthread = 12, verbose = 1) %>%
  set_mode("classification")

clf_03 <-
  logistic_reg() %>%
  set_engine("glmnet", nthread = 12, verbose = 1) %>%
  set_mode("classification")


# auto tune the hyper-parameters ------------------------------------------
# RERUN FROM HERE

# train test split
set.seed(781)
train_test_split <- initial_split(kaggle_train_data, prop = 1/20, strata = 'booked')
data_train <- training(train_test_split)
data_test <- testing(train_test_split)

# the recipe
data_recipe <- data_recipe01
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
folds <- vfold_cv(data_train, v = 10, strata = booked)
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

