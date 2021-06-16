
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(lubridate)
library(treesnip) # lightgbm
library(discrim) # naive bayes in tidymodels
library(rlist) # dealing with lists
library(themis) # for upsampling
require(tictoc)
require(stringr)
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




TuneFitFactory <- function(clf,
                           data_recipe,
                           train_data,
                           clf_grid = NULL,
                           tune_n = 10,
                           save = TRUE,
                           ...) {
  i <- 0
  TuneFitModel <- function(clf,
                           data_recipe,
                           train_data,
                           clf_grid = NULL,
                           tune_n = 10,
                           save = TRUE,
                           parallel_backend = "None",
                           parallel_workers = 3,
                           ...) {
    require(tictoc)
    require(stringr)
    require(lubridate)
    
    i <<- i + 1
    message(str_glue("Now in run: {i}"))
    
    
    tic()
    set.seed(945)
    val_split <- validation_split(train_data, prop = 3/4, strata = "country_destination")
    
    wflow <- 
      workflow() %>% 
      add_model(clf) %>% 
      add_recipe(data_recipe)
    
    if (is.null(clf_grid)) {
      clf_grid <- grid_max_entropy(wflow, size = tune_n)
    }
    
    message(str_glue("Now fitting model: {clf$name}:{clf$engine} using recipe: {data_recipe$name} along {nrow(clf_grid)} different parameters"))
    
    # stopCluster()
    if (parallel_backend == "doParallel") {
      library(doParallel)
      all_cores <- detectCores(logical = FALSE)
      cl <- makeForkCluster(parallel_workers)
      registerDoParallel(cl)
      message(str_glue("Using {parallel_backend} backend with {getDoParWorkers()} cores"))
      
      grid_resamples <- wflow %>% 
        tune_grid(val_split,
                  grid = clf_grid,
                  control = control_grid(verbose = TRUE,
                                         pkgs = "doParallel",
                                         parallel_over = "everything")
        )
      stopCluster(cl)
    }
    
    else if (parallel_backend == "doFuture") {

      library(doFuture)
      registerDoFuture()
      plan(multisession, workers = parallel_workers)
      message(str_glue("Using {parallel_backend} backend with {parallel_workers} cores"))
      
      grid_resamples <- wflow %>% 
        tune_grid(val_split,
                  grid = clf_grid,
                  control = control_grid(verbose = TRUE,
                                         pkgs = "doFuture",
                                         parallel_over = "everything")
        )
      plan(sequential)
    }
    
    else if (parallel_backend == "doMC") {
      library(doMC)
      registerDoMC(parallel_workers)
      message(str_glue("Using {parallel_backend} backend with {parallel_workers} cores"))
      
      grid_resamples <- wflow %>% 
        tune_grid(val_split,
                  grid = clf_grid,
                  control = control_grid(verbose = TRUE,
                                         pkgs = "doMC",
                                         parallel_over = "everything")
        )
      stopCluster(cl)
    }
    else {
      message(str_glue("Not using a parallel backend"))
      grid_resamples <- wflow %>% 
        tune_grid(val_split,
                  grid = clf_grid,
                  control = control_grid(verbose = TRUE,
                                         allow_par = FALSE))
    }
    
    
    timing <- toc(quiet = TRUE)
    elapsed <- timing$toc - timing$tic
    message(str_interp("Done fitting model ${clf$name} with recipe: ${data_recipe$name} in $[.2f]{elapsed/60} minutes."))
    
    
    result <- (list(resamples = grid_resamples,
                    fit_time = elapsed,
                    fit_datetime = now(),
                    clf_name = clf$name,
                    recipe_name = data_recipe$name,
                    model_info = list(...)))
    if (save) {
      saveRDS(result, gsub(" ", "", paste0("singleFitResult_", clf$name, "_",
                                           data_recipe$name, "_", today(), ".rds")))
    }
    return(result)
  }
  
}



ExtractModelInfo <- function(model_out) {
  best <- collect_metrics(model_out$resamples) %>% 
    group_by(.metric) %>% slice_max(mean, n = 1, with_ties = FALSE) %>% 
    select(.metric, mean) %>% 
    rename(best_mean = mean) %>% 
    pivot_wider(names_from = .metric, values_from = best_mean, names_prefix = "best_")
  
  combined <- collect_metrics(model_out$resamples) %>% 
    group_by(.metric) %>% 
    rename(mean_est = mean) %>% 
    summarise(n = n(), mean = mean(mean_est), sd = sd(mean_est), .groups = 'drop') %>% 
    pivot_wider(names_from = .metric, values_from = c(mean, sd))
  
  best_auc <- select_best(model_out$resamples, 'roc_auc')
  best_acc <- select_best(model_out$resamples, 'accuracy')
  best_metrics <- tibble(best_auc_model = best_auc, best_acc_model = best_acc)
  
  fit_time <- model_out$fit_time
  return(bind_cols(model_out$model_info, clf = model_out$clf_name, recipe = model_out$recipe_name,
                   best, combined, best_metrics, fit_time = fit_time))
}

plotMetrics <- function(data, var) {
  data %>%
    ggplot(aes(x = recipe, y = .data[[paste0("mean_",var)]])) +
    geom_point() +
    geom_point(aes(y = .data[[paste0("best_",var)]]), col = "blue") +
    facet_wrap(clf ~ .) +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(name = str_to_title(var), label = scales::percent) +
    geom_errorbar(aes(ymin = .data[[paste0("mean_",var)]] - .data[[paste0("sd_",var)]],
                      ymax = .data[[paste0("mean_",var)]] + .data[[paste0("sd_",var)]]))
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
#' @param y_preds 
#' @param mode if normal then normal accuracy. If binary then binary accuracy "NDF vs NON-NDF"
#' @return numeric, the accuracy score
#' @export
#'
#' @examples
CalcSoftProbAcc <- function(y, y_preds, mode = "normal") {
  orderedPreds <- apply(y_preds, 1, order, decreasing = TRUE)
  topPred <- orderedPreds[1,]
  if (mode == "normal") {
    accuracy <- sum(topPred == y)/length(y)
    return(accuracy)
  }
  else if (mode == "binary") {
    accuracy <- sum((topPred == 1 & y == 1) | (topPred != 1 & y != 1)) / length(y)
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

#' print.tidy_spec()
#' Prints a tidy_spec, it prints the name & the info
#' @param spec 
#'
#' @return
#' @export
#'
#' @examples
print.tidy_spec <- function(spec) {
  spec <- unlist(spec, recursive = FALSE)
  for (i in seq(1, length(spec))) {
    print(str_glue("{format(i, width = 3, justify = 'right')} ==> {spec[[i]]$name}; {format(spec[[i]]$info)}"))
    }
}

summary.tune_spec <- function(tune_list) {
  recps <- tune_list %>% pluck("data_recipe") %>% map_chr("name") 
  clfs <- tune_list %>% pluck("clf") %>% map_chr("name")
  tune_params <- tune_list %>% pluck("clf_grid") %>% map_int(ncol)
  tune_size <- tune_list %>% pluck("clf_grid") %>% map_int(nrow)
  
  spec_summary <- tibble(recipe = recps,
                         clf = clfs,
                         num_params = tune_params,
                         tune_size = tune_size)
  spec_summary
}

# I) Common & initialization -------------------------------------------------

# load non-processed data
load('preProcessedData/Scenario_01_with_sessions_train.RData')
load('preProcessedData/Scenario_01_with_sessions_test.RData')

nclass <- 12
dest_levels <- levels(kaggleTrain$country_destination)

# cyclic encoding for the dates into month sin & cos
kaggle_train_data <- EncodeCyclicDates(kaggleTrain, 'date_account_created', 'date_first_active')
kaggle_test_data <- EncodeCyclicDates(kaggleTest, 'date_account_created', 'date_first_active')

# train test split
train_test_split <- initial_split(kaggle_train_data, prop = 4/5, strata = 'country_destination')
data_train <- training(train_test_split)
data_test <- testing(train_test_split)


# II) Explore models & recipes --------------------------------------------

# II) a - Define the recipes ------------------------------------------------------
# the default recipe, median impute
data_recipe_01 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_medianimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
data_recipe_01$name <- "Std Median Impute"

# mean impute instead of median impute
data_recipe_02 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
data_recipe_02$name <- "Std Mean Impute"

# mean impute, dummy after center/scale
data_recipe_03 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID'))
data_recipe_03$name <- "Std Mean Impute 2"
data_recipe_03$info <- "mean impute, dummy after center/scale"

# knn impute
# VERY EXPENSIVE
data_recipe_04 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_knnimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
data_recipe_04$name <- "Std KNN Impute"

# mean impute + pca
data_recipe_05 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>% 
  step_pca(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID'))
data_recipe_05$name <- "PCA + Mean Impute"

# mean impute + radial pca
data_recipe_06 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>% 
  step_kpca_rbf(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID'))
data_recipe_06$name <- "RPCA + Mean Impute"

# mean impute + quadratic pca
data_recipe_07 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>% 
  step_kpca_poly(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID'))
data_recipe_07$name <- "QPCA + Mean Impute"

# no pca, no center, no scale, smote up-sampling
data_recipe_08 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking, id) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>% 
  step_smote(country_destination)
data_recipe_08$name <- "Semi Minimal + SMOTE"

# no pca, no center, no scale, smote up-sampling
data_recipe_09 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking, id) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>% 
  step_normalize(all_numeric()) %>% 
  step_smote(country_destination)
data_recipe_09$name <- "Std + SMOTE"

# no pca, no center, no scale, rose up-sampling
data_recipe_09b <- # rose upsamples only binary outcomes
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking, id) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>% 
  step_rose(country_destination)
data_recipe_09b$name <- "Std + ROSE"

# no pca, no center, no scale
data_recipe_10 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_meanimpute(all_numeric()) %>%
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) 
data_recipe_10$name <- "Semi Minimal"
data_recipe_10$info <- "No PCA, no normalise"

# no pca, no center, no imputation
data_recipe_11 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) 
data_recipe_11$name <- "Minimal"
data_recipe_11$info <- "Minimal"

# no pca, no center, no imputation
# THIS RECIPE IS INVALID
# As SMOTE can't work with missing values
data_recipe_12 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>% 
  step_smote(country_destination)
data_recipe_12$name <- "Minimal + SMOTE"
data_recipe_12$info <- "Minimal with SMOTE"

# no pca, no center, no imputation, no dummy
data_recipe_13 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors())
data_recipe_13$name <- "Extra Minimal"

# no pca, no center, no imputation, no dummy
# THIS RECIPE IS INVALID
# As SMOTE can't work with missing values
data_recipe_14 <- 
  recipe(country_destination ~ ., data = kaggle_train_data) %>% 
  update_role(id, new_role = "ID") %>%
  step_rm(date_first_booking) %>% 
  step_unknown(all_nominal(), -has_role('ID'), -all_outcomes()) %>% 
  step_relevel(all_nominal(), -has_role('ID'), -all_outcomes(), ref_level = "unknown") %>%
  step_other(all_nominal(), -has_role('ID'), -all_outcomes(), other = 'Others') %>% 
  step_zv(all_predictors()) %>%
  step_smote(country_destination)
data_recipe_14$name <- "Extra Minimal + SMOTE"



#  II) b - Define the models -------------------------------------------------------
# define custom parameter objects
booster <- function() {
  new_qual_param(
    type = "character",
    values = c("gbtree", "dart"), # "gblinear"
    default = "gbtree",
    label = c(booster = "Which booster to use in XGBoost"),
  )
}
booster()

max_delta_step <- function(range = c(0L, 10L), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    values = c(0, 1, 5, 10),
    label = c(max_delta_step = "Use this for imbalanced classes"),
    finalize = NULL
  )
}
max_delta_step()

#' XGBoost
clf_01 <-
  boost_tree(tree_depth = tune(),
             trees = 300,
             learn_rate = 0.01,
             sample_size = tune(),
             mtry = tune(),
             min_n = tune()
             ) %>%
  set_engine("xgboost",
             max_delta_step = tune(),
             booster = tune(),
             nthread = 11,
             verbose = 2,
             verbosity = 2,
             tree_method = "hist") %>%
  set_mode("classification")
clf_01$name <- 'XGBoost'
params <- clf_01 %>% 
  parameters() %>% 
  update(mtry = predictor_prop())
params <- params %>% 
  update(max_delta_step = max_delta_step(),
         booster = booster())
params

grid_clf_01 <- grid_max_entropy(params, size = 18)
grid_clf_01 <- grid_clf_01 %>%
  mutate(max_delta_step = case_when(
    max_delta_step < 4 ~ 0,
    max_delta_step < 8 ~ 5,
    max_delta_step >= 4 ~ 10,
  ))
grid_clf_01

#' LightGBM
clf_02 <-
  boost_tree(tree_depth = tune(),
             trees = 300,
             learn_rate = 0.01,
             sample_size = tune(),
             mtry = tune(),
             min_n = tune()) %>%
  set_engine("lightgbm", nthread = 11) %>%
  set_mode("classification")
clf_02$name <- 'LightGBM'

params <- clf_02 %>% 
  parameters() %>% 
  update(mtry = predictor_prop()) %>% 
  update(sample_size = predictor_prop())
grid_clf_02 <- grid_max_entropy(params, size = 12)
grid_clf_02

#' C5.0
clf_03 <-
  boost_tree(trees = tune(),
             sample_size = tune(),
             min_n = tune()) %>%
  set_engine("C5.0") %>%
  set_mode("classification")
clf_03$name <- 'C5.0'
clf_03 %>% translate()
params <- clf_03 %>% 
  parameters() %>% 
  update(sample_size = predictor_prop())
grid_clf_03 <- grid_max_entropy(params, size = 10)
grid_clf_03

#' Multinomial Regression
clf_04 <-
  multinom_reg(penalty = tune(),
               mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
clf_04$name <- 'glmnet'
params <- clf_04 %>% 
  parameters() 
grid_clf_04 <- grid_max_entropy(params, size = 5)
grid_clf_04

#' SVM
clf_05 <-
  svm_poly(cost = tune(),
           degree = tune(),
           scale_factor = tune()) %>%
  set_engine("kernlab", verbose = 1) %>%
  set_mode("classification")
clf_05$name <- 'SVM'
params <- clf_05 %>% 
  parameters() 
grid_clf_05 <- grid_max_entropy(params, size = 5)
grid_clf_05

#' Naive Bayes
clf_06 <-
  naive_Bayes(smoothness = tune(),
              Laplace = tune())%>%
  set_engine("klaR") %>%
  set_mode("classification")
clf_06$name <- 'klaR'
clf_06 %>% translate()
params <- clf_06 %>% 
  parameters() 
params
grid_clf_06 <- grid_max_entropy(params, size = 5)
grid_clf_06

#' CatBoost
clf_07 <-
  boost_tree(tree_depth = tune(),
             trees = 300,
             learn_rate = 0.01,
             sample_size = tune(),
             mtry = tune(),
             min_n = tune()) %>%
  set_engine("catboost", nthread = 11, verbose = 1) %>%
  set_mode("classification")
clf_07$name <- 'CatBoost'
clf_07 %>% translate()
params <- clf_07 %>% 
  parameters() %>% 
  update(mtry = predictor_prop()) %>% 
  update(sample_size = predictor_prop())
grid_clf_07 <- grid_max_entropy(params, size = 12)
grid_clf_07


#  II) c - Construct models & recipes ----------------------------------------------------------------
recipe_list = tibble(lst(data_recipe_01,
                         data_recipe_02,
                         data_recipe_03,
                         # data_recipe_04, # knn is very slow
                         data_recipe_05,
                         data_recipe_06,
                         data_recipe_07,
                         data_recipe_08,
                         data_recipe_09,
                         data_recipe_10,
                         data_recipe_11))
colnames(recipe_list) <- "data_recipe"
class(recipe_list) <- c("tidy_spec", "tbf_df", "tbl", "data.frame")
recipe_list

clf_list <- lst(clf_01, clf_02, clf_03, clf_04, clf_06) %>% 
  tibble()
colnames(clf_list) <- "clf"
class(clf_list) <- c("tidy_spec", "tbf_df", "tbl", "data.frame")
clf_list

grid_list <- lst(grid_clf_01, grid_clf_02, grid_clf_03, grid_clf_04, grid_clf_06) %>% 
  tibble()
colnames(grid_list) <- "clf_grid"
class(grid_list) <- c("tidy_grid", "tbf_df", "tbl", "data.frame")
grid_list

# cross product of all classifiers & recipes
cross_list <- bind_cols(clf_list, grid_list)
cross_list <- expand_grid(cross_list, recipe_list)
class(cross_list) <- c("tune_spec", "tbl_df", "tbl", "data.frame")
class(cross_list)
cross_list

summary(cross_list) %>% print(n = 100)


# cross product of a single recipe and all classifiers
cross_rec_list <- expand_grid(tibble(clf_list, grid_list),
                              data_recipe = list(data_recipe_01))
cross_rec_list

# cross product of all recipes and a single classifier
cross_clf_list <- expand_grid(recipe_list,
                              tibble(clf = list(clf_04),
                                     clf_grid = list(grid_clf_04)))
class(cross_clf_list) <- c("tune_spec", "tbl_df", "tbl", "data.frame")
summary(cross_clf_list)

# custom list
custom_list <- list(# XGBoost
                    list(data_recipe_01, clf_01, grid_clf_01, FALSE),
                    list(data_recipe_02, clf_01, grid_clf_01, FALSE),
                    list(data_recipe_05, clf_01, grid_clf_01, FALSE),
                    list(data_recipe_06, clf_01, grid_clf_01, FALSE),
                    list(data_recipe_08, clf_01, grid_clf_01, FALSE),
                    list(data_recipe_10, clf_01, grid_clf_01, FALSE),
                    list(data_recipe_11, clf_01, grid_clf_01, FALSE),
                    # C5.0
                    list(data_recipe_01, clf_03, grid_clf_03, "doFuture"),
                    list(data_recipe_02, clf_03, grid_clf_03, "doFuture"),
                    list(data_recipe_05, clf_03, grid_clf_03, "doFuture"),
                    #list(data_recipe_06, clf_03, grid_clf_03, "None"),
                    list(data_recipe_08, clf_03, grid_clf_03, "None"),
                    #list(data_recipe_10, clf_03, grid_clf_03, "doFuture"),
                    list(data_recipe_11, clf_03, grid_clf_03, "doFuture"),
                    # LR
                    list(data_recipe_01, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_02, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_03, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_04, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_05, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_06, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_07, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_08, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_09, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_10, clf_04, grid_clf_04, "doFuture"),
                    list(data_recipe_11, clf_04, grid_clf_04, "doFuture"),
                    # Naive Bayes
                    list(data_recipe_01, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_02, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_03, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_04, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_05, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_06, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_07, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_08, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_09, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_10, clf_06, grid_clf_06, "doFuture"),
                    list(data_recipe_11, clf_06, grid_clf_06, "doFuture"),
                    # LightGBM
                    list(data_recipe_10, clf_02, grid_clf_02, FALSE),
                    list(data_recipe_11, clf_02, grid_clf_02, FALSE),
                    # CatBoost
                    list(data_recipe_10, clf_07, grid_clf_07, FALSE),
                    list(data_recipe_11, clf_07, grid_clf_07, FALSE),
                    list(data_recipe_13, clf_07, grid_clf_07, FALSE)) %>%
                      transpose(.names = c("data_recipe", "clf", "clf_grid", "parallel_backend")) %>% 
                      as_tibble(.name_repair = "minimal")
class(custom_list) <- c("tune_spec", "tbl_df", "tbl", "data.frame")
print(summary(custom_list), n = 100)
custom_list

#  II) d - Train the models -----------------------------------------------

# Should I use full set or just a sample for testing
full_set = FALSE # make this FALSE if I want to test the code on a small subset of the data
data_fraction = 0.4

if (!full_set) {
  val_split = validation_split(data_train, 1 - data_fraction,
                               strata = "country_destination")
  fit_data <- assessment(val_split$splits[[1]])
} else { 
  fit_data <- data_train
}

# For test: Fit a single recipe & a single classifier
FitModel <- TuneFitFactory()
fit_results <- FitModel(clf_03,  clf_grid = grid_clf_03,
                        data_recipe_01,
                        fit_data, parallel_backend = "doFuture")
ExtractModelInfo(fit_results)

#' Fit a list of recipes, classifiers & tune grids
#' =======================================
#' THIS TAKES A LONG TIME. DAYS OR WEEKS
#' =======================================
custom_list %>% summary() %>% print(n = 100)
tic()
FitModel <- TuneFitFactory()
fit_results <- pmap(custom_list, possibly(FitModel, otherwise = NULL),
                    train_data = fit_data)
timing <- toc()
elapsed <- timing$toc - timing$tic
print(str_interp("Done fitting ${nrow(custom_list)} models in $[.2f]{elapsed/60} minutes."))
fit_results %>% str(max.level = 1)
saveRDS(fit_results, "fitResults_20201219.rds")



# II) e - Visualize the results ----------------------------------------------------------

fit_results <- readRDS("fitResults_20201219.rds")
models_metrics <- map_dfr(fit_results, possibly(ExtractModelInfo, otherwise = NULL))
models_metrics
plotMetrics(models_metrics, "roc_auc")
plotMetrics(models_metrics, "accuracy")
models_metrics %>% 
  ggplot(aes(x = clf, y = fit_time)) +
  geom_col(fill = "orange") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10()



# II) f - Check the best workflow--------------------------------------------

#' The best is xgboost + semi minimal pre-processing
#' That's clf_01 & data_recipe_10
best_wflow <- fit_results %>% pluck(7, 1)

best_results <- best_wflow %>% 
  collect_metrics %>% 
  filter(.metric == "roc_auc") %>% 
  select(-.metric, -.estimator, -n, -std_err, -.config) %>% 
  rename(auc = mean)

bs_resample <- bootstraps(best_results)
bs_resample

recp <- recipe(auc ~ ., data = best_results) %>% 
  step_dummy(all_nominal()) %>% 
  step_normalize(all_predictors()) 
recp

regr <- rand_forest() %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "impurity")
regr

wflow <- 
  workflow() %>% 
  add_model(regr) %>% 
  add_recipe(recp)
wflow

res <- wflow %>% 
  fit_resamples(bs_resample)
res %>% collect_metrics()

model_fit <- wflow %>% 
  fit(best_results)

model_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit") %>% 
  vip()


# III) Auto tune the hyper-parameters for the model of choice------------------------------------------

# III) a - Use Bayesian Optimisation --------------------------------------

# the recipe
final_recipe <- data_recipe_10
final_recipe

# the classifier
final_clf <- boost_tree(
          tree_depth = tune(),
          trees = tune(),
          learn_rate = tune(),
          sample_size = tune(),
          mtry = tune(),
          min_n = tune()) %>%
  set_engine("xgboost",
           max_delta_step = tune(),
           booster = tune(),
           nthread = 11,
           verbosity = 2,
           tree_method = "hist",
           eval_metric = "merror") %>%
  set_mode("classification")
final_clf

# the parameters
params <- final_clf %>% 
  parameters() %>% 
  update(mtry = predictor_prop())
params <- params %>% 
  update(max_delta_step = max_delta_step(),
         booster = booster())
params

# the workflow
wflow <- 
  workflow() %>% 
  add_model(final_clf) %>% 
  add_recipe(final_recipe)
wflow

# resamples
set.seed(12)
folds <- vfold_cv(data_train, v = 10, strata = country_destination)
folds

# TUNE BAYES
set.seed(12)
search_res <-
  wflow %>% 
  tune_bayes(
    resamples = folds,
    # To use non-default parameter ranges
    param_info = params,
    # Generate five at semi-random to start
    initial = 10,
    iter = 50,
    # How to measure performance?
    metrics = metric_set(roc_auc),
    control = control_bayes(no_improve = 30, verbose = TRUE)
  ) 
  
  
#' BELOW IS OTHER
#' 

# classifier
autotune_clf <-
  boost_tree(tree_depth = tune(),
             trees = tune(),
             learn_rate = tune(),
             sample_size = tune(),
             mtry = tune(),
             min_n = tune()
  ) %>%
  set_engine("xgboost",
             max_delta_step = tune(),
             nthread = 12,
             verbose = TRUE,
             tree_method = "gpu_hist") %>%
  set_mode("classification")
autotune_clf

# parameters
params <- autotune_clf %>% 
  parameters() %>% 
  update(mtry = predictor_prop()) %>% 
  update(max_delta_step = max_delta_step())
params

grid_clf <- grid_max_entropy(params, size = 8)
grid_clf <- grid_clf %>%
  mutate(max_delta_step = case_when(
    max_delta_step < 4 ~ 0,
    max_delta_step < 8 ~ 5,
    max_delta_step >= 4 ~ 10,
  ))
grid_clf




# cv-folds & resamples
set.seed(345)
folds <- vfold_cv(data_train, v = 4, strata = country_destination)
folds

# III) b - Hyper-parameter tuning ==== LONG TIME ====
# IGNORE if tuning is already done
#' ---------------------------------

#require(doParallel)
#all_cores <- detectCores(logical = FALSE)
#cl <- makeForkCluster(all_cores/4)
#registerDoParallel(cl)
#message(str_glue("Using parallel backend with {getDoParWorkers()} parallel cores"))

tic("Hyper-parameter tuning", quiet = FALSE)
clf_resamples <- wflow %>% 
  tune_grid(
    resamples = folds,
    grid = grid_clf,
    control = control_grid(verbose = TRUE)
  )
toc()

clf_resamples %>% 
  collect_metrics()

clf_resamples %>% 
  show_best(metric = "accuracy")

#stopCluster(cl)
filename <- "ClassifierResamples_XGBoost_20201019.rds"
saveRDS(clf_resamples, filename)
clf_resamples <- readRDS(filename)

best_clf <- clf_resamples %>% 
  select_best('accuracy')
best_clf

clf_resamples %>% 
  select_by_one_std_err(trees, metric = "accuracy")

clf_resamples %>% 
  select_by_pct_loss(trees, metric = "accuracy")


clf_resamples %>%
  collect_metrics() %>% 
  ggplot(aes(mean, std_err)) +
  geom_text(aes(label = .config)) +
  geom_point(color = 'red', size = 2, alpha = 0.65) +
  facet_wrap(~ .metric)


#' END OF IGNORE
#' ---------------------------------

# Final fit
final_wf <- 
  wflow %>% 
  finalize_workflow(best_clf)
final_wf

final_fit <- final_wf %>% 
  fit(data_test)
final_fit

last_fit <- final_wf %>% 
  last_fit(train_test_split)
last_fit

last_fit_filename <- "XGBoost_LastFit_20201019.rds"
saveRDS(last_fit, last_fit_filename)
last_fit <- readRDS(last_fit_filename)

final_wf_fit <- final_fit %>%
  pull(.workflow) %>%
  pluck(1)
final_wf_fit$trained <- TRUE
final_wf_fit

final_wf_fit %>% 
  pull_workflow_fit() %>% 
  vip()

final_fit %>%
  collect_metrics()

train_preds <- final_fit %>%
  collect_predictions()
test_preds2 <- predict(final_wf_fit, data_test)

final_fit %>%
  collect_predictions() %>% 
  roc_curve(country_destination, .pred_NDF:.pred_AU) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(lty = 2, color = "gray80", size = 1.2) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1, color = "green") +
  facet_wrap(~.level, ncol = 4) +
  coord_equal()

final_fit %>%
  collect_predictions() %>% 
  conf_mat(country_destination, .pred_class) %>%
  autoplot(type = "heatmap")

final_fit %>%
  collect_predictions() %>% 
  ppv(country_destination, .pred_class)
  


# IX) Dummy ---------------------------------------------------------------

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



# Extra -------------------------------------------------------------------

fit_result <- TuneFitModel(clf_01, data_recipe01, data_train, tune_n = 10,
                                      name = 'XGBoost & SMOTE',
                                      desc = 'Finally XGBoost and SMOTE')
print(ExtractModelInfo(fit_result), width = Inf)

fit_result_upsampling <- TuneFitModel(clf_02, data_recipe05, data_train, tune_n = 10,
                           name = 'XGBoost & SMOTE',
                           desc = 'Finally XGBoost and SMOTE')
print(ExtractModelInfo(fit_result_upsampling), width = Inf)

# Apply on Kaggle train data ----------------------------------------------
# Write the out file name here
kaggle_filename <- "2020Aug_DataSc01_WithSessions02_allData_tidymodels_96CatBoost.csv"
kaggle_preds <- predict(final_clf, kaggle_test_data, type = 'prob')
kaggle_out <- format_preds(kaggle_preds, kaggleTest$id)
write.csv(kaggle_out, kaggle_filename, quote = FALSE, row.names = FALSE)
