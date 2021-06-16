library(tidyverse)
library(tidymodels)
library(lubridate)

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
  require(lubridate)
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


load('preProcessedData/Scenario_01_with_sessions_train.RData')
load('preProcessedData/Scenario_01_with_sessions_test.RData')

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
  step_dummy(all_nominal(), -all_outcomes(), -has_role('ID')) %>%
  step_zv(all_numeric())

clf <-
  boost_tree(trees = 1000) %>%
  set_engine("xgboost",
             verbose = TRUE,
             #tree_method = "gpu_hist",
             nthread = 12) %>%
  set_mode("classification")

wflow <- workflow() %>% 
  add_recipe(data_recipe) %>% 
  add_model(clf)

xgboost_fit <- fit(wflow, kaggle_train_data)

