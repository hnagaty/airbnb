#' Calculates the nDCG relevance score for a single observation
#' @param truth a single value for the ground truth class
#' @param estimate a vector for the predicted classes
#'
#' @return the nDCG score as a single numeric value
#' @export
#'
#' @examples
#' > pred <- c('FR', 'US', 'IT')
#' > ndcg_single('FR', pred)
#' [1] 1
#' > ndcg_single('US', pred)
#' [1] 0.6309298
ndcg_single <- function(truth, estimate) {
  relevance <- estimate == truth
  i <- seq(1,length(estimate))
  sum((2^relevance-1)/log2(i+1))
}


#' Calculates the nDCG score for a group of predicted values
#' y_preds is a matrix of probabilities
#' @param truth The true labels, as a vector
#' @param estimate The estimate class probabilities, as a matrix with dimensions nxk,
#'                 where n is the no. of observations, and k is the number of classes
#' @param k The rank to calculate nDCG at
ndcg<- function(truth, estimate, k) {
  ordered_preds <- apply(estimate, 1, order, decreasing = TRUE)
  topk_preds <- ordered_preds[1:k,] %>%
    matrix(nrow = k, byrow = FALSE)
  l <- split(topk_preds, col(topk_preds))
  scores <- mapply(ndcg_single, as.integer(truth), l, USE.NAMES = FALSE)
  return(mean(scores))
}

estimate <- matrix(c(0.3, 0.3, 0.4,
                     0.1, 0.1, 0.8,
                     0.3, 0.6, 0.1,
                     0.4, 0.3, 0.3,
                     0.8, 0.2, 0.0), nrow = 5, byrow = TRUE)
estimate
truth1 <- c(3, 3, 2, 1, 1)
truth2 <- c(1, 2, 1, 2, 2)
truth3 <- c(2, 3, 3, 3, 3)
ndcg(truth1, estimate, 3)
ndcg(truth2, estimate, 3)
ndcg(truth3, estimate, 3)

ndcg_vec <- function(truth, estimate, estimator = NULL, k = 5) {
  
  estimator <- finalize_estimator(truth, estimator)
  
  #' Calculates the nDCG relevance score for a single observation
  #' @param truth a single value for the ground truth class
  #' @param estimate a vector for the predicted classes
  #'
  #' @return the nDCG score as a single numeric value
  #' @export
  #'
  #' @examples
  #' > pred <- c('FR', 'US', 'IT')
  #' > ndcg_single('FR', pred)
  #' [1] 1
  #' > ndcg_single('US', pred)
  #' [1] 0.6309298
  ndcg_single <- function(truth, estimate) {
    relevance <- estimate == truth
    i <- seq(1,length(estimate))
    sum((2^relevance-1)/log2(i+1))
  }
  
  
  #' Calculates the nDCG score for a group of predicted values
  #' y_preds is a matrix of probabilities
  #' @param truth The true labels, as a vector
  #' @param estimate The estimate class probabilities, as a matrix with dimensions nxk,
  #'                 where n is the no. of observations, and k is the number of classes
  #' @param k The rank to calculate nDCG at
  ndcg_imp <- function(truth, estimate, k) {
    ordered_preds <- apply(estimate, 1, order, decreasing = TRUE)
    topk_preds <- ordered_preds[1:k,] %>%
      matrix(nrow = k, byrow = FALSE)
    l <- split(topk_preds, col(topk_preds))
    scores <- mapply(ndcg_single, as.integer(truth), l, USE.NAMES = FALSE)
    return(mean(scores))
  }
  
  metric_vec_template(
    metric_impl = ndcg_imp,
    truth = truth,
    estimate = estimate,
    k = k,
    cls = "factor",
    estimator = estimator,
    ...
  )
} 

ndcg_vec(truth1, estimate, 3)
