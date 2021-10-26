#' @title
#' Split Data
#'
#' @description
#' Splits data into discovery and inference subsamples.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ratio_dis the ratio of data delegated to the discovery subsample
#'
#' @return a list containing the discovery and inference subsamples
#'
#' @export
#'
split_data <- function(y, z, X, ratio_dis) {

  n <- check_input_data(y, z, X)

  total_sample <- cbind(y, z, X)

  index <- sample(1:n, round(n*ratio_dis), replace=F)

  discovery <- total_sample[index,]
  inference <- total_sample[-index,]

  return(list(discovery = discovery, inference = inference))
}
