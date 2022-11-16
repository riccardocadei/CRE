#' @title
#' Honest Splitting
#'
#' @description
#' Splits data into discovery and inference subsamples.
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The features matrix.
#' @param ratio_dis the ratio of data delegated to the discovery subsample
#'
#' @return A list containing the discovery and inference subsamples
#'
#' @keywords internal
#'
honest_splitting <- function(y, z, X, ratio_dis) {

  n <- check_input_data(y, z, X)
  index <- sample(1:n, round(n*ratio_dis), replace=F)

  discovery <- list(y = y[index],
                    z = z[index],
                    X = X[index,])
  inference <- list(y = y[-index],
                    z = z[-index],
                    X = X[-index,])

  return(list(discovery = discovery, inference = inference))
}
