#' @title
#' Honest Splitting
#'
#' @description
#' Splits data into discovery and inference subsamples.
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The features matrix.
#' @param ratio_dis The ratio of data delegated to the discovery subsample.
#' @param ite The estimated ITE.
#'
#' @return A list containing the discovery and inference subsamples.
#'
#' @keywords internal
#'
honest_splitting <- function(y, z, X, ratio_dis, ite=NULL) {

  n <- check_input_data(y, z, X, ite)
  index <- sample(1:n, round(n*ratio_dis), replace=F)

  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  if (!is.null(ite)) { ite <- as.matrix(ite) }

  discovery <- list(y = y[index],
                    z = z[index],
                    X = X[index,],
                    ite = ite[index,])
  inference <- list(y = y[-index],
                    z = z[-index],
                    X = X[-index,],
                    ite = ite[-index,])

  return(list(discovery = discovery, inference = inference))
}
