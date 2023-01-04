#' @title
#' Honest splitting
#'
#' @description
#' Splits data into discovery and inference sub-samples.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param ratio_dis A double number indicating the ratio of data delegated to
#' the discovery sub-sample.
#' @param ite A vector of estimated ITE.
#'
#' @return
#' A list containing the discovery and inference sub-samples.
#'
#' @keywords internal
#'
honest_splitting <- function(y, z, X, ratio_dis, ite = NULL) {

  n <- check_input_data(y, z, X, ite)
  index <- sample(1:n, round(n * ratio_dis), replace = FALSE)

  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  if (!is.null(ite)) {
    ite <- as.matrix(ite)
  }

  discovery <- list(y = y[index],
                    z = z[index],
                    X = X[index, ],
                    ite = ite[index, ])
  inference <- list(y = y[-index],
                    z = z[-index],
                    X = X[-index, ],
                    ite = ite[-index, ])

  return(list(discovery = discovery, inference = inference))
}
