#' @title
#' Estimate the Individual Treatment Effect using Outcome Regression
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Outcome
#' Regression given a response vector, a treatment vector, and a features matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return a vector of ITE estimates
#'
#' @keywords internal
#'

estimate_ite_oreg <- function(y, z, X) {

  if (!requireNamespace("BART", quietly = TRUE)) {
    stop(
      "Package \"BART\" must be installed to use this function.",
      call. = FALSE
    )
  }

  temp1 <- BART::wbart(x.train = X[z==1,],
                       y.train = y[z==1],
                       x.test = X[z==0,])

  y1hat_control <- temp1$yhat.test.mean

  temp0 <- BART::wbart(x.train = X[z==0,],
                       y.train = y[z==0],
                       x.test = X[z==1,])

  y0hat_treated <- temp0$yhat.test.mean

  ite <- rep(NA, times = length(y))
  ite[z==0] <- y1hat_control - y[z==0]
  ite[z==1] <- y[z==1] - y0hat_treated
  return(ite)
}
