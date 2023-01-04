#' @title
#' Estimate the Individual Treatment Effect (ITE) using Outcome Regression
#' (OREG)
#'
#' @description
#' Estimates the Individual Treatment Effect (ITE) using Outcome Regression
#' given a response vector, a treatment vector, and a features matrix.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#'
#' @return
#' A vector of ITE estimates
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
