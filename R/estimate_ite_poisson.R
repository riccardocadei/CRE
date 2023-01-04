#' @title
#' Estimate the Individual Treatment Effect (ITE) using Poisson Regression
#'
#' @description
#' Estimates the Individual Treatment Effect using Poisson Regression given a
#' response vector, a treatment vector, and a features matrix.
#'
#' @param y An observed response vector.
#' @param z A treatment vector.
#' @param X A features matrix.
#' @param X_names A vector that includes the names of the covariates.
#' @param offset A name of the covariate to use as offset (i.e. 'x1') to model
#' the corresponding outcome rate. `NULL` to model directly the outcome counts
#' without offset.
#'
#' @return
#' A vector of ITE estimates.
#'
#' @keywords internal
#'
estimate_ite_poisson <- function(y, z, X, X_names, offset){
  if (!is.null(offset)) {
    if (!(offset %in% X_names)){
      stop("Offset varible is not observed. Please replace `offset` with an
           observed varibale.")
    } else {
      X_names = X_names[-which(X_names == offset)]
      names(X)[names(X) == offset] <- 'offset'
      y_treated <- data.frame(y = y[z==1])
      X_treated <- as.data.frame(X[z==1,])
      y_control <- data.frame(y = y[z==0])
      X_control <- as.data.frame(X[z==0,])
      data_treated <- cbind(y_treated, X_treated)
      data_control <- cbind(y_control, X_control)
      formula = as.formula(paste("y ~ ", paste(X_names, collapse= "+")))
      temp1 <- stats::glm(formula,
                          data = data_treated,
                          offset = log(offset),
                          family = stats::poisson(link = "log"))
      temp0 <- stats::glm(formula,
                          data = data_control,
                          offset = log(offset),
                          family = stats::poisson(link = "log"))
    }
  } else {
    y_treated <- data.frame(y = y[z==1])
    X_treated <- as.data.frame(X[z==1,])
    y_control <- data.frame(y = y[z==0])
    X_control <- as.data.frame(X[z==0,])
    data_treated <- cbind(y_treated, X_treated)
    data_control <- cbind(y_control, X_control)
    temp1 <- stats::glm(y ~ .,
                        data = data_treated,
                        family = stats::poisson(link = "log"))
    temp0 <- stats::glm(y ~ .,
                        data = data_control,
                        family = stats::poisson(link = "log"))
  }
  y1hat <- stats::predict(temp1, as.data.frame(X))
  y0hat <- stats::predict(temp0, as.data.frame(X))
  ite <- y1hat - y0hat
  return(ite)
}
