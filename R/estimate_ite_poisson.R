#' @title
#' Estimate the Individual Treatment Effect using Poisson Regression
#'
#' @description
#' Estimates the Individual Treatment Effect using Poisson Regression given a
#' response vector, a treatment vector, and a features matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param X_names the names of the covariates
#' @param include_offset whether to include an offset (i.e. model outcome rate)
#' or not (i.e. model outcome counts)
#' @param offset_name the name of the covariate to use as offset (i.e. 'x1')
#'
#' @return
#' a vector of ITE estimates
#'
#' @keywords internal
#'
#'
estimate_ite_poisson <- function(y, z, X, X_names, include_offset, offset_name){
  if (include_offset) {
    X_names = X_names[-which(X_names == offset_name)]
    names(X)[names(X) == offset_name] <- 'offset'
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
