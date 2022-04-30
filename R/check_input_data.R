#' @title
#' Check Input Data
#'
#' @description
#' Conducts sanity checks for the input data
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The features matrix.
#'
#' @keywords internal
#'
#' @return
#' Number of data samples.
#'
#'
check_input_data <- function(y, z, X){

  #---------------- Input data checks ------------------------------------------
  # type
  if (!is.vector(y) & !is.numeric(y)){
    stop("Observed response vector (y) input values should be a numerical vector")
  }

  if (!is.vector(z) & !is.numeric(z)){
    stop("Treatment (z) input values should be a numerical vector.")
  }

  ## size
  y_size <- length(y)
  z_size <- length(z)

  if (y_size != z_size){
    stop(paste("Response and and treatment vectors should be the same size. ",
               "Current values: ", y_size, ", ", z_size))
  }

  covars_size <- dim(X)

  if (covars_size[1] != y_size){
    stop(paste("Covariates (X) data.frame has different number of",
               "observation than response and treatment vectors.",
               "Current values: ", covars_size, ", ", y_size))
  }

  #TODO: convert all variables to a data.table.
  if (!(class(y)[[1]] %in% c("numeric", "integer", "matrix"))){
    stop("Invalid 'y' input. Please input a numeric vector.")
  }

  if (length(unique(z)) != 2){
    stop("Invalid 'z' input. Please input a binary treatment vector.")
  }

  if (length(class(X)) == 1) {
    if (!(class(X) %in% c("data.frame", "matrix"))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }

  if (length(class(X)) == 2) {
    if (!(identical(class(X), c("matrix", "array")))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }

  X_classes <- apply(X, 2, class)

  if (!all(X_classes %in% c("integer", "numeric"))){
    stop(paste("Invalid 'X' input. Please input a matrix or data frame",
               " of numeric categorical variables"))
  }
  #-----------------------------------------------------------------------------
  invisible(y_size)
}
