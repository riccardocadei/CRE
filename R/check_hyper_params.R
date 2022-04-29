

#' @title
#' Check input parameters
#'
#' @description
#' Checks input parameters for the cre function.
#'
#' @param params The list of parameters required to run the function.
#'
#' @return
#' A list of params that might be changed during checks.
#' @export
#'
check_hyper_params <- function(params){

  # Input params checks --------------------------------------------------------
  if (class(getElement(params, "ntrees_rf")) != "numeric"){
    stop("Invalid 'ntrees_rf' input. Please input a number.")
  }

  if (class(getElement(params, "ntrees_gbm")) != "numeric"){
    stop("Invalid 'ntrees_gbm' input. Please input a number.")
  }

  if (class(getElement(params, "min_nodes")) != "numeric"){
    stop("Invalid 'min_nodes' input. Please input a number.")
  }

  if (class(getElement(params, "max_nodes")) != "numeric"){
    stop("Invalid 'max_nodes' input. Please input a number.")
  }

  if (class(getElement(params, "t")) != "numeric"){
    stop("Invalid 't' input. Please input a number.")
  }

  if (class(getElement(params, "q")) != "numeric"){
    stop("Invalid 'q' input. Please input a number.")
  }
}
