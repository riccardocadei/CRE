#' @title
#' Check input parameters
#'
#' @description
#' Checks input parameters for the cre function.
#'
#' @param params The list of parameters required to run the function.
#'
#' @keywords internal
#'
#' @return
#' No return value. This function is called for side effects.
#'
check_hyper_params <- function(params){

  # Input params checks --------------------------------------------------------
  if (!inherits(getElement(params, "ntrees_rf"),"numeric")){
    stop("Invalid 'ntrees_rf' input. Please input a number.")
  }

  if (!inherits(getElement(params, "ntrees_gbm"),"numeric")){
    stop("Invalid 'ntrees_gbm' input. Please input a number.")
  }

  if (!inherits(getElement(params, "node_size"),"numeric")){
    stop("Invalid 'node_size' input. Please input a number.")
  }

  if (!inherits(getElement(params, "max_nodes"),"numeric")){
    stop("Invalid 'max_nodes' input. Please input a number.")
  }

  if (!inherits(getElement(params, "t"),"numeric")){
    stop("Invalid 't' input. Please input a number.")
  }

  if (!inherits(getElement(params, "q"),"numeric")){
    stop("Invalid 'q' input. Please input a number.")
  }

  if (!(getElement(params, "stability_selection") %in% c(TRUE, FALSE))) {
    stop("Please specify 'TRUE' or 'FALSE' for the stability_selection argument.")
  }

  if (getElement(params, "stability_selection")) {
    if (!inherits(getElement(params, "pfer_val"),"numeric")){
      stop("Invalid 'pfer_val' input. Please input a number.")
    }
  } else {
    params[["pfer_val"]] <- NA
  }
}
