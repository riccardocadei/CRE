#' @title
#' Extend print function for the CRE object
#'
#' @description
#' Prints a brief summary of the CRE object
#'
#' @param x A CRE object from running the CRE function
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cre <- function(x, ...) {
  print(x$CATE)
}



#' @title
#' Print summary of CRE object
#'
#' @description
#' Prints a brief summary of the CRE object
#'
#' @param object A cre object from running the CRE function
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' A summary of the CRE object
#'
#' @export
summary.cre <- function(object, ...) {

  object <- unclass(object)
  params <- c(...)

  cat("CAUSAL RULE ENSAMBLE - Summary")

  # params
  if (!is.null(getElement(c(...), "ratio_dis"))) {
    cat("\n\nMethods")

    cat("\n- Discovery:")
    cat("\n  - ITE Estimator : ", getElement(params, "ite_method_dis"))
    if (getElement(params, "ite_method_dis")=='aipw') {
      cat("\n  - Outcome Estim.: ", getElement(params, "oreg_method_dis"))
    }
    if (!getElement(params, "include_ps_dis")) {
      cat("\n  - PS Estimator  : ", getElement(params, "ps_method_dis"))
    } else { cat("\n  - PS Estimator  : None") }

    # hyper params
    if (!is.null(getElement(c(...), "ntrees_rf"))) {
      cat("\n  - Rule Generation:")
      cat("\n    - Number of Trees: ", getElement(params, "ntrees_rf"),
          "RF +",getElement(params, "ntrees_gbm"), "GBM")
      cat("\n    - Node Size      : ", getElement(params, "node_size"))
      cat("\n    - Max Nodes      : ", getElement(params, "max_nodes"))
      cat("\n    - Common Support : ", getElement(params, "t"))
      cat("\n    - Threshold      : ", getElement(params, "q"))
      cat("\n  - Rule Regularization:")
      cat("\n    - Stability Selection: ",
          getElement(params, "stability_selection"))
      cat("\n    - Per-Family E.R.    : ",
          getElement(params, "pfer_val"))
    }

    cat("\n- Inference:")
    cat("\n  - ITE Estimator : ", getElement(params, "ite_method_inf"))
    if (getElement(params, "ite_method_inf")=='aipw') {
      cat("\n  - Outcome Estim.: ", getElement(params, "oreg_method_inf"))
    }
    if (!getElement(params, "include_ps_inf")) {
      cat("\n  - PS Estimator  : ", getElement(params, "ps_method_inf"))
    } else { cat("\n  - PS Estimator  : None") }
    cat("\n  - CATE Estimator: ", getElement(params, "cate_method"))
  }

  cat("\n\nResults\n")
  cat("- Heterogeneity:", object[['M']][['Filter 4 (LASSO)']], "(significant) Causal Rules discovered\n", sep=" ")
  cat("- CATE         :\n")
  print(object[["CATE"]])
}
