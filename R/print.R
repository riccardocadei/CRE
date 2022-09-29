#' @title
#' Extend print function for the CRE object
#'
#' @description
#' Prints a brief summary of the CRE object
#'
#' @param cre_results A CRE object from running the CRE function
#' @param method_params Method parameters.
#' @param hyper_params Hyper parameters.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cre <- function(cre_results, method_params = NULL, hyper_params = NULL) {

  cre_results <- unclass(cre_results)

  cat("CAUSAL RULE ENSAMBLE - Summary")

  if (!is.null(method_params)) {
    cat("\n\nMethods")

    cat("\n- Discovery:")
    cat("\n  - ITE Estimator : ", getElement(method_params, "ite_method_dis"))
    if (getElement(method_params, "ite_method_dis")=='aipw') {
      cat("\n  - Outcome Estim.: ", getElement(method_params, "oreg_method_dis"))
    }
    if (!getElement(method_params, "include_ps_dis")) {
      cat("\n  - PS Estimator  : ", getElement(method_params, "ps_method_dis"))
    } else { cat("\n  - PS Estimator  : None") }

    if (!is.null(hyper_params)) {
      cat("\n  - Rule Generation:")
      cat("\n    - Number of Trees: ", getElement(hyper_params, "ntrees_rf"),
          "RF +",getElement(hyper_params, "ntrees_gbm"), "GBM")
      cat("\n    - Node Size      : ", getElement(hyper_params, "node_size"))
      cat("\n    - Max Nodes      : ", getElement(hyper_params, "max_nodes"))
      cat("\n    - Common Support : ", getElement(hyper_params, "t"))
      cat("\n    - Threshold      : ", getElement(hyper_params, "q"))
      cat("\n  - Rule Regularization:")
      cat("\n    - Stability Selection: ",
          getElement(hyper_params, "stability_selection"))
      cat("\n    - Per-Family E.R.    : ",
          getElement(hyper_params, "pfer_val"))
    }

    cat("\n- Inference:")
    cat("\n  - ITE Estimator : ", getElement(method_params, "ite_method_inf"))
    if (getElement(method_params, "ite_method_inf")=='aipw') {
      cat("\n  - Outcome Estim.: ", getElement(method_params, "oreg_method_inf"))
    }
    if (!getElement(method_params, "include_ps_inf")) {
      cat("\n  - PS Estimator  : ", getElement(method_params, "ps_method_inf"))
    } else { cat("\n  - PS Estimator  : None") }
    cat("\n  - CATE Estimator: ", getElement(method_params, "cate_method"))
  }

  cat("\n\nResults\n")
  cat("- Heterogeneity:", cre_results[['M']], "(significant) Causal Rules discovered\n", sep=" ")
  if (cre_results[["M"]]==0) {
    cat("- ATE          : ")
    cat(cre_results[["ATE"]])
  } else {
    cat("- CATE         :\n")
    print(cre_results[["CATE"]])

    # plot results
    attr(cre_results, "class") <- "cre"
    plot(cre_results, method_params)
  }
}
