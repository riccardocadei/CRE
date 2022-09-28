#' @title
#' Extend print function for the CRE object
#'
#' @description
#' Prints a brief summary of the CRE object
#'
#' @param cre_results A cre object from running the CRE function
#' @param method_params Method parameters.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cre <- function(cre_results, method_params) {

  cre_results <- unclass(cre_results)

  cat("CAUSAL RULE ENSAMBLE - Summary \n\n")

  cat("Methods")
  cat("\n- Discovery:")
  cat("\n  - ITE Estimator : ", getElement(method_params, "ite_method_dis"))
  cat("\n  - PS Estimator  : ", getElement(method_params, "ps_method_dis"))
  cat("\n- Inference:")
  cat("\n  - ITE Estimator : ", getElement(method_params, "ite_method_inf"))
  cat("\n  - PS Estimator  : ", getElement(method_params, "ps_method_inf"))
  cat("\n  - CATE Estimator: ", getElement(method_params, "cate_method"))

  # TO DO: add important params and hyper-params summary

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
