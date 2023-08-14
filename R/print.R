#' @title
#' Extend print function for the CRE object
#'
#' @description
#' Prints a brief summary of the CRE object
#'
#' @param x A cre object from running the CRE function.
#' @param verbose Set level of results description details: only results summary
#' 0, results+parameters summary 1, results+parameters+rules summary
#' (default 2).
#' @param ... Additional arguments passed to customize the results description.
#'
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cre <- function(x, verbose = 2, ...) {
  summary(x, verbose, ...)
}


#' @title
#' Print summary of CRE object
#'
#' @description
#' Prints a brief summary of the CRE object
#'
#' @param object A cre object from running the CRE function.
#' @param verbose Set level of results description details: only results summary
#' 0, results+parameters summary 1, results+parameters+rules summary
#' (default 2).
#' @param ... Additional arguments passed to customize the results description.
#'
#' @return
#' A summary of the CRE object
#'
#' @export
summary.cre <- function(object, verbose = 2, ...) {

  object <- unclass(object)
  summary_options <- c(...)

  M <- object[["M"]]
  CATE <- object[["CATE"]]
  hyper_params <- object[["hyper_params"]]
  method_params <- object[["method_params"]]
  params <- c(hyper_params, method_params)

  cat("CAUSAL RULE ENSAMBLE - Summary")

  if (verbose > 0) {
    cat("\n\nModel parameters")

    cat("\n- Pseudo-Outcome estimation")
    if (getElement(params, "ite_method") == "tpoisson") {
      cat("\n. - Estimator       : tlearner")
      cat("\n  - Outcome         : poisson")
      cat("\n  - Offset          :", getElement(params, "offset"))
    } else {
      cat("\n  - Estimator       :", getElement(params, "ite_method"))
      cat("\n  - Outcome         :", getElement(params, "learner_y"))
      cat("\n  - Propensity Score:", getElement(params, "learner_ps"))
    }

    cat("\n- Rules Generation")
    if (!is.null(getElement(params, "intervention_vars"))) {
      cat("\n  - Intervention Variables:", getElement(params,
                                                        "intervention_vars"))
    } else {
      cat("\n  - Intervention Variables: All")
    }
    cat("\n  - Number of Trees       :", getElement(params, "ntrees"))
    cat("\n  - Node Size             :", getElement(params, "node_size"))
    #cat("\n  - Max Rules             :", getElement(params, "max_rules"))
    cat("\n  - Max Depth             :", getElement(params, "max_depth"))
    cat("\n- Filtering")
    cat("\n  - Threshold Decay (Irrelevant):", getElement(params,
                                                            "t_decay"))
    cat("\n  - Threshold (Extreme)         :", getElement(params,
                                                            "t_ext"))
    cat("\n  - Threshold (Correlated)      :", getElement(params,
                                                            "t_corr"))
    cat("\n  - Threshold (p-Value)         :", getElement(params,
                                                            "t_pvalue"))
    if (getElement(params, "stability_selection")) {
      cat("\n- Stability Selection")
      cat("\n  - Cutoff:", getElement(params, "cutoff"))
      cat("\n  - PFER  :", getElement(params, "pfer"))
    } else {
      cat("\n- Stability Selection : FALSE")
    }
  }

  if (verbose > 1) {
    cat("\n\nRules")
    cat("\n  - Intial              :", getElement(M, "initial"))
    cat("\n  - Filter (irrelevant) :", getElement(M, "filter_irrelevant"))
    cat("\n  - Filter (extreme)    :", getElement(M, "filter_extreme"))
    cat("\n  - Filter (correlated) :", getElement(M, "filter_correlated"))
    cat("\n  - Select (LASSO)      :", getElement(M, "select_LASSO"))
    cat("\n  - Select (significant):", getElement(M, "select_significant"))
  }

  cat("\n\nResults\n")
  cat("- CATE Linear Decomposition:\n")
  print(CATE)
}
