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
    cat("\n\nMethods")

    cat("\n- Causal Rules Discovery")
    cat("\n  - ITE")
    cat("\n    - Estimator       :", getElement(params, "ite_method_dis"))
    if (getElement(params, "ite_method_dis") == "aipw") {
      cat("\n    - Outcome         :", getElement(params, "oreg_method_dis"))
    }
    if (getElement(params, "include_ps_dis")) {
      cat("\n    - Propensity Score:", getElement(params, "ps_method_dis"))
    }


    cat("\n  - Rules Generation")
    if (!is.null(getElement(params, "intervention_vars"))) {
      cat("\n    - Intervention Variables:", getElement(params, "node_size"))
    } else {
      cat("\n    - Intervention Variables: All")
    }
    cat("\n    - Number of Trees       :", getElement(params, "ntrees_rf"),
        "RF +", getElement(params, "ntrees_gbm"), "GBM")
    cat("\n    - Node Size             :", getElement(params, "node_size"))
    cat("\n    - Max Nodes             :", getElement(params, "max_nodes"))
    cat("\n    - Max Depth             :", getElement(params, "max_depth"))
    cat("\n  - Filtering")
    cat("\n    - Threshold Decay (Irrelevant):", getElement(params,
                                                            "max_decay"))
    cat("\n    - Decay Type (Irrelevant)     :", getElement(params,
                                                            "type_decay"))
    cat("\n    - Threshold (Extreme)         :", getElement(params,
                                                            "t_ext"))
    cat("\n    - Threshold (Correlated)      :", getElement(params,
                                                            "t_corr"))
    cat("\n    - Threshold (p-Value)         :", getElement(params,
                                                            "t_pvalue"))
    cat("\n  - Causal Rules Discovery")
    cat("\n    - Penalty Rules Length:", getElement(params, "penalty_rl"))
    if (getElement(params, "stability_selection")) {
      cat("\n    - Stability Selection")
      cat("\n      - Cutoff:", getElement(params, "cutoff"))
      cat("\n      - PFER  :", getElement(params, "pfer"))
    } else {
      cat("\n    - Stability Selection : FALSE")
    }


    cat("\n- CATE Inference")
    cat("\n  - ITE")
    cat("\n    - Estimator       :", getElement(params, "ite_method_inf"))
    if (getElement(params, "ite_method_inf") == "aipw") {
      cat("\n    - Outcome         :", getElement(params, "oreg_method_inf"))
    }
    if (getElement(params, "include_ps_inf")) {
      cat("\n    - Propensity Score:", getElement(params, "ps_method_inf"))
    }
    cat("\n  - CATE")
    cat("\n    - Estimator:", getElement(params, "cate_method"))
  }

  if (verbose > 1) {
    cat("\n\nRules")
    cat("\n  - Intial              :", getElement(M, "Initial"))
    cat("\n  - Filter (irrelevant) :", getElement(M, "Filter (irrelevant)"))
    cat("\n  - Filter (extreme)    :", getElement(M, "Filter (extreme)"))
    cat("\n  - Filter (correlated) :", getElement(M, "Filter (correlated)"))
    cat("\n  - Select (LASSO)      :", getElement(M, "Select (LASSO)"))
    cat("\n  - Select (significant):", getElement(M, "Select (significant)"))
  }

  cat("\n\nResults\n")
  cat("- CATE Linear Deccomposition:\n")
  print(CATE)
}
