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

    cat("\n- Causal Rules Discovery")
    cat("\n  - ITE")
    cat("\n    - Estimator:", getElement(params, "ite_method_dis"))
    if (getElement(params, "ite_method_dis")=='aipw') {
      cat("\n    - Outcome:", getElement(params, "oreg_method_dis"))
    }
    if (!getElement(params, "include_ps_dis")) {
      cat("\n    - Propensity Score:", getElement(params, "ps_method_dis"))
    }

    # hyper params
    if (!is.null(getElement(c(...), "ntrees_rf"))) {
      cat("\n  - Rules Generation")
      if (!is.null(getElement(params, "intervention_vars"))) {
        cat("\n    - Intervention Variables:", getElement(params, "node_size"))
      } else {cat("\n    - Intervention Variables: All")}
      cat("\n    - Number of Trees       :", getElement(params, "ntrees_rf"),
          "RF +",getElement(params, "ntrees_gbm"), "GBM")
      cat("\n    - Node Size             :", getElement(params, "node_size"))
      cat("\n    - Max Nodes             :", getElement(params, "max_nodes"))
      cat("\n  - Filtering")
      cat("\n    - Threshold Decay (Ireelevant):", getElement(params, "max_decay"))
      cat("\n    - Decay Type (Irrelevant)     :", getElement(params, "type_decay"))
      cat("\n    - Threshold (Extreme)         :", getElement(params, "t_ext"))
      cat("\n    - Threshold (Correlated)      :", getElement(params, "t_corr"))
      cat("\n    - Signfifcant (p-value>0.05)  :", getElement(params, "filter_cate"))
      cat("\n  - Causal Rules Discovery")
      cat("\n    - Stability Selection:", getElement(params, "stability_selection"))
      if (getElement(params, "stability_selection")){
        cat("\n    - Cutoff             :", getElement(params, "cutoff"))
        cat("\n    - PFER               :", getElement(params, "pfer"))
      }
    }

    cat("\n- CATE Inference")
    cat("\n  - ITE")
    cat("\n    - ITE Estimator:", getElement(params, "ite_method_inf"))
    if (getElement(params, "ite_method_inf")=='aipw') {
      cat("\n    - Outcome:", getElement(params, "oreg_method_inf"))
    }
    if (!getElement(params, "include_ps_inf")) {
      cat("\n    - Propensity Score:", getElement(params, "ps_method_inf"))
    }
    cat("\n  - CATE")
    cat("\n    - Estimator:", getElement(params, "cate_method"))
  }

  cat("\n\nResults\n")
  if (!is.null(getElement(c(...), "filter_cate"))) {
    if (getElement(params, "filter_cate")) {
      cat("- Heterogeneity:", object[['M']][['Causal (significant)']], "(significant) Causal Rules discovered\n", sep=" ")
    }
  } else {
      cat("- Heterogeneity:", object[['M']][['Causal']], "Causal Rules discovered\n", sep=" ")
  }

  cat("- CATE         :\n")
  print(object[["CATE"]])
}
