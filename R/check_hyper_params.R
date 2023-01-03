#' @title
#' Check input parameters
#'
#' @description
#' Checks consistency in input (hyper) parameters for the cre function.
#'
#' @param X_names The observed covariates names.
#' @param params The list of parameters required to run the function.
#'
#' @keywords internal
#'
#' @return
#' A modified input `params`. A list of parameters that might be changed during
#' the checks.
#'
check_hyper_params <- function(X_names, params){

  # Input params checks --------------------------------------------------------
  ntrees_rf <- getElement(params, "ntrees_rf")
  if (length(ntrees_rf)==0) {
    ntrees_rf <- 100
  } else {
    if (!inherits(ntrees_rf,"numeric")){
      stop("Invalid 'ntrees_rf' input. Please input a number.")
    }
  }
  params[["ntrees_rf"]] <- ntrees_rf

  ntrees_gbm <- getElement(params, "ntrees_gbm")
  if (length(ntrees_gbm)==0) {
    ntrees_gbm <- 0
  } else {
    if (!inherits(ntrees_gbm,"numeric")){
      stop("Invalid 'ntrees_gbm' input. Please input a number.")
    }
  }
  params[["ntrees_gbm"]] <- ntrees_gbm

  node_size <- getElement(params, "node_size")
  if (length(node_size)==0) {
    node_size <- 20
  } else {
    if (!inherits(node_size,"numeric")){
      stop("Invalid 'node_size' input. Please input a number.")
    }
  }
  params[["node_size"]] <- node_size

  max_nodes <- getElement(params, "max_nodes")
  if (length(max_nodes)==0) {
    max_nodes <- 5
  } else {
    if (!inherits(max_nodes,"numeric")){
      stop("Invalid 'max_nodes' input. Please input a number.")
    }
  }
  params[["max_nodes"]] <- max_nodes

  max_depth <- getElement(params, "max_depth")
  if (length(max_depth)==0) {
    max_depth <- 3
  } else {
    if (!inherits(max_depth,"numeric")){
      stop("Invalid 'max_depth' input. Please input a number.")
    }
  }
  params[["max_depth"]] <- max_depth

  max_decay <- getElement(params, "max_decay")
  if (length(max_decay)==0) {
    max_decay <- 0.025
  } else {
    if (!inherits(max_decay,"numeric")){
      stop("Invalid 'max_decay' input. Please input a number.")
    }
  }
  params[["max_decay"]] <- max_decay

  type_decay <- getElement(params, "type_decay")
  if (length(type_decay)==0) {
    type_decay <- 2
  } else {
    if (!inherits(type_decay,"numeric")){
      stop("Invalid 'type_decay' input. Please input a number.")
    }
  }
  params[["type_decay"]] <- type_decay

  replace <- getElement(params, "replace")
  if (length(replace)==0) {
    replace <- TRUE
  } else {
    if (!(replace %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the replace argument.")
    }
  }
  params[["replace"]] <- replace

  t_ext <- getElement(params, "t_ext")
  if (length(t_ext)==0) {
    t_ext <- 0.01
  } else {
    if (!inherits(t_ext,"numeric")){
      stop("Invalid 't_ext' input. Please input a number.")
    }
  }
  params[["t_ext"]] <- t_ext

  t_corr <- getElement(params, "t_corr")
  if (length(t_corr)==0) {
    t_corr <- 1
  } else {
    if (!inherits(t_corr,"numeric")){
      stop("Invalid 't_corr' input. Please input a number.")
    }
  }
  params[["t_corr"]] <- t_corr

  t_pvalue <- getElement(params, "t_pvalue")
  if (length(t_pvalue)==0) {
    t_pvalue <- 0.05
  } else {
    if (!inherits(t_pvalue,"numeric")){
      stop("Invalid 't_pvalue' input. Please input a number.")
    }
  }
  params[["t_pvalue"]] <- t_pvalue

  stability_selection <- getElement(params, "stability_selection")
  pfer <- getElement(params, "pfer")
  cutoff <- getElement(params, "cutoff")
  if (length(stability_selection)==0) {
    stability_selection <- TRUE
    pfer <- 0.9
    cutoff <- 1
  } else {
    if (!(stability_selection %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the stability_selection argument.")
    } else if (stability_selection) {
      if (length(pfer)==0) {
        pfer <-  0.9
      } else {
        if (!inherits(pfer,"numeric")){
          stop("Invalid 'pfer' input. Please input a number.")
        }
      }
      if (length(cutoff)==0) {
        cutoff <-  1
      } else {
        if (!inherits(cutoff,"numeric")){
          stop("Invalid 'cutoff' input. Please input a number.")
        }
      }
    }
  }
  params[["stability_selection"]] <- stability_selection
  params[["pfer"]] <- pfer
  params[["cutoff"]] <- cutoff


  penalty_rl <- getElement(params, "penalty_rl")
  if (length(penalty_rl)==0) {
    penalty_rl <- 1
  } else {
    if (!inherits(penalty_rl,"numeric")){
      stop("Invalid 'penalty_rl' input. Please input a number.")
    }
  }
  params[["penalty_rl"]] <- penalty_rl

  intervention_vars <- getElement(params, "intervention_vars")
  if (length(intervention_vars)==0) {
    intervention_vars <- NULL
  } else {
    for (intervention_var in intervention_vars) {
      if (!(intervention_var %in% X_names))
        stop(paste(intervention_var,
                  "variable is not observed. Please input a set of",
                  "'intervention_vars' included among the observed covariates."))
    }
  }
  params[["intervention_vars"]] <- intervention_vars

  return(params)
}
