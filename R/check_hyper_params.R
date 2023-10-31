#' @title
#' Check input parameters
#'
#' @description
#' Checks consistency in input (hyper) parameters for the `cre` function.
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
check_hyper_params <- function(X_names, params) {

  logger::log_debug("Checking hyper parameters...")

  # Input params checks --------------------------------------------------------
  ntrees <- getElement(params, "ntrees")
  if (length(ntrees) == 0) {
    ntrees <- 20
  } else {
    if (!inherits(ntrees, "numeric")) {
      stop("Invalid 'ntrees' input. Please input a positive integer")
    }
    if (ntrees<1) {
      stop("Invalid 'ntrees' input. Please input a positive integer")
    }
  }
  params[["ntrees"]] <- ntrees

  node_size <- getElement(params, "node_size")
  if (length(node_size) == 0) {
    node_size <- 20
  } else {
    if (!inherits(node_size, "numeric")) {
      stop("Invalid 'node_size' input. Please input a number.")
    }
  }
  params[["node_size"]] <- node_size

  max_rules <- getElement(params, "max_rules")
  if (length(max_rules) == 0) {
    max_rules <- 50
  } else {
    if (!inherits(max_rules, "numeric")) {
      stop("Invalid 'max_rules' input. Please input a number.")
    }
  }
  params[["max_rules"]] <- max_rules

  max_depth <- getElement(params, "max_depth")
  if (length(max_depth) == 0) {
    max_depth <- 3
  } else {
    if (!inherits(max_depth, "numeric")) {
      stop("Invalid 'max_depth' input. Please input a number.")
    }
  }
  params[["max_depth"]] <- max_depth

  t_decay <- getElement(params, "t_decay")
  if (length(t_decay) == 0) {
    t_decay <- 0.025
  } else {
    if (!inherits(t_decay, "numeric")) {
      stop("Invalid 't_decay' input. Please input a number.")
    }
  }
  params[["t_decay"]] <- t_decay

  replace <- getElement(params, "replace")
  if (length(replace) == 0) {
    replace <- TRUE
  } else {
    if (!(replace %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the replace argument.")
    }
  }
  params[["replace"]] <- replace

  t_ext <- getElement(params, "t_ext")
  if (length(t_ext) == 0) {
    t_ext <- 0.01
  } else {
    if (!inherits(t_ext, "numeric")) {
      stop("Invalid 't_ext' input. Please input a number.")
    }
    if (t_ext > 0.5 || t_ext < 0){
      stop(paste("t_ext should be defind in [0, 0.5) range. ",
                 "Current provided value: ", t_ext))
    }
  }


  params[["t_ext"]] <- t_ext

  t_corr <- getElement(params, "t_corr")
  if (length(t_corr) == 0) {
    t_corr <- 1
  } else {
    if (!inherits(t_corr, "numeric")) {
      stop("Invalid 't_corr' input. Please input a number.")
    }
  }
  params[["t_corr"]] <- t_corr

  stability_selection <- getElement(params, "stability_selection")
  if (length(stability_selection) == 0) {
    stability_selection <- "vanilla"
  } else {
    if (!(stability_selection %in% c("error_control", "no","vanilla"))) {
      stop(paste0("Invalid `stability_selection` argument. Please input ",
                  "a value among: {`no`, `vanilla`, `error_control`}."))
    }
  }
  params[["stability_selection"]] <- stability_selection

  cutoff <- getElement(params, "cutoff")
  if (length(cutoff) == 0) {
    cutoff <- 0.9
  } else {
    if (!inherits(cutoff, "numeric")) {
      stop("Invalid 'cutoff' input. Please input a number.")
    }
  }
  params[["cutoff"]] <- cutoff

  pfer <- getElement(params, "pfer")
  if (length(pfer) == 0) {
    pfer <- 1
  } else {
    if (!inherits(pfer, "numeric")) {
      stop("Invalid 'pfer' input. Please input a number.")
    }
  }
  params[["pfer"]] <- pfer

  intervention_vars <- getElement(params, "intervention_vars")
  if (length(intervention_vars) == 0) {
    intervention_vars <- NULL
  } else {
    for (intervention_var in intervention_vars) {
      if (!(intervention_var %in% X_names))
        stop(paste(intervention_var,
              "variable is not observed. Please select a set of",
              "'intervention_vars' included among the observed covariates."))
    }
  }
  params[["intervention_vars"]] <- intervention_vars

  # Check for correct offset input
  offset <- getElement(params, "offset")
  if (!is.null(offset)) {
    if (!(offset %in% X_names)) {
      stop(paste(offset,
                 "variable is not observed. Please select a ",
                 "'offset' included among the observed covariates."))
    }
  }
  params[["offset"]] <- offset

  # Check for correct B input
  B <- getElement(params, "B")
  if (length(B) == 0) {
    B <- 20
  } else {
    if (!inherits(B, "numeric")) {
      stop("Invalid 'B' input. Please input an integer.")
    }
  }
  params[["B"]] <- B

  # Check for correct subsample imput
  subsample <- getElement(params, "subsample")
  if (length(subsample) == 0) {
    subsample <- 0.5
  } else {
    if (!inherits(subsample, "numeric") || (subsample < 0) || (subsample > 1)) {
      stop("Invalid 'subsample' input. Please input a number between 0 and 1.")
    }
  }
  params[["subsample"]] <- subsample

  logger::log_debug("Done with checking hyper parameters.")

  return(params)
}
