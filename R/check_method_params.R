#' @title
#' Check method-related parameters
#'
#' @description
#' Checks method-related parameters.
#'
#' @param y The observed response vector.
#' @param X_names The observed covariates names.
#' @param ite The estimated ITE vector.
#' @param params The list of parameters required to run the method functions.
#'
#' @keywords internal
#'
#' @return
#' A modified input `params`. A list of parameters that might be changed during
#' the checks.
#'
check_method_params <- function(y, X_names, ite, params) {

  # Honest Splitting Parameters Check ------------------------------------------
  ratio_dis <- getElement(params, "ratio_dis")
  if (length(ratio_dis) == 0) {
    ratio_dis <- 0.5
  } else {
    if (!inherits(ratio_dis, "numeric") | (ratio_dis < 0) | (ratio_dis > 1)) {
      stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
    }
  }
  params[["ratio_dis"]] <- ratio_dis


  # ITE Estimation Parameters Check --------------------------------------------
  ite_method_dis <- tolower(getElement(params, "ite_method_dis"))
  if (length(ite_method_dis) == 0) {
    ite_method_dis <- "aipw"
  } else {
    if (!(ite_method_dis %in% c("aipw", "oreg",
                                "bart","bcf", "cf", "poisson"))) {
      stop(paste(
        "Invalid ITE method for Discovery Subsample. Please choose ",
        "from the following:\n", "'aipw', oreg', 'bart', ",
        "'bcf', 'cf', or 'poisson'"
      ))
    }
  }
  params[["ite_method_dis"]] <- ite_method_dis

  ite_method_inf <- tolower(getElement(params, "ite_method_inf"))
  if (length(ite_method_inf) == 0) {
    ite_method_inf <- "aipw"
  } else {
    if (!(ite_method_dis %in% c("aipw", "oreg",
                                "bart","bcf", "cf", "poisson"))) {
      stop(paste(
        "Invalid ITE method for Inference Subsample. Please choose ",
        "from the following:\n", "'aipw', oreg', 'bart', ",
        "'bcf', 'cf', or 'poisson'"
      ))
    }
  }
  params[["ite_method_inf"]] <- ite_method_inf


  # Propensity Score Estimation Parameters Check--------------------------------
  include_ps_dis <- toupper(getElement(params, "include_ps_dis"))
  if (ite_method_dis %in% c("bart", "cf")) {
    if (length(include_ps_dis) == 0) {
      include_ps_dis <- TRUE
    } else {
      if (!(include_ps_dis %in% c(TRUE, FALSE))) {
        stop(
          "Please specify 'TRUE' or 'FALSE' for the include_ps_dis argument."
          )
      }
    }
  } else {
    include_ps_dis <- TRUE
  }
  params[["include_ps_dis"]] <- include_ps_dis

  include_ps_inf <- toupper(getElement(params, "include_ps_inf"))
  if (ite_method_inf %in% c("bart", "cf")) {
    if (length(include_ps_inf) == 0) {
      include_ps_inf <- TRUE
    } else {
      if (!(include_ps_inf %in% c(TRUE, FALSE))) {
        stop(
          "Please specify 'TRUE' or 'FALSE' for the include_ps_inf argument."
          )
      }
    }
  } else {
    include_ps_inf <- TRUE
  }
  params[["include_ps_inf"]] <- include_ps_inf

  ps_method_dis <- getElement(params, "ps_method_dis")
  if (!(ite_method_dis %in% c("or", "poisson"))) {
    if (length(ps_method_dis) == 0) {
      ps_method_dis <- "SL.xgboost"
    } else {
      if (!(class(ps_method_dis) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the ps_method_dis
           argument.")
      }
    }
  } else {
    ps_method_dis <- NA
  }
  params[["ps_method_dis"]] <- ps_method_dis

  ps_method_inf <- getElement(params, "ps_method_inf")
  if (!(ite_method_inf %in% c("or", "poisson"))) {
    if (length(ps_method_inf) == 0) {
      ps_method_inf <- "SL.xgboost"
    } else {
      if (!(class(ps_method_inf) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the ps_method_inf
           argument.")
      }
    }
  } else {
    ps_method_inf <- NA
  }
  params[["ps_method_inf"]] <- ps_method_inf

  # Outcome Regression Score Estimation Parameters Check -----------------------
  oreg_method_dis <- getElement(params, "oreg_method_dis")
  if (ite_method_dis %in% c("aipw")) {
    if (length(oreg_method_dis) == 0) {
      oreg_method_dis <- "SL.xgboost"
    } else {
      if (!(class(oreg_method_dis) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the oreg_method_dis
           argument.")
      }
    }
  } else {
    oreg_method_dis <- NA
  }
  params[["oreg_method_dis"]] <- oreg_method_dis

  oreg_method_inf <- getElement(params, "oreg_method_inf")
  if (ite_method_inf %in% c("aipw")) {
    if (length(oreg_method_inf) == 0) {
      oreg_method_inf <- "SL.xgboost"
    } else {
      if (!(class(oreg_method_inf) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the oreg_method_inf
           argument.")
      }
    }
  } else {
    oreg_method_inf <- NA
  }
  params[["oreg_method_inf"]] <- oreg_method_inf

  # Check Outcome Domain -------------------------------------------------------
  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  if (binary_outcome) {
    if (ite_method_dis %in% c("bcf") |
        ite_method_inf %in% c("bcf")) {
      stop(paste("The 'bcf' methods is not ",
                 "applicable to data with binary outcomes. Please select a ",
                 "method from the following: 'aipw',' or', 'cf', or 'bart'"))
    }
  }

  # Offset Parameter Check------------------------------------------------------

  offset <- getElement(params, "offset")
  if ((ite_method_dis == "poisson") | (ite_method_inf == "poisson")) {
    if (length(offset) == 0) {
      offset <- NULL
    } else {
      if (!(offset %in% X_names))
        stop("Offset varible is not observed. Please replace `offset` with an
           observed varibale.")
    }
  } else {
    offset <- NA
  }
  params[["offset"]] <- offset

  # Discard ITE Parameters if ITE estimates are provided
  if (!is.null(ite)) {
    params[["ite_method_dis"]] <- "personalized"
    params[["include_ps_dis"]] <- FALSE
    params[["ps_method_dis"]] <- FALSE
    params[["or_method_dis"]] <- FALSE
    params[["ite_method_inf"]] <- "personalized"
    params[["include_ps_inf"]] <- FALSE
    params[["ps_method_inf"]] <- FALSE
    params[["or_method_inf"]] <- FALSE
  }

  return(params)
}
