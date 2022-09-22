#' @title
#' Check method-related parameters
#'
#' @description
#' Checks method-related parameters.
#'
#' @param y The observed response vector.
#' @param params The list of parameters required to run the method functions.
#'
#' @keywords internal
#'
#' @return
#' A modified input `params`. A list of parameters that might be changed during
#' the checks.
#'
check_method_params <- function(y, params){

  # Input params checks --------------------------------------------------------
  if (!inherits(getElement(params, "ratio_dis"), "numeric") |
      (getElement(params, "ratio_dis") < 0) |
      (getElement(params, "ratio_dis") > 1)){
    stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
  }

  ite_method_dis <- tolower(getElement(params, "ite_method_dis"))
  params[["ite_method_dis"]] <- ite_method_dis
  if (!(ite_method_dis %in% c("ipw", "sipw", "aipw", "oreg", "bart",
                              "bcf", "cf", "poisson"))) {
    stop(paste("Invalid ITE method for Discovery Subsample. Please choose ",
               "from the following:\n","'ipw', 'sipw', 'aipw', oreg', 'bart', ",
               "'bcf', 'cf', or 'poisson'"))
  }

  ite_method_inf <- tolower(getElement(params, "ite_method_inf"))
  params[["ite_method_inf"]] <- ite_method_inf
  if (!(ite_method_inf %in% c("ipw", "sipw", "aipw", "oreg", "bart",
                              "bcf", "cf", "poisson"))) {
    stop(paste("Invalid ITE method for Inference Subsample. Please choose ",
               "from the following: 'ipw', 'sipw', 'aipw', 'oreg', 'bart', ",
               "'bcf', 'cf', or 'poisson'"))
  }

  # Check for correct propensity score estimation inputs -----------------------
  include_ps_dis <- toupper(getElement(params, "include_ps_dis"))
  if (ite_method_dis %in% c("bart", "cf")) {
    if (!(include_ps_dis %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_dis argument.")
    }
  } else {
    include_ps_dis <- NA
  }
  params[["include_ps_dis"]] <- include_ps_dis

  include_ps_inf <- toupper(getElement(params, "include_ps_inf"))
  if (ite_method_inf %in% c("bart", "cf")) {
    if (!(include_ps_inf %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_inf argument.")
    }
  } else {
    include_ps_inf <- NA
  }
  params[["include_ps_inf"]] <- include_ps_inf

  if (!(getElement(params, "ite_method_dis") %in% c("or", "poisson"))) {
    if (!(class(getElement(params, "ps_method_dis")) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the ps_method_dis argument.")
    }
  } else {
    ps_method_dis <- NA
    params[["ps_method_dis"]] <- ps_method_dis
  }

  if (!(getElement(params, "ite_method_inf") %in% c("or", "poisson"))) {
    if (!(class(getElement(params, "ps_method_inf")) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the ps_method_inf argument.")
    }
  } else {
    ps_method_inf <- NA
    params[["ps_method_inf"]] <- ps_method_inf
  }

  # Check for outcome regression score estimation inputs -----------------------
  if (getElement(params, "ite_method_dis") %in% c("aipw")) {
    if (!(class(getElement(params, "oreg_method_dis")) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the oreg_method_dis argument.")
    }
  } else {
    oreg_method_dis <- NA
    params[["oreg_method_dis"]] <- oreg_method_dis
  }

  if (getElement(params, "ite_method_inf") %in% c("aipw")) {
    if (!(class(getElement(params, "oreg_method_inf")) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the oreg_method_inf argument.")
    }
  } else {
    oreg_method_inf <- NA
    params[["oreg_method_inf"]] <- oreg_method_inf
  }

  # Determine outcome type
  is_y_binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  params[["is_y_binary"]] <- is_y_binary
  if (is_y_binary) {
    if (getElement(params, "ite_method_dis") %in% c("bcf", "ipw", "sipw") |
        getElement(params, "ite_method_inf") %in% c("bcf", "ipw", "sipw")) {
      stop(paste("The 'ipw', 'sipw', and 'bcf' methods are not ",
                 "applicable to data with binary outcomes.Please select a ",
                 "method from the following: 'or', 'cf', or 'bart'"))
    }
  }

  # Check for correct offset input
  if ((getElement(params, "ite_method_dis") == "poisson") |
      (getElement(params, "ite_method_inf") == "poisson") |
      (getElement(params, "cate_method") == "poisson")) {
    if (getElement(params, "include_offset") == TRUE) {
      if (is.na(getElement(params, "offset_name"))) {
        stop(paste("Invalid offset_name input. Please specify an offset_name ",
                   "if you wish to include an offset."))
      }
    } else {
      offset_name <- NA
      params[["offset_name"]] <- offset_name
    }
  } else {
    include_offset <- FALSE
    offset_name <- NA
    params[["include_offset"]] <- include_offset
    params[["offset_name"]] <- offset_name
  }

  # Check for correct offset input
  if ((getElement(params, "ite_method_dis") == "poisson") |
      (getElement(params, "ite_method_inf") == "poisson") |
      (getElement(params, "cate_method") == "poisson")) {
    if (getElement(params, "include_offset") == TRUE) {
      if (is.na(getElement(params, "offset_name"))) {
        stop(paste("Invalid offset_name input. Please specify an offset_name ",
                   "if you wish to include an offset."))
      }
    } else {
      offset_name <- NA
      params[["offset_name"]] <- offset_name
    }
  } else {
    include_offset <- FALSE
    offset_name <- NA
    params[["include_offset"]] <- include_offset
    params[["offset_name"]] <- offset_name
  }

  # Check for correct CATE estimation inputs -----------------------

  if (!(getElement(params, "cate_method") %in% c("poisson", "DRLearner",
                                                 "bart-baggr", "cf-means",
                                                 "linreg"))) {
    stop(paste("Invalid CATE method for Inference Subsample. Please choose from ",
               "the following: 'poisson', 'DRLearner', 'bart-baggr', ",
               "'cf-means', or 'linreg'"))
  }

  if (getElement(params, "cate_method") == "DRLearner") {
    if (!(class(getElement(params, "cate_SL_library")) %in% c("character", "list"))) {
      stop("Please specify a string or list for the cate_SL_library argument.")
    }
  } else {
    cate_SL_library <- NA
    params[["cate_SL_library"]] <- cate_SL_library
  }

  if (getElement(params, "cate_method") == "bart-baggr") {
    if (!(getElement(params, "ite_method_inf") %in% c("bart"))) {
      stop(paste("Please choose 'bart' for ite_method_inf ",
                 "if you wish to use 'bart-baggr' as the cate_method"))
    }
  }

  if (getElement(params, "cate_method") == "cf-means") {
    if (!(getElement(params, "ite_method_inf") %in% c("cf", "bcf"))) {
      stop(paste("Please choose 'cf', or 'bcf' for ite_method_inf ",
                 "if you wish to use 'cf-means' as the cate_method"))
    }
  }

  if (!(getElement(params, "filter_cate") %in% c(TRUE, FALSE))) {
    stop("Invalid 'filter_cate' input. Please specify TRUE or FALSE.")
  }

  return(params)
}
