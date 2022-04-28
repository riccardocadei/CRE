

#' @title
#' Check input parameters
#'
#' @description
#' Checks input parameters for the cre function.
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The covariate matrix.
#' @param params The list of parameters required to run the function.
#'
#' @return
#' A list of params that might be changed during checks.
#' @export
#'
check_input <- function(y, z, X, params){


  # Input data checks ----------------------------------------------------------
  check_input_data(y, z, X)

  # ## type
  # if (!is.vector(y) & !is.numeric(y)){
  #   stop("Observed response vector (y) input values should be a numerical vector")
  # }
  #
  # if (!is.vector(z) & !is.numeric(z)){
  #   stop("Treatment (z) input values should be a numerical vector.")
  # }
  #
  # # if (!is.data.frame(X)){
  # #   stop("Covariates (x) input values should be a data.frame.")
  # # }
  #
  # ## size
  # y_size <- length(y)
  # z_size <- length(z)
  #
  # if (y_size != z_size){
  #   stop(paste("Response and and treatment vectors should be the same size. ",
  #              "Current values: ", y_size, ", ", z_size))
  # }
  #
  # covars_size <- dim(X)
  #
  # if (covars_size[1] != y_size){
  #   stop(paste("Covariates (X) data.frame has different number of",
  #              "observation than response and treatment vectors.",
  #              "Current values: ", covars_size, ", ", y_size))
  # }
  #
  # if (!(class(y) %in% c("numeric", "integer"))){
  #   stop("Invalid 'y' input. Please input a numeric vector.")
  # }
  #
  # if (length(unique(z)) != 2){
  #   stop("Invalid 'z' input. Please input a binary treatment vector.")
  # }
  #
  # if (length(class(X)) == 1) {
  #   if (!(class(X) %in% c("data.frame", "matrix"))) {
  #     stop("Invalid 'X' input. Please input a matrix or data frame.")
  #   }
  # }
  #
  # if (length(class(X)) == 2) {
  #   if (!(identical(class(X), c("matrix", "array")))) {
  #     stop("Invalid 'X' input. Please input a matrix or data frame.")
  #   }
  # }
  #
  # X_classes <- apply(X, 2, class)
  #
  # if (!all(X_classes %in% c("integer", "numeric"))){
  #   stop(paste("Invalid 'X' input. Please input a matrix or data frame",
  #              " of numeric categorical variables"))
  # }

  # Input params checks --------------------------------------------------------


  # todo: dplyr between is not necessary.
  if (class(getElement(params, "ratio_dis")) != "numeric" | !dplyr::between(getElement(params, "ratio_dis"), 0, 1)){
    stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
  }

  if (class(getElement(params, "ntrees_rf")) != "numeric"){
    stop("Invalid 'ntrees_rf' input. Please input a number.")
  }

  if (class(getElement(params, "ntrees_gbm")) != "numeric"){
    stop("Invalid 'ntrees_gbm' input. Please input a number.")
  }

  if (class(getElement(params, "min_nodes")) != "numeric"){
    stop("Invalid 'min_nodes' input. Please input a number.")
  }

  if (class(getElement(params, "max_nodes")) != "numeric"){
    stop("Invalid 'max_nodes' input. Please input a number.")
  }

  if (class(getElement(params, "t")) != "numeric"){
    stop("Invalid 't' input. Please input a number.")
  }

  if (class(getElement(params, "q")) != "numeric"){
    stop("Invalid 'q' input. Please input a number.")
  }

  ite_method_dis <- tolower(getElement(params, "ite_method_dis"))
  params[["ite_method_dis"]] <- ite_method_dis
  if (!(ite_method_dis %in% c("ipw", "sipw", "aipw", "or", "bart", "xbart",
                              "bcf", "xbcf", "cf", "poisson"))) {
    stop(paste("Invalid ITE method for Discovery Subsample. Please choose ",
               "from the following:\n","'ipw', 'sipw', 'aipw', or', 'bart', ",
               "'xbart', 'bcf', 'xbcf', 'cf', or 'poisson'"))
  }

  ite_method_inf <- tolower(getElement(params, "ite_method_inf"))
  params[["ite_method_inf"]] <- ite_method_inf
  if (!(ite_method_inf %in% c("ipw", "sipw", "aipw", "or", "bart", "xbart",
                              "bcf", "xbcf", "cf", "poisson"))) {
    stop(paste("Invalid ITE method for Inference Subsample. Please choose ",
               "from the following: 'ipw', 'sipw', 'aipw', 'or', 'bart', ",
               "'xbart', 'bcf', 'xbcf', 'cf', or 'poisson'"))
  }

  # Check for correct propensity score estimation inputs -----------------------
  include_ps_dis <- toupper(getElement(params, "include_ps_dis"))
  if (ite_method_dis %in% c("bart", "xbart", "cf")) {
    if (!(include_ps_dis %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_dis argument.")
    }
  } else {
    include_ps_dis <- NA
  }
  params[["include_ps_dis"]] <- include_ps_dis

  include_ps_inf <- toupper(getElement(params, "include_ps_inf"))
  if (ite_method_inf %in% c("bart", "xbart", "cf")) {
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
    if (!(class(getElement(params, "or_method_dis")) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the or_method_dis argument.")
    }
  } else {
    or_method_dis <- NA
    params[["or_method_dis"]] <- or_method_dis
  }

  if (getElement(params, "ite_method_inf") %in% c("aipw")) {
    if (!(class(getElement(params, "or_method_inf")) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the or_method_inf argument.")
    }
  } else {
    or_method_inf <- NA
    params[["or_method_inf"]] <- or_method_inf
  }


  # Determine outcome type
  is_y_binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  params[["is_y_binary"]] <- is_y_binary
  if (is_y_binary) {
    if (getElement(params, "ite_method_dis") %in% c("bcf", "xbcf", "ipw", "sipw") |
        getElement(params, "ite_method_inf") %in% c("bcf", "xbcf", "ipw", "sipw")) {
      stop(paste("The 'ipw', 'sipw', 'bcf', and 'xbcf' methods are not ",
                 "applicable to data with binary outcomes.Please select a ",
                 "method from the following: 'or', 'cf', 'bart', or 'xbart'"))
    }
  }

  # Check for correct rules_method input
  rules_method <- tolower(getElement(params, "rules_method"))
  if (is_y_binary) {
    if (!(rules_method %in% c("conservative", "anticonservative"))) {
      stop(paste("Invalid rules_method input. Please specify 'conservative' ",
                 "or 'anticonservative'."))
    }
  } else {
    rules_method <- NA
  }

  params[["rules_method"]] <- rules_method



  # Check for correct offset input
  if ((getElement(params, "ite_method_dis") == "poisson") | (getElement(params, "ite_method_inf") == "poisson") | (getElement(params, "cate_method") == "poisson")) {
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
    if (!(getElement(params, "ite_method_inf") %in% c("bart", "xbart"))) {
      stop(paste("Please choose 'bart' or 'xbart' for ite_method_inf ",
                 "if you wish to use 'bart-baggr' as the cate_method"))
    }
  }

  if (getElement(params, "cate_method") == "cf-means") {
    if (!(getElement(params, "ite_method_inf") %in% c("cf", "bcf", "xbcf"))) {
      stop(paste("Please choose 'cf', 'bcf', or 'xbcf' for ite_method_inf ",
                 "if you wish to use 'cf-means' as the cate_method"))
    }
  }

  if (!(getElement(params, "filter_cate") %in% c(TRUE, FALSE))) {
    stop("Invalid 'filter_cate' input. Please specify TRUE or FALSE.")
  }


  return(params)


}
