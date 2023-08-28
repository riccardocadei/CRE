#' @title
#' Check method-related parameters
#'
#' @description
#' Checks method-related parameters.
#'
#' @param y The observed response vector.
#' @param ite The estimated ITE vector.
#' @param params The list of parameters required to run the method functions.
#'
#' @keywords internal
#'
#' @return
#' A modified input `params`. A list of parameters that might be changed during
#' the checks.
#'
check_method_params <- function(y, ite, params) {

  logger::log_debug("Checking method parameters...")

  # Honest Splitting Parameters Check ------------------------------------------
  ratio_dis <- getElement(params, "ratio_dis")
  if (length(ratio_dis) == 0) {
    ratio_dis <- 0.5
  } else {
    if (!inherits(ratio_dis, "numeric") || (ratio_dis < 0) || (ratio_dis > 1)) {
      stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
    }
  }
  params[["ratio_dis"]] <- ratio_dis


  # ITE Estimation Parameters Check --------------------------------------------
  ite_method <- tolower(getElement(params, "ite_method"))
  if (length(ite_method) == 0) {
    ite_method <- "aipw"
  } else {
    if (!(ite_method %in% c("aipw", "slearner", "tlearner", "xlearner",
                                "bart", "cf", "tpoisson"))) {
      stop(paste(
        "Invalid ITE method. Please choose from the following:",
        "\n", "'aipw', 'bart', 'slearner','tlearner', ",
        "'xlearner', 'cf', or 'tpoisson'"
      ))
    }
  }
  params[["ite_method"]] <- ite_method


  # Propensity Score Estimation Parameters Check--------------------------------

  learner_ps <- getElement(params, "learner_ps")
  if (!(ite_method %in% c("slearner", "tlearner",
                              "xlearner", "tpoisson"))) {
    if (length(learner_ps) == 0) {
      learner_ps <- "SL.xgboost"
    } else {
      if (!(class(learner_ps) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the learner_ps
           argument.")
      }
    }
  } else {
    learner_ps <- NA
  }
  params[["learner_ps"]] <- learner_ps

  # Outcome Estimation Parameters Check -----------------------
  learner_y <- getElement(params, "learner_y")
  if (ite_method %in% c("slearner", "tlearner", "xlearner", "aipw")) {
    if (length(learner_y) == 0) {
      learner_y <- "SL.xgboost"
    } else {
      if (!(class(learner_y) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the learner_y
           argument.")
      }
    }
  } else {
    learner_y <- NA
  }
  params[["learner_y"]] <- learner_y


  # Discard ITE Parameters if ITE estimates are provided------------------------
  if (!is.null(ite)) {
    params[["ite_method"]] <- "personalized"
    params[["learner_ps"]] <- NULL
    params[["learner_y"]] <- NULL
  }

  logger::log_debug("Done with checking method parameters.")

  return(params)
}
