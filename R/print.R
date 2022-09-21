#' @title
#' Extend print function for the CRE object
#'
#' @description
#' Prints a brief description of the CRE object
#'
#' @param x A cre object from running the CRE function
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.cre <- function(x, ...) {

  x <- unclass(x)

  cat(" CRE object \n")
  cat("    ***       ")
  if ("CATE_results" %in% names(x)) {
    print(paste("CRE results using CATE method: ", x[["CATE_method"]]))
    print(x[["CATE_results"]])
  } else {
    print("No significant rules were discovered.")
  }
  cat("    ***       ")
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

  if ("CATE_results" %in% names(object)) {
    print(paste("CRE results using CATE method: ", object[["CATE_method"]]))
    print(object[["CATE_results"]])
    if ("select_rules_1" %in% names(object)) {
      print("Select rules (1): ")
      print(object[["select_rules_1"]])
      print("Select rules (2): ")
      print(object[["select_rules_2"]])
    } else {
      print("Select rules: ")
      print(object[["select_rules"]])
    }
  } else {
    print("No significant rules were discovered.\n")
    print(paste("Average Treatment Effect (discovery): ", object[["ATE_dis"]]))
    print(paste("Average Treatment Effect (inference): ", object[["ATE_inf"]]))
  }
}

