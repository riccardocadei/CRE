#' @title
#' Interpret rules
#'
#' @description
#' Replaces the column numbers in the rules vector with their real names.
#'
#' @param rules A vector of rules.
#' @param X_names A vector of real names of the covariates.
#'
#' @return
#' A list of explicit (human-readable) rules.
#'
#'
#'@keywords internal
#'
interpret_rules <- function(rules, X_names) {
  replacements <- X_names
  names(replacements) <- paste("X[,", 1:length(X_names), "]", sep = "")
  n_rules <- length(rules)
  rules_explicit <- vector(length = n_rules)
  for (j in 1:n_rules) {
    rules_explicit[j] <- stringr::str_replace_all(rules[j],
                                                  stringr::fixed(replacements))
  }
  return(rules_explicit)
}
