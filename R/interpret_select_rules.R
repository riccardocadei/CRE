#' @title
#' Interpret select rules
#'
#' @description
#' Replaces the column numbers in the select rules vector with their real names.
#'
#' @param select_rules_dis A vector of select causal rules.
#' @param X_names A vector of real names of the covariates.
#'
#' @return
#' A vector of select causal rules that are interpretable.
#'
#'
#'@keywords internal
#'
interpret_select_rules <- function(select_rules_dis, X_names) {
  replacements <- X_names
  names(replacements) <- paste("X[,", 1:length(X_names), "]", sep = "")
  n_rules <- length(select_rules_dis)
  select_rules_interpretable <- vector(length = n_rules)
  for (j in 1:n_rules) {
    select_rules_interpretable[j] <- stringr::str_replace_all(
                                                  select_rules_dis[j],
                                                  stringr::fixed(replacements))
  }
  return(select_rules_interpretable)
}
