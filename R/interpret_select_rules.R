#' @title
#' Interpret Select Rules
#'
#' @description
#' Replace the column numbers in the Select Rules vector with their real names
#'
#' @param select_rules_dis a vector of select causal rules
#' @param X_names the real names of the covariates
#'
#' @return a vector of select causal rules that are interpretable
#'

interpret_select_rules <- function(select_rules_dis, X_names) {
  replacements <- X_names
  names(replacements) <- paste("X[,", 1:length(X_names), "]", sep = "")
  n_rules <- length(select_rules_dis)
  select_rules_interpretable <- vector(length = n_rules)
  for (j in 1:n_rules) {
    select_rules_interpretable[j] <- stringr::str_replace_all(select_rules_dis[j],
                                                              stringr::fixed(replacements))
  }
  return(select_rules_interpretable)
}
