#' @title
#' Filter Correlated Decision Rules
#'
#' @description
#' Discard redundant correlated rules.
#'
#' @param rules_matrix The causal rules matrix.
#' @param rules_list The list of the causal rules.
#' @param t The threshold to define correlated rules.
#'
#' @keywords internal
#'
#' @return
#' The rules matrix only with the rules selected
#'
filter_correlated_rules <- function(rules_matrix, rules_list, t){

  # Identify correlated rules
  nrules <- length(rules_list)
  ind <- 1:nrules
  C <- stats::cor(rules_matrix)
  elim <- c()
  for(i in 1:(nrules - 1)) {
    elim <- c(elim,
              which(round(abs(C[i, (i + 1):nrules]), digits = 4) >= t) + i)
  }
  if (length(elim) > 0) {ind <- ind[-elim]}

  # Remove correlated rules
  rules_matrix <- rules_matrix[, ind,drop=FALSE]
  rules_list <- rules_list[ind]
  colnames(rules_matrix) <- rules_list

  return(rules_matrix)
}
