#' @title
#' Filter correlated rules
#'
#' @description
#' Discards highly correlated rules (i.e. `Cov(rule1,rule2) > t_corr`).
#'
#' @param rules_matrix A rules matrix.
#' @param rules_list A list of rules (names).
#' @param t_corr A threshold to define correlated rules.
#'
#' @keywords internal
#'
#' @return
#' A rules matrix (without the highly correlated columns).
#'
filter_correlated_rules <- function(rules_matrix, rules_list, t_corr) {

  # Identify correlated rules
  nrules <- length(rules_list)
  ind <- 1:nrules
  C <- stats::cor(rules_matrix)
  elim <- c()
  for (i in 1:(nrules - 1)) {
    elim <- c(elim,
              which(round(abs(C[i, (i + 1):nrules]), digits = 4) >= t_corr) + i)
  }
  if (length(elim) > 0) ind <- ind[-elim]

  # Remove correlated rules
  rules_matrix <- rules_matrix[, ind, drop = FALSE]
  rules_list <- rules_list[ind]
  colnames(rules_matrix) <- rules_list

  return(rules_matrix)
}
