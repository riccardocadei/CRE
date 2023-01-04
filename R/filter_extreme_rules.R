#' @title
#' Filter extreme decision rules
#'
#' @description
#' Discards rules with too few or too many observations.
#'
#' @param rules_matrix A causal rules matrix.
#' @param rules_list A list of the causal rules.
#' @param t_ext A threshold to define too generic or too specific rules.
#'
#' @keywords internal
#'
#' @return
#' A rules matrix only with the rules selected
#'
filter_extreme_rules <- function(rules_matrix, rules_list, t_ext) {

  # Identify rules with too few or too many observations
  ind <- 1:dim(rules_matrix)[2]
  sup <- apply(rules_matrix, 2, mean)
  elim <- which((sup < t_ext) | (sup > (1 - t_ext)))
  if (length(elim) > 0) {ind <- ind[-elim]}

  # Remove rules with too few/too many observations
  rules_matrix <- rules_matrix[, ind, drop=FALSE]
  rules_list <- rules_list[ind]
  colnames(rules_matrix) <- rules_list

  return(rules_matrix)
}
