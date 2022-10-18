#' @title
#' Discard Anomalous (Causal) Decision Rules
#'
#' @description
#' Discard rules with too few or too many observations.
#'
#' @param rules_matrix The causal rules matrix.
#' @param rules_list The list of the causal rules.
#' @param t The threshold to define too generic or too specific rules.
#'
#' @keywords internal
#'
#' @return
#' A list with:
#' - the filtered rules matrix
#' - the filtered rules list
#'
discard_anomalous_rules <- function(rules_matrix, rules_list, t){

  # Identify rules with too few or too many observations
  ind <- 1:dim(rules_matrix)[2]
  sup <- apply(rules_matrix, 2, mean)
  elim <- which((sup < t) | (sup > (1 - t)))
  if (length(elim) > 0) {ind <- ind[-elim]}

  # Remove rules with too few/too many observations
  rules_matrix <- rules_matrix[, ind, drop=FALSE]
  rules_list <- rules_list[ind]

  return(list(rules_matrix = rules_matrix, rules_list = rules_list))
}
