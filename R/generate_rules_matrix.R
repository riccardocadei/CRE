#' @title
#' Generate Rules Matrix
#'
#' @description
#' Method for generating a matrix of causal rules given a list
#'
#' @param X the features matrix
#' @param rules_list a vector of causal rules
#' @param t the common support used in generating the causal rules matrix
#'
#' @return a list with a raw matrix of causal rules, a standardized matrix of causal rules, and a vector of causal rules
#'
#' @export
#'
generate_rules_matrix <- function(X, rules_list, t) {
  rules_matrix <- matrix(0, nrow = dim(X)[1], ncol = length(rules_list))
  for (i in 1:length(rules_list)){
    rules_matrix[eval(parse(text = rules_list[i])), i] <- 1
  }
  # Identify rules with too few observations
  nrules <- dim(rules_matrix)[2]
  ind <- 1:nrules
  if (dim(X)[1] < 200){
    t <- 0.05
  }
  sup <- apply(rules_matrix, 2, mean)
  elim <- which((sup < t) | (sup > (1 - t)))

  if (length(elim) > 0) {
    ind <- ind[-elim]
  }
  # Identify correlated rules
  corelim <- 1
  C <- stats::cor(rules_matrix[,ind])
  diag(C) <- 0
  nrules <- dim(rules_matrix[, ind])[2]
  elim <- c()
  for(i in 1:(nrules - 1)) {
    elim <- c(elim, which(round(abs(C[i, (i + 1):nrules]), digits = 4) >= corelim) + i)
  }
  if (length(elim) > 0) {
    ind <- ind[-elim]
  } else {
    ind <- ind
  }
  # Remove rules with too few observations and correlated rules
  rules_matrix <- rules_matrix[, ind]
  rules_list <- rules_list[ind]

  # Standardize rules matrix
  mu_rules_matrix <- apply(rules_matrix, 2, mean)
  sd_rules_matrix <- apply(rules_matrix, 2, stats::sd)
  rules_matrix_std <- matrix(0, dim(X)[1], dim(rules_matrix)[2])
  for(l in 1:ncol(rules_matrix_std)){
    rules_matrix_std[, l] <- (rules_matrix[, l] - mu_rules_matrix[l]) / sd_rules_matrix[l]
  }

  return(list(rules_matrix = rules_matrix, rules_matrix_std = rules_matrix_std, rules_list = rules_list))
}
