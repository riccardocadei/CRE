#' @title
#' Generate Rules Matrix
#'
#' @description
#' Generates a matrix of causal rules given a list.
#'
#' @param X The features matrix.
#' @param rules_list A vector of causal rules.
#' @param t The common support used in generating the causal rules matrix.
#'
#' @return
#' a list with:
#' - a raw matrix of causal rules
#' - a standardized matrix of causal rules, and
#' - a vector of causal rules
#'
generate_rules_matrix <- function(X, rules_list, t) {

  # Generate and Rules Matrix
  samplesize <- dim(X)[1]
  nrules <- length(rules_list)
  rules_matrix <- matrix(0, nrow = samplesize, ncol = nrules)
  for (i in 1:nrules){
    rules_matrix[eval(parse(text = rules_list[i])), i] <- 1
  }

  # Identify rules with too few or too many observations
  ind <- 1:nrules
  sup <- apply(rules_matrix, 2, mean)
  elim <- which((sup < t) | (sup > (1 - t)))
  if (length(elim) > 0) {ind <- ind[-elim]}

  # Identify correlated rules
  corelim <- 1
  C <- stats::cor(rules_matrix[, ind])
  nrules <- length(ind)
  elim <- c()
  for(i in 1:(nrules - 1)) {
    elim <- c(elim, which(round(abs(C[i, (i + 1):nrules]), digits = 4)
                          >= corelim)
                  + i)
  }
  if (length(elim) > 0) {ind <- ind[-elim]}

  # Remove rules with too few/too many observations and correlated rules
  rules_matrix <- rules_matrix[, ind,drop=FALSE]
  rules_list <- rules_list[ind]

  return(list(rules_matrix = rules_matrix, rules_list = rules_list))
}


#' @title
#' Standardize Rules Matrix
#'
#' @description
#' Standardize the matrix of causal rules given a list.
#'
#' @param rules_matrix The rules matrix.
#'
#' @return
#' Standardized rules matrix
#'
standardize_rules_matrix <- function(rules_matrix) {

  samplesize <- dim(rules_matrix)[1]
  nrules <- dim(rules_matrix)[2]
  mu_rules_matrix <- apply(rules_matrix, 2, mean)
  sd_rules_matrix <- apply(rules_matrix, 2, stats::sd)
  rules_matrix_std <- matrix(0, samplesize, nrules)
  for(l in 1:ncol(rules_matrix_std)){
    rules_matrix_std[, l] <- ((rules_matrix[, l] - mu_rules_matrix[l]) /
                                sd_rules_matrix[l])
  }

  return(rules_matrix_std)

}
