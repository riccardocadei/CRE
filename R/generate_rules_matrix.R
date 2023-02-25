#' @title
#' Generate rules matrix
#'
#' @description
#' Generates the rules matrix from the feature covariate matrix and a vector of
#' rules. The number of rows in rules_matrix is equal to the number of samples
#' in `X`, and the number of columns is equal to the number of rules in
#' `rules_list`. Each element of rules_matrix corresponds to a specific data
#' sample and rule. If the data sample satisfies a rule, the corresponding
#' element in rules_matrix is set to 1. Otherwise, the element is set to 0.
#'
#' @param X Features matrix.
#' @param rules_list A vector of rules.
#'
#'
#' @return
#' A causal rules matrix.
#'
#' @keywords internal
#'
generate_rules_matrix <- function(X, rules_list) {

  # Generate and Rules Matrix
  samplesize <- dim(X)[1]
  nrules <- length(rules_list)
  rules_matrix <- matrix(0, nrow = samplesize, ncol = nrules)
  for (i in 1:nrules){
    rules_matrix[eval(parse(text = rules_list[i]), list(X = X)), i] <- 1
  }
  return(rules_matrix)
}


#' @title
#' Standardize Rules Matrix
#'
#' @description
#' Standardize (i.e. mean=0 and stdev=1) the rules matrix.
#'
#' @param rules_matrix The rules matrix.
#'
#' @return
#' Standardized rules matrix
#'
#' @keywords internal
standardize_rules_matrix <- function(rules_matrix) {

  samplesize <- dim(rules_matrix)[1]
  nrules <- dim(rules_matrix)[2]
  mu_rules_matrix <- apply(rules_matrix, 2, mean)
  sd_rules_matrix <- apply(rules_matrix, 2, stats::sd)
  rules_matrix_std <- matrix(0, samplesize, nrules)
  for (l in 1:ncol(rules_matrix_std)) {
    rules_matrix_std[, l] <- ((rules_matrix[, l] - mu_rules_matrix[l]) /
                                sd_rules_matrix[l])
  }

  return(rules_matrix_std)
}
