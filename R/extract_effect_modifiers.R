#' @title
#' Extract effect modifiers
#'
#' @description
#' Extracts the effect modifiers from a list of (causal) decision rules.
#'
#' @param rules_list A list of (causal) decision rules.
#' @param X_names A list of the covariate names.
#'
#' @keywords internal
#'
#' @return
#' A list of the Effect Modifiers.
#'
extract_effect_modifiers <- function(rules_list, X_names) {
  effect_modifiers <- c()
  for (X_name in X_names) {
    if (any(grepl(X_name,rules_list))){
      effect_modifiers <- append(effect_modifiers, X_name)
    }
  }
  return(effect_modifiers)
}
