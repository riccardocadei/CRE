#' @title
#' Extract Effect Modifiers
#'
#' @description
#' Extracts the Effect Modifiers from a list of (Causal) Decision Rules.
#'
#' @param rules_list A list of (Causal) Decision Rules
#' @param X_names A list of the Covariate names.
#'
#' @keywords internal
#'
#' @return
#' List of the Effect Modifiers.
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
