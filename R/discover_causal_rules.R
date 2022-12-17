#' @title
#' Discover Causal Decision Rules
#'
#' @description
#' Discover Causal Decision Rules by randomization-based tests
#'
#' @param rules_matrix The causal rules matrix.
#' @param rules_list A vector of causal rules.
#' @param ite The estimated ITE.
#' @param stability_selection Whether or not using stability selection to
#' select the causal rules.
#' @param cutoff  Threshold defining the minimum cutoff value for the stability
#' scores.
#' @param pfer Upper bound for the per-family error rate (tolerated amount of
#' falsely selected rules).
#' @param penalty_rl Order of penalty for rules length during LASSO for Causal
#' Rules Discovery (i.e. 0: no penalty, 1: ∝rules_length, 2: ∝rules_length^2)
#'
#' @return
#' List of the Causal Decision Rules discovered
#'
#' @keywords internal
#'
discover_causal_rules <- function(rules_matrix, rules_list, ite,
                                  stability_selection, cutoff, pfer,
                                  penalty_rl) {

  if (penalty_rl>0){
    rules_weight = c()
    for (rule in rules_list){
      rules_length = lengths(regmatches(rule, gregexpr("&", rule)))+1
      rule_weight <- rules_length^penalty_rl
      rules_weight <- append(rules_weight,rule_weight)
    }
    rules_matrix = t(t(rules_matrix)/rules_weight)
  }

  `%>%` <- magrittr::`%>%`
  rules <- NULL
  if (length(rules_list)>1){

    if (stability_selection) {
      # Stability Selection LASSO
      stab_mod <- stabs::stabsel(x = rules_matrix,
                                 y = ite,
                                 fitfun = "glmnet.lasso",
                                 cutoff = cutoff,
                                 PFER = pfer)
      rule_stab <- rules_list[stab_mod$selected]
      select_rules <- rule_stab

    } else {
      # vanilla LASSO
      cv_lasso <- glmnet::cv.glmnet(x = rules_matrix,
                                    y = ite,
                                    alpha = 1,
                                    intercept = FALSE)
      aa <- stats::coef(cv_lasso, s = cv_lasso$lambda.1se)
      index_aa <- which(aa[-1,1] != 0)
      rule_LASSO <- data.frame(rules = rules_list[index_aa],
                               val = aa[index_aa + 1, 1])
      rule_LASSO <- rule_LASSO[order(-rule_LASSO[,2]), ] %>%
        dplyr::filter(!is.na(rules))
      select_rules <- rule_LASSO$rules
    }
  } else {
    select_rules <- rules_list
  }

  return(select_rules)
}
