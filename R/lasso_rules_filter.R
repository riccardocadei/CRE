#' @title
#' Filter (Causal) Decision Rules by LASSO (with stability selection)
#'
#' @description
#' Selects the causal rules that are most important.
#'
#' @param rules_matrix_std The standardized causal rules matrix.
#' @param rules_list A vector of causal rules.
#' @param ite_std The standardized ITE.
#' @param stability_selection Whether or not using stability selection to
#' select the causal rules.
#' @param cutoff  Threshold defining the minimum cutoff value for the stability
#' scores.
#' @param pfer Upper bound for the per-family error rate (tolerated amount of
#' falsely selected rules).
#'
#' @return
#' a vector of causal rules
#'
#' @keywords internal
#'
lasso_rules_filter <- function(rules_matrix_std, rules_list, ite_std,
                                stability_selection, cutoff, pfer) {

  `%>%` <- magrittr::`%>%`
  rules <- NULL
  if (length(rules_list)>1){

    if (stability_selection) {
      # LASSO Stability Selection
      stab_mod <- stabs::stabsel(x = rules_matrix_std,
                                 y = ite_std,
                                 fitfun = "glmnet.lasso",
                                 cutoff = cutoff,
                                 PFER = pfer)
      rule_stab <- rules_list[stab_mod$selected]
      select_rules <- rule_stab

    } else {
      # (Vanilla) LASSO
      cv_lasso <- glmnet::cv.glmnet(x = rules_matrix_std,
                                    y = ite_std,
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
