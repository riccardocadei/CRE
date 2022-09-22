#' @title
#' Select Causal Rules
#'
#' @description
#' Selects the causal rules that are most important.
#'
#' @param rules_matrix_std The standardized causal rules matrix.
#' @param rules_list A vector of causal rules.
#' @param ite_std The standardized ITE.
#' @param q The selection threshold used in selecting the causal rules.
#' @param stability_selection Whether or not using stability selection to select the causal rules.
#' @param pfer_val the Per-Family Error Rate, the expected number of false discoveries
#'
#' @return
#' a vector of causal rules
#'
select_causal_rules <- function(rules_matrix_std, rules_list, ite_std,
                                q, stability_selection, pfer_val) {

  #`%>%` <- magrittr::`%>%`
  rules <- NULL
  if (stability_selection) {
    # Stability selection
    stab_mod <- stabs::stabsel(rules_matrix_std, ite_std, fitfun = "glmnet.lasso",
                               cutoff = q, PFER = pfer_val, args.fitfun = "conservative")
    rule_stab <- rules_list[stab_mod$selected]
    select_rules <- rule_stab

  } else {

    # LASSO
    cv_lasso <- glmnet::cv.glmnet(rules_matrix_std, ite_std, alpha = 1, intercept = FALSE)
    aa <- stats::coef(cv_lasso, s = cv_lasso$lambda.1se)
    index_aa <- which(aa[-1,1] != 0)
    rule_LASSO <- data.frame(rules = rules_list[index_aa], val = aa[index_aa + 1, 1])
    rule_LASSO <- rule_LASSO[order(-rule_LASSO[,2]), ] %>% dplyr::filter(!is.na(rules))
    select_rules <- rule_LASSO$rules

  }

  return(select_rules)
}
