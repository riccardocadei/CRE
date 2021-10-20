#' @title
#' Select Causal Rules
#'
#' @description
#' Selects the causal rules that are most important.
#'
#' @param rules_matrix_std the standardized causal rules matrix
#' @param rules_list a vector of causal rules
#' @param ite_std the standardized ITE
#' @param binary whether or not the outcome is binary
#' @param q the selection threshold used in selecting the causal rules
#' @param rules_method the method for selecting causal rules with binary
#'  outcomes, either "conservative", "anticonservative", or NA
#'
#' @return
#' a vector of causal rules
#'
#' @export
#'
select_causal_rules <- function(rules_matrix_std, rules_list, ite_std, binary,
                                q, rules_method) {

  `%>%` <- magrittr::`%>%`
  rules <- NULL

  if (binary) {

    # Stability selection
    stab_mod <- stabs::stabsel(rules_matrix_std, ite_std,
                               fitfun = "glmnet.lasso", cutoff = q, PFER = 1,
                               args.fitfun = list(type = rules_method))
    rule_stab <- rules_list[stab_mod$selected]
    select_rules <- rule_stab

  } else {

    # LASSO
    cv_lasso <- glmnet::cv.glmnet(rules_matrix_std, ite_std, alpha = 1,
                                  intercept = FALSE)
    aa <- stats::coef(cv_lasso, s = cv_lasso$lambda.1se)
    index_aa <- which(aa[-1,1] != 0)
    rule_LASSO <- data.frame(rules = rules_list[index_aa],
                             val = aa[index_aa + 1, 1])
    rule_LASSO <- (rule_LASSO[order(-rule_LASSO[,2]), ] %>%
                     dplyr::filter(!is.na(rules)))
    select_rules <- rule_LASSO$rules

  }

  return(select_rules)
}
