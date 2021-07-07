#' @title
#' Select Causal Rules
#'
#' @description
#' Method for selecting the causal rules that are most important
#'
#' @param rules_matrix_std the standardized causal rules matrix
#' @param rules_list a vector of causal rules
#' @param ite_std the standardized ITE
#' @param rules_method the method for performing penalized regression on the causal rules to select only those that are important
#'
#' @return a vector of causal rules
#'
#' @export
#'
#' @examples
#' TBD
#'
select_causal_rules <- function(rules_matrix_std, rules_list, ite_std, rules_method) {
  stopifnot(rules_method %in% c("lasso", "stab"))
  if (rules_method == "lasso") {
    # LASSO
    lambda <- 10^seq(10, -2, length = 100)
    lasso_mod <- glmnet::glmnet(rules_matrix_std, ite_std, alpha = 1, lambda = lambda, intercept = FALSE)
    cv_lasso <- glmnet::cv.glmnet(rules_matrix_std, ite_std, alpha = 1, intercept = FALSE)
    bestlamda <- cv_lasso$lambda.min
    aa <- coef(cv_lasso, s = cv_lasso$lambda.1se)
    index_aa <- which(aa[-1,1] != 0)
    rule_LASSO <- data.frame(rules = rules_list[index_aa], val = aa[index_aa + 1, 1])
    rule_LASSO <- rule_LASSO[order(-rule_LASSO[,2]), ] %>% filter(!is.na(rules))
    select_rules <- rule_LASSO$rules
  } else {
    # Stability selection
    stab_mod <- stabs::stabsel(rules_matrix_std, ite_std, fitfun = glmnet.lasso, cutoff = 0.8, PFER = 1, args.fitfun = list(type = "conservative"))
    rule_stab <- rules_list[stab_mod$selected]
    select_rules <- rule_stab
  }
  return(select_rules)
}
