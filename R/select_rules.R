#' @title
#' Select Rules
#'
#' @description
#' Given a set of Rules, select the minimal set Linearly Decomposing the
#' Conditional Average Treatment Effect (CATE) by LASSO (optionally with
#' Stability Selection).
#'
#' @param rules_matrix The rules matrix.
#' @param rules_list A list of Rules.
#' @param ite The estimated ITE.
#' @param stability_selection Whether or not using stability selection.
#' @param cutoff  Threshold (percentage) defining the minimum cutoff value for
#' the stability scores Only for Stability Selection.
#' @param pfer Upper bound for the per-family error rate (tolerated amount of
#' falsely selected rules). Only for Stability Selection.
#' @param penalty_rl Order of penalty for rules length during LASSO
#' regularization (i.e. 0: no penalty, 1: rules_length, 2: rules_length^2).
#'
#' @return
#' The minimal set of Rules Linearly Decomposing the CATE.
#'
#' @keywords internal
#'
select_rules <- function(rules_matrix, rules_list, ite,
                         stability_selection, cutoff, pfer,
                         penalty_rl) {
  if (penalty_rl > 0) {
    rules_weight <- c()
    for (rule in rules_list) {
      rules_length <- lengths(regmatches(rule, gregexpr("&", rule))) + 1
      rule_weight <- rules_length^penalty_rl
      rules_weight <- append(rules_weight, rule_weight)
    }
    rules_matrix <- t(t(rules_matrix) / rules_weight)
  }

  `%>%` <- magrittr::`%>%`
  rules <- NULL
  if (length(rules_list) > 1) {
    if (stability_selection) {
      # Stability Selection LASSO
      stab_mod <- tryCatch(
        {
          stabs::stabsel(x = rules_matrix,
                         y = ite,
                         fitfun = "glmnet.lasso",
                         cutoff = cutoff,
                         PFER = pfer)
        },
        error = function(e) {
          stop(paste(
            "Combination of `cutoff` and `pfer` not allowed.",
            "\nTry to decrease the `cutoff` or increase the `pfer`.",
            "\nSee Stability Selection documentation for further details.",
            "\n\nOriginal Error message:", e))
        }
      )
      rules_list <- rules_list[stab_mod$selected]
    } else {
      # vanilla LASSO
      cv_lasso <- glmnet::cv.glmnet(x = rules_matrix,
                                    y = ite,
                                    alpha = 1,
                                    intercept = FALSE)
      aa <- stats::coef(cv_lasso, s = cv_lasso$lambda.1se)
      index_aa <- which(aa[-1, 1] != 0)
      rule_LASSO <- data.frame(rules = rules_list[index_aa],
                               val = aa[index_aa + 1, 1])
      rule_LASSO <- rule_LASSO[order(-rule_LASSO[, 2]), ] %>%
        dplyr::filter(!is.na(rules))
      rules_list <- rule_LASSO$rules
    }
  }

  return(rules_list)
}
