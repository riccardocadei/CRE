#' @title
#' Select rules
#'
#' @description
#' Given a set of rules, selects the minimal set linearly decomposing the
#' Conditional Average Treatment Effect (CATE) by LASSO (optionally with
#' Stability Selection).
#'
#' @param rules_matrix The rules matrix.
#' @param rules A list of rules.
#' @param ite The estimated ITE.
#' @param stability_selection Stability selection method.
#' @param cutoff  Threshold (percentage) defining the minimum cutoff value for
#' the stability scores. Only for stability selection.
#' @param pfer Upper bound for the per-family error rate (tolerated amount of
#' falsely selected rules). Only for stability selection.
#' @param B Number of bootstrap samples.
#'
#' @return
#' A minimal set of rules linearly decomposing the CATE.
#'
#' @keywords internal
#'
select_rules <- function(rules_matrix, rules, ite,
                         stability_selection, cutoff, pfer, B) {

  logger::log_debug("Selecting rules...")

  "%>%" <- magrittr::"%>%"

  rules_weight <- c()
  for (rule in rules) {
    rule_length <- lengths(regmatches(rule, gregexpr("&", rule))) + 1
    rules_weight <- append(rules_weight, rule_length)
  }
  R <- t(t(rules_matrix) / rules_weight)
  M <- ncol(R)

  if (length(rules) > 1) {

    if (stability_selection=="vanilla") {
      # Vanilla Stability Selection
      stability_scores <- rep(0, M)
      ite_mean <- mean(ite)
      for (i in 1:B) {
        subsample <- 0.5
        indices <- sample(1:nrow(R),
                          size = round(nrow(R) * subsample),
                          replace = FALSE)
        lasso <- glmnet::cv.glmnet(x = as.matrix(R[indices, ]),
                                   y = ite[indices] - ite_mean,
                                   alpha = 1,
                                   nfolds = 5,
                                   gamma = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10),
                                   intercept = FALSE)
        non_zero_indices <- which(as.matrix(coef(lasso)) != 0) - 1
        stability_scores[non_zero_indices] <- stability_scores[non_zero_indices] + 1
      }
      stability_scores <- stability_scores / B
      rules <- colnames(R)[stability_scores >= cutoff]

    } else if (stability_selection=="error_control") {
      # Stability Selection with Error Control
      stab_mod <- tryCatch(
        {
          stabs::stabsel(x = as.data.frame(R),
                         y = ite - mean(ite),
                         intercept = FALSE,
                         fitfun = "glmnet.lasso",
                         cutoff = cutoff,
                         PFER = pfer)
        },
        error = function(e) {
          stop(paste(
            "Combination of `cutoff` and `pfer` not allowed. ",
            "Try to decrease the `cutoff` or increase the `pfer`. ",
            "See Stability Selection documentation for further details.",
            "\n\nOriginal Error message:", e))
        }
      )
      rules <- rules[stab_mod$selected]

    } else if (stability_selection=="no") {
        # LASSO
        cv_lasso <- glmnet::cv.glmnet(x = rules_matrix,
                                      y = ite - mean(ite),
                                      alpha = 1,
                                      intercept = FALSE)
        aa <- stats::coef(cv_lasso, s = cv_lasso$lambda.1se)
        index_aa <- which(aa[-1, 1] != 0)
        rule_LASSO <- data.frame(rules = rules[index_aa],
                                 val = aa[index_aa + 1, 1])
        rule_LASSO <- rule_LASSO[order(-rule_LASSO[, 2]), ]
        rule_LASSO <- rule_LASSO[!is.na(rule_LASSO$rules), ]
        rules <- rule_LASSO$rules
    }
  } else {
    rules <- NULL
  }

  logger::log_debug("Done with selecting rules.")
  return(rules)
}
