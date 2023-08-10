#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Estimates the Conditional Average Treatment Effect (CATE) by
#' linearly modeling the Individual Treatment Effect by a set of rules.
#'
#' @param rules_matrix A rules matrix,
#' @param rules_explicit A list of select rules in terms of covariate names.
#' @param ite The estimated ITEs.
#' @param B The number of bootstrap samples for uncertainty quantification in
#' estimation.
#' @param subsample The bootstrap ratio subsample for uncertainty quantification
#' in estimation.
#'
#' @return
#' A list with 2 elements:
#' `summary`: A data frame summarizing the CATE linear decomposition:
#' - 'Rule': rule name,
#' - 'Estimate': linear contribution to CATE,
#' - 'CI_lower`: lower bound 95% confidence interval on the estimate,
#' - 'CI_upper`: upper bound 95% confidence interval on the estimate,
#' - `P-Value`: p-value (from Z-test).
#' `model`: A linear model for CATE-ATE estimation.
#'
#' @import stats
#' @keywords internal
#'
#'
estimate_cate <- function(rules_matrix, rules_explicit, ite, B=1, subsample=1) {

  logger::log_debug("Estimating CATE ...")

  "%>%" <- magrittr::"%>%"

  if (B == 1){
    # Estimate ATE (if No Rules Selected)
    ate_model <- stats::lm(ite ~ 1)
    ate_coeff <- summary(ate_model)$coefficients
    ate_ci <- stats::confint(ate_model)
    ate_summary <- data.frame(Rule = "(ATE)",
                              Estimate = ate_coeff[1],
                              CI_lower = ate_ci[1],
                              CI_upper = ate_ci[2],
                              P_Value = ate_coeff[2])
    rownames(ate_summary) <- 1:nrow(ate_summary)
    if (length(rules_explicit) == 0) {
      result <- ate_summary
    } else {
      # Estimate AATEs
      rules_df_inf <- as.data.frame(rules_matrix)
      names(rules_df_inf) <- rules_explicit
      aate_model <- stats::lm(ite - mean(ite) ~ . -1, data = rules_df_inf)
      filter_na <- is.na(aate_model$coefficients)
      if (sum(filter_na)) {
        rules_matrix <- rules_matrix[, !filter_na]
        rules_explicit <- rules_explicit[!filter_na]
        return(estimate_cate(rules_matrix, rules_explicit, ite))
      }
      aate_coeff <- summary(aate_model)$coef[, c(1, 4), drop = FALSE] %>%
                    as.data.frame()
      aate_ci <- stats::confint(aate_model) %>% as.data.frame()
      aate_summary <- data.frame(Rule = rules_explicit,
                                 Estimate = aate_coeff[, 1],
                                 CI_lower = aate_ci[, 1],
                                 CI_upper = aate_ci[, 2],
                                 P_Value = aate_coeff[, 2])
      result <- rbind(ate_summary, aate_summary)
      rownames(result) <- 1:nrow(result)
    }
  }
  else {
    models <- NULL
    for (i in 1:B) {
      index <- sample(length(ite),
                      size = round(length(ite)*subsample),
                      replace = FALSE)
      ite_ <- ite[index]
      if (length(rules_explicit) > 0) {
        rules_matrix_ <- rules_matrix[index, ]
      } else {
        rules_matrix_ <- NULL
      }
      model <- estimate_cate(rules_matrix_, rules_explicit, ite_, B=1,
                             subsample=1)
      models <- rbind(models, model)
    }
    result <- aggregate(Estimate ~ Rule,
                        data = models,
                        FUN = function(x) c(mean = mean(x), sd = sd(x)))
    Mean_Estimate <- result[,2][,1]
    Std_Dev_Estimate <- result[,2][,2]
    result$t <- Mean_Estimate / Std_Dev_Estimate
    result$p_value <- 2 * (1 - pt(abs(result$t),
                                  length(ite) - nrow(result)))
    result$CI_lower <- Mean_Estimate - 1.96 * Std_Dev_Estimate
    result$CI_upper <- Mean_Estimate + 1.96 * Std_Dev_Estimate
    result <- data.frame(Rule = result$Rule,
                         Estimate = Mean_Estimate,
                         CI_lower = result$CI_lower,
                         CI_upper = result$CI_upper,
                         P_Value = result$p_value)
  }

  if (nrow(result)>1 & B>1) {
    filter_pvalue <- result$P_Value[2:length(result$P_Value)] <= 0.05
    if (sum(filter_pvalue) < length(filter_pvalue)) {
      rules_matrix <- rules_matrix[, filter_pvalue, drop = FALSE]
      rules_explicit <- rules_explicit[filter_pvalue]
      return(estimate_cate(rules_matrix, rules_explicit, ite, B, subsample))
    }
  }
  #logger::log_debug("Done with estimating CATE.")
  return(result)
}
