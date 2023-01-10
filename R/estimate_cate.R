#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Estimates the Conditional Average Treatment Effect (CATE) by
#' linearly modeling the Individual Treatment Effect on the Inference subsample
#' via the rules selected in the Discovery step.
#'
#' @param y_inf The outcome vector for the inference subsample.
#' @param z_inf The treatment vector for the inference subsample.
#' @param X_inf The covariate vector for the inference subsample.
#' @param rules_matrix_inf The rules matrix for the
#' inference subsample.
#' @param rules_explicit The list of select rules in terms of
#' coviariate names (explicit).
#' @param ite_inf The estimated ITEs for the inference subsample.
#' @param t_pvalue The threshold to define statistically significant rules
#' (filter only rules with p-value <= t_pvalue).
#'
#' @return
#' A dataframe summarizing the CATE linear decomposition:
#' - 'Rule': rule name,
#' - 'Estimate': linear contribution to CATE,
#' - 'CI_lower`: lower bound 95% confidence interval on the estimate,
#' - 'CI_upper`: upper bound 95% confidence interval on the estimate,
#' - `P-Value`: p-value (from Z-test).
#'
#' @import stats
#' @keywords internal
#'
#'
estimate_cate <- function(y_inf, z_inf, X_inf,
                          rules_matrix_inf, rules_explicit,
                          ite_inf, t_pvalue) {

  `%>%` <- magrittr::`%>%`

  if (any(is.na(rules_explicit))) {
    # Estimate ATE (if No Rules Selected)
    cate_model <- stats::lm(ite_inf ~ 1)
    cate_coeff <- summary(cate_model)$coefficients
    cate_ci <- stats::confint(cate_model)
    cate_summary <- data.frame(Rule = "(BATE)",
                               Estimate = cate_coeff[1],
                               CI_lower = cate_ci[1],
                               CI_upper = cate_ci[2],
                               P_Value = cate_coeff[2])
    rownames(cate_summary) <- 1:nrow(cate_summary)

  } else {
    # Estimate CATE
    rules_df_inf <- as.data.frame(rules_matrix_inf)
    names(rules_df_inf) <- rules_explicit
    dataset_inf <- cbind(ite_inf, rules_df_inf)
    cate_model <- stats::lm(ite_inf ~ ., data = dataset_inf)
    cate_coeff <- summary(cate_model)$coef[, c(1, 4)] %>% as.data.frame()
    cate_ci <- stats::confint(cate_model) %>% as.data.frame()
    cate_summary <- data.frame(Rule = c("(BATE)", rules_explicit),
                               Estimate = cate_coeff[, 1],
                               CI_lower = cate_ci[, 1],
                               CI_upper = cate_ci[, 2],
                               P_Value = cate_coeff[, 2])
    row.names(cate_summary) <- 1:nrow(cate_summary)
    # Filter Not Significant Rules
    if (t_pvalue<1){
      filter_pvalue <- cate_summary$P_Value <= t_pvalue
      M <- length(filter_pvalue)
      if (sum(filter_pvalue[2:M])<M-1) {
        rules_matrix_inf <- rules_matrix_inf[,filter_pvalue[2:M]]
        rules_explicit <- rules_explicit[filter_pvalue[2:M]]
        return(estimate_cate(y_inf, z_inf, X_inf, rules_matrix_inf,
                             rules_explicit, ite_inf, t_pvalue))
      }
    }
  }
  cate <- list(summary = cate_summary, model = cate_model)
  return(cate)
}
