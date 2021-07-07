#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Method for estimating the Conditional Average Treatment Effect given a standardized vector of Individual Treatment Effects, a standardized matrix of causal rules, a list of causal rules
#'
#' @param ite_inf the standardized ITE for the inference subsample
#' @param rules_matrix_inf the standardized causal rules matrix for the inference subsample
#' @param rules_list_inf the list of causal rules for the inference subsample
#'
#' @return two sets of CATE estimates, one done by averaging the ITEs and the other using linear regression
#'
#' @export
#'
#' @examples
#' TBD
#'
estimate_cate <- function(ite_inf, rules_matrix_inf, rules_list_inf) {
  # Convert rules matrix to a data frame and convert columns to factors
  rules_df_factor <- as.data.frame(rules_matrix_inf)
  for (i in 1:ncol(rules_df_factor)) {
    rules_df_factor[,i] <- as.factor(rules_df_factor[,i])
  }

  # Check that matrix and list of rules have same length, then join them
  stopifnot(ncol(rules_matrix_inf) == length(rules_list_inf))
  names(rules_df_factor) <- rules_list_inf
  joined_ite_rules <- cbind(ite_inf, rules_df_factor)

  # Generate CATE data frame with ATE
  cate_means <- data.frame(Rule = "Average Treatment Effect", CATE = mean(ite_inf),
                           CI_lower = mean(ite_inf) - (1.96 * sqrt(var(ite_inf)) / sqrt(length(ite_inf))),
                           CI_upper = mean(ite_inf) + (1.96 * sqrt(var(ite_inf)) / sqrt(length(ite_inf))))

  # Determine CATE manually for each rule
  for (i in 1:length(rules_list_inf)) {
    df_temp <- data.frame(ite = joined_ite_rules[,1], rule = joined_ite_rules[,i + 1]) %>% filter(rule == 1)
    cate_temp <- data.frame(Rule = rules_list_inf[i], CATE = mean(df_temp$ite),
                            CI_lower = mean(df_temp$ite) - (1.96 * sqrt(var(df_temp$ite)) / sqrt(length(df_temp$ite))),
                            CI_upper = mean(df_temp$ite) + (1.96 * sqrt(var(df_temp$ite)) / sqrt(length(df_temp$ite))))
    cate_means <- rbind(cate_means, cate_temp)
  }

  # Fit linear regression model without contrasts, then extract coefficients and confidence intervals
  model1_cate <- stats::lm(ite_inf ~ ., data = joined_ite_rules)
  model1_coef <- summary(model1_cate)$coef[,c(1,4)] %>% as.data.frame
  model1_ci <- stats::confint(model1_cate) %>% as.data.frame() %>% filter(!is.na(.))

  # Generate model 1 data frame
  cate_reg_orig <- model1_coef %>% cbind(model1_ci)
  cate_reg_orig_names <- stringr::str_extract(row.names(cate_reg_orig), "`.*`") %>% str_remove_all("`")
  cate_reg_orig_names[1] <- "(Intercept)"
  cate_reg_orig$Rule <- cate_reg_orig_names
  row.names(cate_reg_orig) <- 1:nrow(cate_reg_orig)
  cate_reg_orig <- cate_reg_orig %>% summarize(Rule, CATE = Estimate, PVal = `Pr(>|t|)`,
                                               CI_lower = `2.5 %`, CI_upper = `97.5 %`)

  # Return final results
  cates <- list(cate_means = cate_means, cate_reg_orig = cate_reg_orig)
  return(cates)
}
