#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Method for estimating the Conditional Average Treatment Effect given a standardized vector of Individual Treatment Effects, a standardized matrix of causal rules, a list of causal rules
#'
#' @param ite_inf the ITE for the inference subsample
#' @param sd_ite_inf the standard deviations for the inference ITEs
#' @param rules_matrix_inf the standardized causal rules matrix for the inference subsample
#' @param rules_list_inf the list of causal rules for the inference subsample
#' @param ite_method_inf the method for estimating the inference ITEs
#'
#' @return a matrix of CATE estimates
#'
#' @export
#'
estimate_cate <- function(ite_inf, sd_ite_inf, rules_matrix_inf, rules_list_inf, ite_method_inf) {
  # Check that matrix and list of rules have same length, then join them
  stopifnot(ncol(rules_matrix_inf) == length(rules_list_inf))
  df_rules_factor <- as.data.frame(rules_matrix_inf) %>% dplyr::transmute_all(as.factor)
  names(df_rules_factor) <- rules_list_inf

  if (ite_method_inf %in% c("cf", "bart", "xbart", "bcf", "xbcf")) {
    joined_ite_rules <- cbind(ite_inf, sd_ite_inf, df_rules_factor)

    # Generate CATE data frame with ATE
    cate_means <- data.frame(Rule = "Average Treatment Effect", CATE = mean(ite_inf),
                             CI_lower = mean(ite_inf) - (1.96 * mean(sd_ite_inf)),
                             CI_upper = mean(ite_inf) + (1.96 * mean(sd_ite_inf)))

    # Determine CATE manually for each rule
    for (i in 1:length(rules_list_inf)) {
      df_temp <- data.frame(ite_inf = joined_ite_rules[,1], sd_ite_inf = joined_ite_rules[,2],
                            rule = joined_ite_rules[,i + 2]) %>% dplyr::filter(rule == 1)
      cate_temp <- data.frame(Rule = rules_list_inf[i], CATE = mean(df_temp$ite),
                              CI_lower = mean(df_temp$ite_inf) - (1.96 * mean(df_temp$sd_ite_inf)),
                              CI_upper = mean(df_temp$ite_inf) + (1.96 * mean(df_temp$sd_ite_inf)))
      cate_means <- rbind(cate_means, cate_temp)
    }
    cate_final <- cate_means
  } else {
    joined_ite_rules <- cbind(ite_inf, df_rules_factor)

    # Fit linear regression model with contr.treatment, then extract coefficients and confidence intervals
    options(contrasts = rep("contr.treatment", 2))
    model1_cate <- stats::lm(ite_inf ~ ., data = joined_ite_rules)
    model1_coef <- summary(model1_cate)$coef[,c(1,4)] %>% as.data.frame
    model1_ci <- stats::confint(model1_cate) %>% as.data.frame() %>% dplyr::filter(!is.na(.))

    # Generate model 1 data frame
    cate_reg_orig <- model1_coef %>% cbind(model1_ci)
    cate_reg_orig_names <- stringr::str_extract(row.names(cate_reg_orig), "`.*`") %>%
      stringr::str_remove_all("`")
    cate_reg_orig_names[1] <- "(Intercept)"
    cate_reg_orig$Rule <- cate_reg_orig_names
    row.names(cate_reg_orig) <- 1:nrow(cate_reg_orig)
    cate_reg_orig <- cate_reg_orig %>%
      dplyr::summarize(Rule, Model_Coef = Estimate, CATE = Estimate, PVal = `Pr(>|t|)`,
                       CI_lower = `2.5 %`, CI_upper = `97.5 %`)
    for (i in 2:nrow(cate_reg_orig)) {
      cate_reg_orig[i,3] <- cate_reg_orig[1,2] + cate_reg_orig[i,2]
    }
    cate_final <- cate_reg_orig
  }

  # Return final results
  return(cate_final)
}
