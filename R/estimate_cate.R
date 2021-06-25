#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Method for estimating the Conditional Average Treatment Effect given a standardized vector of Individual Treatment Effects, a standardized matrix of causal rules, a list of causal rules
#'
#' @param ite_std_inf the standardized ITE for the inference subsample
#' @param rules_matrix_std_inf the standardized causal rules matrix for the inference subsample
#' @param rules_list_inf the list of causal rules for the inference subsample
#'
#' @return two sets of CATE estimates, one with contrasts and one without
#'
#' @export
#'
#' @examples
#' TBD
#'
estimate_cate <- function(ite_std_inf, rules_matrix_std_inf, rules_list_inf) {
  # Convert rules matrix to a data frame and convert columns to factors
  rules_df_factor <- as.data.frame(rules_matrix_inf)
  for (i in 1:ncol(rules_df_factor)) {
    rules_df_factor[,i] <- as.factor(rules_df_factor[,i])
  }
  # Check that matrix and list of rules have same length
  stopifnot(ncol(rules_matrix_inf) == length(rules_list_inf))
  names(rules_df_factor) <- rules_list_inf
  joined_ite_rules <- cbind(ite_inf, rules_df_factor)

  # Fit model without contrasts, extract coefficients and confidence intervals
  model1_cate <- stats::lm(ite_inf ~ ., data = joined_ite_rules)
  model1_coef <- summary(model1_cate)$coef[,c(1,4)] %>% as.data.frame
  model1_ci <- confint(model1_cate) %>% as.data.frame() %>% filter(!is.na(.))

  # Fit sum-to-zero contrasts model, extract coefficients and confidence intervals
  contrasts_list <- lapply(names(rules_df_factor), function(X) X = "code_deviation")
  names(contrasts_list) <- names(rules_df_factor)
  model2_cate <- stats::update(model1_cate, contrasts = contrasts_list)
  model2_coef <- summary(model2_cate)$coef[,c(1,4)] %>% as.data.frame
  model2_ci <- confint(model2_cate) %>% as.data.frame() %>% filter(!is.na(.))

  # Generate model 1 results
  cate_1 <- model1_coef %>% cbind(model1_ci)
  cate_1_names <- str_extract(row.names(cate_1), "`.*`") %>% str_remove_all("`")
  cate_1_names[1] <- "(Intercept)"
  cate_1$Rule <- cate_1_names
  row.names(cate_1) <- 1:nrow(cate_1)

  # Generate model 2 results
  cate_2 <- model2_coef %>% cbind(model2_ci)
  cate_2_names <- str_extract(row.names(cate_2), "`.*`") %>% str_remove_all("`")
  cate_2_names[1] <- "(Intercept)"
  cate_2$Rule <- cate_2_names
  row.names(cate_2) <- 1:nrow(cate_2)

  # Return final results
  cates <- list(cate_original = cate_1, cate_contrast = cate_2)
  return(cates)
}
