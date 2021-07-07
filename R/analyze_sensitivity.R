#' @title
#' Sensitivity Analysis
#'
#' @description
#' Method for analyzing the sensitivity of the estimates of the causal rule-specific treatment effects
#'
#' @param ite_std the standardized ITE
#' @param rules_matrix_std the standardized causal rules matrix
#'
#' @return a list containing the results of the sensitivity analysis
#'
#' @export
#'
analyze_sensitivity <- function(ite_std, rules_matrix_std) {
  # Initialize parameters
  all_zero <- FALSE
  sensitivity_results <- list()
  i <- 0

  # Continue testing different values of lambda until all confidence intervals contain 0
  while (all_zero == FALSE) {
    lambda <- 1.0 + (0.01 * i)

    # Generate confidence intervals using current lambda
    # model_temp <- TBD
    ci_temp <- as.data.frame(confint(model_temp)) %>% filter(complete.cases(.))

    # Add confidence intervals to list
    sensitivity_results[[as.character(lambda)]] <- ci_temp

    # Check if 0 is present in all confidence intervals for that iteration
    zero_in_range <- vector(length = nrow(ci_temp))
    for (l in 1:nrow(ci_temp)) {
      zero_in_range[l] <- between(0, ci_temp[l,1], ci_temp[l,2])
    }
    if (all(zero_in_range)) {
      all_zero <- TRUE
    }

    # Increase i
    i <- i + 1
  }
  return(sensitivity_results)
}
