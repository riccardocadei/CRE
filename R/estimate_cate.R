#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Method for estimating the Conditional Average Treatment Effect given a standardized vector of Individual Treatment Effects, a standardized matrix of causal rules, a list of causal rules
#'
#' @param y_inf the outcome vector for the inference subsample
#' @param z_inf the treatment vector for the inference subsample
#' @param X_inf the covariate vector for the inference subsample
#' @param X_names the names of the covariates
#' @param include_offset whether or not to include an offset when estimating the ITE, for poisson only
#' @param offset_name the name of the offset, if it is to be included
#' @param rules_matrix_inf the standardized causal rules matrix for the inference subsample
#' @param select_rules_interpretable the list of select causal rules in terms of coviariate names
#'
#' @return a matrix of CATE estimates
#'
#' @export
#'
estimate_cate <- function(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
                          rules_matrix_inf, select_rules_interpretable) {
  colnames(rules_matrix_inf) <- select_rules_interpretable
  colnames(X_inf) <- X_names
  if (include_offset) {
    X_offset <- X_inf[,which(X_names == offset_name)]
    X_inf <- X_inf[,-which(X_names == offset_name)]

    # Fit gnm model
    conditional_gnm <- gnm::gnm(y_inf ~ offset(log(X_offset)) + z_inf + z_inf:rules_matrix_inf + X_inf,
                                family = poisson(link = "log"))
  } else {
    # Fit gnm model
    conditional_gnm <- gnm::gnm(y_inf ~ z_inf + z_inf:rules_matrix_inf + X_inf,
                                family = poisson(link = "log"))
  }
  cate_model <- summary(conditional_gnm)$coefficients
  cate_names <- rownames(cate_model) %>%
    stringr::str_remove_all("X_inf") %>%
    stringr::str_remove_all("rules_matrix_inf") %>%
    stringr::str_replace_all("z_inf", "treatment")

  cate_temp <- data.frame(Predictor = cate_names) %>%
    cbind(cate_model)
  names(cate_temp) <- c("Predictor", "Estimate", "Std_Error", "Z_Value", "P_Value")
  cate_final <- subset(cate_temp, cate_temp$P_Value < 0.05)
  rownames(cate_final) <- 1:nrow(cate_final)

  # Return final results
  return(cate_final)
}
