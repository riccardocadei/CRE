#' @title
#' The Causal Rule Ensemble Output Plot
#'
#' @description
#' Plots the results of the CRE function: select causal rules and CATE estimates
#'
#' @param cre_results the S3 object returned from the cre() function
#'
#' @return
#' Returns nothing; plots the CRE results using ggplot
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' cre_results <- cre(y = dataset[["y"]], z = dataset[["z"]],
#'                    X = as.data.frame(dataset[["X"]]), ratio_dis = 0.25,
#'                    ite_method_dis = "bart", include_ps_dis = TRUE,
#'                    ite_method_inf = "bart", include_ps_inf = TRUE,
#'                    ntrees_rf = 100, ntrees_gbm = 50, min_nodes = 20,
#'                    max_nodes = 5, t = 0.025, q = 0.8)
#'
#' plot_cre(cre_results)
#'
plot_cre <- function(cre_results) {

  cate_results <- cre_results[["CATE_results"]]
  cate_method <- cre_results[["CATE_method"]]

  if (cate_method %in% c("poisson", "DRLearner")) {
    # Specify the width of the 95% confidence intervals
    interval_95 <- -qnorm((1-0.95)/2)

    # Plot
    cate_results %>% ggplot() +
      geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
      geom_linerange(aes(x = Predictor,
                         ymin = Estimate - Std_Error*interval_95,
                         ymax = Estimate + Std_Error*interval_95),
                     lwd = 1, position = position_dodge(width = 1/2)) +
      geom_pointrange(aes(x = Predictor,
                          y = Estimate,
                          ymin = Estimate - Std_Error*interval_95,
                          ymax = Estimate + Std_Error*interval_95),
                      lwd = 1/2, position = position_dodge(width = 1/2),
                      shape = 21, fill = "WHITE") +
      coord_flip() +
      ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects Per Rule",
                    "\nwith 95% Confidence Intervals\n\n",
                    "CATE Method: ", cate_method))

  } else if (cate_method %in% c("bart-baggr", "cf-means")) {
    # Plot
    cate_results %>% ggplot() +
      geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
      geom_linerange(aes(x = Rule,
                         ymin = CI_lower,
                         ymax = CI_upper),
                     lwd = 1, position = position_dodge(width = 1/2)) +
      geom_pointrange(aes(x = Rule,
                          y = CATE,
                          ymin = CI_lower,
                          ymax = CI_upper),
                      lwd = 1/2, position = position_dodge(width = 1/2),
                      shape = 21, fill = "WHITE") +
      coord_flip() +
      ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects Per Rule",
                    "\nwith 95% Confidence Intervals\n\n",
                    "CATE Method: ", cate_method))

  } else if (cate_method == "linreg") {
    # Plot
    cate_results %>% ggplot() +
      geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
      geom_linerange(aes(x = Rule,
                         ymin = CI_lower,
                         ymax = CI_upper),
                     lwd = 1, position = position_dodge(width = 1/2)) +
      geom_pointrange(aes(x = Rule,
                          y = Estimate,
                          ymin = CI_lower,
                          ymax = CI_upper),
                      lwd = 1/2, position = position_dodge(width = 1/2),
                      shape = 21, fill = "WHITE") +
      coord_flip() +
      ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects Per Rule",
                    "\nwith 95% Confidence Intervals\n\n",
                    "CATE Method: ", cate_method))

  } else {
    stop("Error: Unrecognized CATE method.")
  }
}
