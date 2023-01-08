#' @title
#' A helper function for CRE object
#'
#' @description
#' A helper function to plot CRE object using ggplot2 package.
#'
#' @param object A CRE object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#'
autoplot.cre <- function(object, ...) {

  gg_labs <- gg_title <- NULL

  # collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)
  for (i in arg_names) {
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  cate <- object[["CATE"]]
  cate_method <- object[["method_params"]][["cate_method"]]

  # Handling global variable error.
  `%>%` <- magrittr::`%>%`
  Predictor <- Estimate <- Std_Error <- NULL
  Rule <- CI_lower <- CI_upper <- CATE <- NULL

  if (cate_method %in% c("poisson", "DRLearner")) {
    # Specify the width of the 95% confidence intervals
    interval_95 <- -stats::qnorm((1 - 0.95) / 2)

    # Plot
    g <- ggplot2::ggplot(data = cate) +
         ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
         ggplot2::geom_linerange(
                    ggplot2::aes(x = Rule,
                                 ymin = Estimate - Std_Error * interval_95,
                                 ymax = Estimate + Std_Error * interval_95),
                    lwd = 1,
                    position = ggplot2::position_dodge(width = 1 / 2))

    g <- g + ggplot2::geom_pointrange(
                   ggplot2::aes(x = Rule,
                                y = Estimate,
                                ymin = Estimate - Std_Error * interval_95,
                                ymax = Estimate + Std_Error * interval_95),
                                lwd = 1 / 2,
                                position = ggplot2::position_dodge(
                                                      width = 1 / 2),
                                shape = 21, fill = "WHITE") +
        ggplot2::xlab("Causal Decision Rule") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(
                   paste("CRE Plot:\nConditional Average Treatment Effects",
                         "Linear Decomposition",
                         "\nwith 95% Confidence Intervals\n\n",
                         "CATE Method: ", cate_method))

  } else if (cate_method %in% c("bart-baggr", "cf-means")) {
    # Plot
    g <- ggplot2::ggplot(data = cate) +
         ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
         ggplot2::geom_linerange(ggplot2::aes(x = Rule,
                                           ymin = CI_lower,
                                           ymax = CI_upper),
                              lwd = 1,
                              position = ggplot2::position_dodge(
                                                    width = 1 / 2)) +
      ggplot2::geom_pointrange(ggplot2::aes(x = Rule,
                                            y = CATE,
                                            ymin = CI_lower,
                                            ymax = CI_upper),
                               lwd = 1 / 2,
                               position = ggplot2::position_dodge(
                                                     width = 1 / 2),
                               shape = 21, fill = "WHITE") +
      ggplot2::xlab("Causal Decision Rule") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects",
                             "Linear Decomposition",
                             "\nwith 95% Confidence Intervals\n\n",
                             "CATE Method: ", cate_method))

  } else if (cate_method == "linreg") {
    # Plot
    g <- ggplot2::ggplot(data = cate) +
      ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
      ggplot2::geom_linerange(ggplot2::aes(x = Rule,
                                           ymin = CI_lower,
                                           ymax = CI_upper),
                              lwd = 1,
                              position = ggplot2::position_dodge(
                                                    width = 1 / 2)) +
      ggplot2::geom_pointrange(ggplot2::aes(x = Rule,
                                            y = Estimate,
                                            ymin = CI_lower,
                                            ymax = CI_upper),
                               lwd = 1 / 2,
                               position = ggplot2::position_dodge(
                                                     width = 1 / 2),
                               shape = 21, fill = "WHITE") +
      ggplot2::xlab("Causal Decision Rule") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects",
                             "Linear Decomposition",
                             "\nwith 95% Confidence Intervals\n\n",
                             "CATE Method: Linear Regression"))

  } else {
    stop("Error: Unrecognized CATE method.")
  }

  return(g)
}

#' @title
#' Extend generic plot functions for CRE class
#'
#' @description
#' A wrapper function to extend generic plot functions for CRE class.
#'
#' @param x  A CRE object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side
#' effects.
#'
#' @export
#'
plot.cre <- function(x, ...) {
  if (x[["M"]]["Select (significant)"] == 0) {
    message("Visualization not available (0 causal decision rules discovered).")
  } else {
    g <- ggplot2::autoplot(x, ...)
    print(g)
    invisible(g)
  }
}
