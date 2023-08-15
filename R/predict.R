#' @title
#' Predict Individual Treatment Effect via Causal Rule Ensemble
#'
#' @description
#' Predict Individual Treatment Effect via Causal Rule Ensemble algorithm.
#'
#' @param object A cre object from running the CRE function.
#' @param X A covariate matrix (or data.frame)
#'
#' @return
#' Array with the estimated Individual Treatment Effects
#'
#' @export
#'
predict.cre <- function(object, X) {
  if (is.null(object$rules)){
    ite_pred <- rep(object$CATE$Estimate[1], times = nrow(X))
  } else {
    rules_matrix <- generate_rules_matrix(X, object$rules)
    rownames(object$CATE) <- object$CATE$Rule
    ite_pred <- rules_matrix %*% as.matrix(object$CATE[2:nrow(object$CATE),]["Estimate"])
                + object$CATE$Estimate[1]
  }
  return(ite_pred)
}
