#' @title
#' Predict Individual Treatment Effect via Causal Rule Ensemble
#'
#' @description
#' Predict Individual Treatment Effect via Causal Rule Ensemble algorithm.
#'
#' @param CRE A cre object from running the CRE function.
#' @param X A covariate matrix (or data.frame)
#'
#' @return
#' Array with the estimated Individual Treatment Effects
#'
#' @export
#'
predict.cre <- function(CRE, X) {
  if (is.null(CRE$rules)){
    ite_pred <- rep(CRE$CATE$Estimate[1], times = nrow(X))
  } else {
    rules_matrix <- generate_rules_matrix(X, CRE$rules)
    rownames(CRE$CATE) <- CRE$CATE$Rule
    ite_pred <- rules_matrix %*% as.matrix(CRE$CATE[2:nrow(CRE$CATE),]["Estimate"])
                + CRE$CATE$Estimate[1]
  }
  return(ite_pred)
}
