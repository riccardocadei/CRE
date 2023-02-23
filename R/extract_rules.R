#' @title
#' Extract (causal) decision rules
#'
#' @description
#' Extracts causal rules from the random forest or the gradient
#' boosting algorithms.
#'
#' @param treelist A list of decision trees.
#' @param X Features matrix.
#' @param max_depth A number of top levels from each tree considered.
#' @param digits A Number of digits for rounding decision rules to extract
#' conditions.
#'
#' @keywords internal
#'
#' @return
#' A vector of (causal) decision rules.
#'
extract_rules <- function(treelist, X, max_depth, digits = 2) {

  if(is.numeric(digits)) digits <- as.integer(abs(digits))
  levelX <- list()
  for (iX in 1:ncol(X)) levelX <- c(levelX, list(levels(X[, iX])))
  ntree <- min(treelist$ntree)
  allRulesList <- list()
  for (iTree in 1:ntree) {
    rule <- list(); count <- 0; rowIx <- 1;
    tree <- treelist$list[[iTree]]
    if (nrow(tree) <= 1) next # skip if there is no split
    ruleSet <- vector("list", length(which(tree[, "status"] == -1)))
    res <- inTrees::treeVisit(tree,
                              rowIx = rowIx,
                              count,
                              ruleSet,
                              rule,
                              levelX,
                              length = 0,
                              max_length = max_depth,
                              digits = digits)
    allRulesList <- c(allRulesList, res$ruleSet)
  }

  allRulesList <- allRulesList[!unlist(lapply(allRulesList, is.null))]
  rules <- inTrees::ruleList2Exec(X, allRulesList)
  return(rules)
}
