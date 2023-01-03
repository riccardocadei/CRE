#' @title
#' Discovery (performance) Evaluation
#'
#' @description
#' Computes different metrics for discovery evaluation.
#'
#' @param ground_truth List of true (CDR/EM) values.
#' @param prediction List of predicted (CDR/EM) values.
#'
#' @keywords internal
#'
#' @return
#' Intersection Over Union, Precision, Recall
#'
evaluate <- function(ground_truth, prediction){
  intersect <- intersect(prediction, ground_truth)
  union <- union(prediction, ground_truth)
  TP <- length(intersect)
  FP <- length(setdiff(prediction, ground_truth))
  FN <- length(setdiff(ground_truth, prediction))
  recall <- TP/(TP+FN) # quantity
  precision <- TP/(TP+FP) # quality
  IoU <- length(intersect) / length(union)
  evaluate <- list(recall = recall,
                   precision = precision,
                   IoU = IoU)
  return(evaluate)
}
