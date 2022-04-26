#' @title
#' The 'CRE' package.
#'
#' @description
#' Provides an interpretable identification of subgroups with
#' heterogeneous causal effect. The heterogeneous subgroups are
#' discovered through ensemble learning of causal rules. Causal rules are
#' highly interpretable if-then statement that recursively partition the
#' features space into heterogeneous subgroups. A small number of
#' significant causal rules are selected through Stability Selection to
#' control for family-wise error rate in the finite sample setting. It
#' proposes various estimation methods for the conditional causal effects
#' for each discovered causal rule.  It is highly flexible and multiple
#' causal estimands and imputation methods are implemented.
#'
#' @docType package
#' @name CRE-package
#' @aliases CRE
#'
#' # imports for inTrees package
#' @import stats
#' @import xtable
#' @import data.table
#' @importFrom RRF RRF
#' @importFrom RRF getTree
#' @importFrom gbm pretty.gbm.tree
#' @importFrom xgboost xgb.model.dt.tree
#' @import arules
#' @importFrom methods as
#'
#'
NULL
