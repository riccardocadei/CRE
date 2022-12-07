metrics <- function(ground_truth, prediction){
  intersect = intersect(prediction, ground_truth)
  union = union(prediction, ground_truth)
  TP = length(intersect)
  FP = length(setdiff(prediction, ground_truth))
  FN = length(setdiff(ground_truth, prediction))
  recall = TP/(TP+FN) # quantity
  precision = TP/(TP+FP) # quality
  IoU = length(intersect) / length(union)
  metrics <- list(recall = recall,
                  precision = precision,
                  IoU = IoU)
  return(metrics)
}

extract_effect_modifiers <- function(rules_list, X_names) {
  effect_modifiers <- c()
  for (X_name in X_names) {
    if (any(grepl(X_name,rules_list))){
      effect_modifiers <- append(effect_modifiers, X_name)
    }
  }
  return(effect_modifiers)
}

generate_syn_dataset <- function(n = 1000, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 2, binary_covariates = TRUE,
                                 binary_outcome = TRUE, confounding=FALSE) {

  # Check for correct binary input
  if (!(binary_outcome %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }
  if (is.numeric(n) & !is.integer(n)){
    n <- as.integer(n)
  }

  # Generate Covariate Matrix
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  if (binary_covariates) { X <- stats::qbinom(pvars, 1, 0.5) }
  else { X <- pvars }
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Treatment Vector
  logit.prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Causal Rules and Potential Outcomes
  stopifnot(n_rules %in% c(2, 4))
  if (binary_outcome == TRUE){
    y0 <- rep(0, n)
    y1 <- rep(0, n)
    effect_size = 1
  }
  else {
    if (confounding) {mean = X$x1 + X$x3 + X$x4}
    else {mean = 0}
    y0 <- stats::rnorm(n, mean = mean, sd = 1)
    y1 <- y0
  }
  y0[X$x1 == 1 & X$x2 == 0] = effect_size
  y1[X$x5 == 1 & X$x6 == 0] = effect_size
  if (n_rules == 4) {
    y0[X$x4 == 1] = effect_size
    y1[X$x5 == 0 & X$x7 == 1 & X$x8 == 0] = effect_size
  }

  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Compute Individual Treatment Effect
  ite <- y1 - y0

  # Generate Observed Data
  dataset <- list(y = y, z = z, X = X, ite = ite)
  return(dataset)
}
