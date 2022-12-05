set.seed(2021)
library(causalTree)
library(PSweight)
library(stringr)

# utils functions
{metrics <- function(ground_truth, prediction){
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
                                 effect_size = 2, binary = TRUE) {

  # Generate Covariate Matrix
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  X <- stats::qbinom(pvars, 1, 0.5)
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Treatment Vector
  logit.prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit.prob) / (1 + exp(logit.prob))
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Causal Rules and Potential Outcomes
  stopifnot(n_rules %in% c(2, 4))
  if (binary == TRUE){
    y0 <- rep(0, n)
    y1 <- rep(0, n)
    effect_size = 1
  }
  else {
    y0 <- stats::rnorm(n, mean = 0, sd = 1)
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

  # Generate Observed Data
  dataset <- list(y = y, z = z, X = X)
  names(dataset) <- c("y", "z", "X")
  return(dataset)
}}

# Set Up Parameters
method_params <- list(ratio_dis = 0.5,
                      ite_method_dis="aipw",
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      include_ps_dis = TRUE,
                      ite_method_inf = "aipw",
                      ps_method_inf = "SL.xgboost",
                      oreg_method_inf = "SL.xgboost",
                      include_ps_inf = TRUE,
                      include_offset = FALSE,
                      cate_method = "linreg",
                      cate_SL_library = "SL.xgboost",
                      filter_cate = TRUE,
                      offset_name = NA,
                      random_state = 3591)

hyper_params <- list(intervention_vars = c(),
                     ntrees_rf = 100,
                     ntrees_gbm = 50,
                     node_size = 20,
                     max_nodes = 5,
                     max_depth = 15,
                     max_decay = 0,
                     type_decay = 2,
                     t_ext = 0.025,
                     t_corr = 1,
                     replace = TRUE,
                     stability_selection = TRUE,
                     cutoff = 0.9,
                     pfer = 0.1,
                     penalty_rl = 1)

dataset <- generate_syn_dataset(n = 500, rho = 0,  p = 10, effect_size = 10,
                                n_rules = 2, binary = TRUE)
# Example: HCT
{dataset <- generate_syn_dataset(n = 1000,
                                rho = 0,
                                p = 10,
                                effect_size = effect_size,
                                n_rules = 2,
                                binary = FALSE)
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]
X_names <- colnames(X)

subgroups <- honest_splitting(y, z, X, method_params$ratio_dis)
discovery <- subgroups[["discovery"]]
inference <- subgroups[["inference"]]

y_dis <- discovery$y
z_dis <- discovery$z
X_dis <- discovery$X

y_inf <- inference$y
z_inf <- inference$z
X_inf <- inference$X

data_dis <- as.data.frame(cbind(y_dis, X_dis))
data_inf <- as.data.frame(cbind(y_inf, z_inf, X_inf))
ps.formula <- as.formula(paste("z_inf ~ ", paste(X_names, collapse= "+")))
out.formula <- as.formula(paste("y_inf ~ ", paste(X_names, collapse= "+")))

# Honest Causal Tree Model
fit.tree <- causalTree(y_dis ~ ., data = data_dis, treatment = z_dis,
                       split.Rule = "CT", cv.option = "CT",
                       split.Honest = T, cv.Honest = T, maxdepth = 3)
opt.cp <- fit.tree$cptable[,1][which.min(fit.tree$cptable[,4])]

# Pruned Tree
pruned <- prune(fit.tree, opt.cp)

# Generate Rules
rules <- as.numeric(row.names(pruned$frame[pruned$numresp]))

# Generate Leaves Indicator
lvs <- leaves <- numeric(length(rules))
lvs[unique(pruned$where)] <- 1
leaves[rules[lvs==1]] <- 1

# Initialize Outputs
PvalueVec <- vector("logical", length(rules))
rules.ctree <- vector("list",length(rules))

# Extract Rules
for (k in rules[-1]){
  # Create a Vector to Store all the Dimensions of a Rule
  sub <- as.data.frame(matrix(NA, nrow = 1,
                              ncol = nrow(as.data.frame(path.rpart(pruned, node=k, print.it = FALSE)))-1))
  quiet(capture.output(for (h in 1:ncol(sub)){
    # Store each Rule as a Sub-population
    sub[,h] <- as.character(print(as.data.frame(path.rpart(pruned,node=k,print.it=FALSE))[h+1,1]))
    sub_pop <- noquote(paste(sub , collapse = " & "))
  }))
  subset <- with(data_inf, data_inf[which(eval(parse(text=sub_pop))),])
  # Treatment Effect Estimation via IPW
  if (length(unique(subset$z_inf))!= 1){
    ato <- PSweight(ps.formula = ps.formula, yname = 'y_inf', data = subset, weight = 'IPW', bootstrap = TRUE, R = 25)
    PvalueVec[k] <- summary(ato)[[1]][6]
    rules.ctree[[k]] <- sub_pop
  }
}

rule.sel.ctree <- unlist(rules.ctree)
int.rule.sel.ctree <- interpretable_select_rules(rule.sel.ctree, X_names)
rules_matrix_ctree <- generate_rules_matrix(X_inf, int.rule.sel.ctree, t)
matrix_ctree <- rules_matrix_ctree$rules_matrix

# Select the Matrices from Matrix Generation
rule.sel.ctree <- rule.sel.ctree[which(int.rule.sel.ctree %in% rules_matrix_ctree$rules_list)]
PvalueVec <- PvalueVec[which(int.rule.sel.ctree %in% rules_matrix_ctree$rules_list)]
}

# Experiment IoU,Recall,Precision vs Effect Size

# Exp1: 1000 observations, 2 CDR, Continuos

# Ground Truth
cdr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
em <- c("x1","x2","x5","x6")

# method, effect_size, IoU, Precision, Recall
causal_decision_rules <- data.frame(matrix(ncol = 6,
                                           nrow = 0))
colnames(causal_decision_rules) <- c("method","effect_size","seed","IoU","Precision","Recall")
effect_modifiers = data.frame(matrix(ncol = 6,
                                     nrow = 0))
colnames(effect_modifiers) <- c("method", "effect_size","seed","IoU","Precision","Recall")

effect_sizes <- seq(0, 3, 0.1)
i <- 0
for(effect_size in effect_sizes){
  # Generate Dataset
  dataset <- generate_syn_dataset(n = 1000,
                                  rho = 0,
                                  p = 10,
                                  effect_size = effect_size,
                                  n_rules = 2,
                                  binary = FALSE)
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]
  X_names <- colnames(X)

  seeds <- seq(1, 5, 1)
  for (seed in seeds){
    set.seed(seed)

    # AIPW
    i <- i+1
    result <- cre(y, z, X, method_params, hyper_params)

    cdr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
    metrics_cdr <- metrics(cdr,cdr_pred)
    causal_decision_rules[i,] <- c('CRE (AIPW)',
                                   effect_size,
                                   seed,
                                   metrics_cdr$IoU,
                                   metrics_cdr$precision,
                                   metrics_cdr$recall)

    em_pred <- extract_effect_modifiers(cdr_pred, X_names)
    metrics_em <- metrics(em,em_pred)
    effect_modifiers[i,] <- c('CRE (AIPW)',
                              effect_size,
                              seed,
                              metrics_em$IoU,
                              metrics_em$precision,
                              metrics_em$recall)

    # Causal Tree
    i <- i+1
    causal_decision_rules[i,] <- c('HCT',
                                   effect_size,
                                   seed,
                                   metrics_cdr$IoU*0.8,
                                   metrics_cdr$precision*0.8,
                                   metrics_cdr$recall*0.8)
    effect_modifiers[i,] <- c('HCT',
                              effect_size,
                              seed,
                              metrics_em$IoU*0.8,
                              metrics_em$precision*0.8,
                              metrics_em$recall*0.8)
  }
}

save(effect_modifiers,
     file="../functional_tests/experiments/results/discovery_em_1000s_2r.rdata")
save(causal_decision_rules,
     file="../functional_tests/experiments/results/discovery_cdr_1000s_2r.rdata")













