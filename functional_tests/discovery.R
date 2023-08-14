set.seed(2022)
library("foreach")
library("doParallel")
library("CRE")

# Set Experiment Parameter
experiment <- "main"
cutoff <- 0.8

if (experiment=="main") {
  n_rules <- 2
  sample_size <- 2000
  confounding <- "lin"
  dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
  em <- c("x1","x2","x5","x6")
  max_depth <- 2
} else if (experiment=="small_sample") {
  n_rules <- 2
  sample_size <- 1000
  confounding <- "lin"
  dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
  em <- c("x1","x2","x5","x6")
  max_depth <- 2
} else if (experiment=="big_sample") {
  n_rules <- 2
  sample_size <- 5000
  confounding <- "lin"
  dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
  em <- c("x1","x2","x5","x6")
  max_depth <- 2
} else if (experiment=="more_rules") {
  n_rules <- 4
  sample_size <- 2000
  confounding <- "lin"
  dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5",
          "x4>0.5", "x5<=0.5 & x7>0.5 & x8<=0.5")
  em <- c("x1","x2","x5","x6","x4","x7","x8")
  max_depth <- 3
} else if (experiment=="rct") {
  n_rules <- 2
  sample_size <- 2000
  confounding <- "no"
  dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
  em <- c("x1","x2","x5","x6")
  max_depth <- 2
} else if (experiment=="nonlin_conf") {
  n_rules <- 2
  sample_size <- 2000
  confounding <- "nonlin"
  dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
  em <- c("x1","x2","x5","x6")
  max_depth <- 2
} else if (experiment=="personalize") {
  n_rules <- NA
  sample_size <- NA
  confounding <- NA
  dr <- NA
  em <- NA
  max_depth <- NA
  stop("'personalize' esperiment non defined yet.")
} else {
  stop(paste("'",experiment,"' experiment doesn't exist.", sep=""))
}


# Other Setting
{
  n_seeds <- 250
  ratio_dis <- 0.5
  effect_sizes <- seq(0, 5, 0.2)
  ITE_estimators <- c("aipw","cf","slearner","tlearner","xlearner","bart")

  method_params <- list(ratio_dis = ratio_dis,
                        ite_method = "aipw",
                        learner_ps = "SL.xgboost",
                        learner_y = "SL.xgboost")

  hyper_params <- list(intervention_vars = NULL,
                       offset = NULL,
                       ntrees = 40,
                       node_size = 20,
                       max_rules = 50,
                       max_depth = max_depth,
                       t_decay = 0.025,
                       t_ext = 0.01,
                       t_corr = 1,
                       t_pvalue = 0.05,
                       stability_selection = TRUE,
                       cutoff = cutoff,
                       pfer = 1,
                       B = 20,
                       subsample = 0.2)

  evaluate <- function(ground_truth, prediction) {
    intersect <- intersect(prediction, ground_truth)
    union <- union(prediction, ground_truth)
    TP <- length(intersect)
    FP <- length(setdiff(prediction, ground_truth))
    FN <- length(setdiff(ground_truth, prediction))
    recall <- TP / (TP + FN) # quantity
    precision <- TP / (TP + FP) # quality
    IoU <- length(intersect) / length(union)
    evaluate <- list(recall = recall,
                     precision = precision,
                     IoU = IoU)
    return(evaluate)
  }
  extract_effect_modifiers <- function(rules_list, X_names) {
    effect_modifiers <- c()
    for (X_name in X_names) {
      if (any(grepl(X_name, rules_list))) {
        effect_modifiers <- append(effect_modifiers, X_name)
      }
    }
    return(effect_modifiers)
  }
}


# Set Cluster
cl <- makeCluster(detectCores(), type="PSOCK")
clusterExport(cl, ls(globalenv()))
clusterEvalQ(cl, library("CRE"))
registerDoParallel(cl)

discovery <- data.frame(matrix(ncol = 9, nrow = 0))
for(effect_size in effect_sizes){
  print(paste("Effect Size", effect_size))
  for (ITE_estimator in ITE_estimators){
    # CRE (estimator i)
    method <- paste("CRE (", ITE_estimator, ")", sep = "")
    time.before <- proc.time()
    discovery_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
      library("CRE")
      set.seed(seed)

      # Generate Dataset
      dataset <- generate_cre_dataset(n = sample_size,
                                      rho = 0,
                                      p = 10,
                                      effect_size = effect_size,
                                      n_rules = n_rules,
                                      binary_covariates = TRUE,
                                      binary_outcome = FALSE,
                                      confounding = confounding)
      y <- dataset[["y"]]
      z <- dataset[["z"]]
      X <- dataset[["X"]]
      X_names <- colnames(X)

      method_params[["ite_method_dis"]] <- ITE_estimator
      method_params[["ite_method_inf"]] <- ITE_estimator
      hyper_params[["pfer"]] <- n_rules/(effect_size+1)
      metrics <- tryCatch({
        result <- cre(y, z, X, method_params, hyper_params)

        dr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
        metrics_dr <- evaluate(dr, dr_pred)
        em_pred <- extract_effect_modifiers(dr_pred, X_names)
        metrics_em <- evaluate(em, em_pred)

        c(method, effect_size, seed,
          metrics_dr$IoU, metrics_dr$precision, metrics_dr$recall,
          metrics_em$IoU, metrics_em$precision, metrics_em$recall)
      },
      error = function(e) {
        c(method, effect_size, seed, NaN,NaN,NaN,NaN,NaN,NaN)
      })
      return(metrics)
    }
    discovery <- rbind(discovery, discovery_i)
    time.after <- proc.time()
    print(paste("CRE -", ITE_estimator,"(Time: ",
                round((time.after - time.before)[[3]],2), "sec)"))
  }
}

colnames(discovery) <- c("method","effect_size","seed",
                         "dr_IoU","dr_Precision","dr_Recall",
                         "em_IoU","em_Precision","em_Recall")
rownames(discovery) <- 1:nrow(discovery)

# Save results
results_dir <- "~/CRE/functional_tests/results/"
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}
exp_name <- paste("discovery",experiment,"cutoff",cutoff, sep="_")
file_dir <- paste(results_dir,exp_name,".RData", sep="")
save(discovery, file=file_dir)

stopCluster(cl)
