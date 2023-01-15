set.seed(2022)
library(foreach)
library(doParallel)

# Set Experiment Parameter
n_rules <- 2
sample_size <- 5000
effect_size <- 5
confoundings <- c("no","lin","nonlin")
ite_estimators <- c("aipw","cf","bcf","slearner","tlearner","xlearner","bart")
n_seeds <- 200
ratio_dis <- 0.5
pfer <- n_rules/(effect_size+1)

# Set Ground Truth
{
  if (n_rules==1) {
    dr <- c("x1>0.5 & x2<=0.5")
  } else if (n_rules==2) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
  } else if (n_rules==3) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5", "x4>0.5")
  } else if (n_rules==4) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5",
            "x4>0.5", "x5<=0.5 & x7>0.5 & x8<=0.5")
  } else {
    stop(paste("Synthtic dataset with", n_rules,"rules has not been",
               "implemented yet. Available 'n_rules' options: {1,2,3,4}."))
  }
}

# Set Method and Hyper Parameters
{
  method_params <- list(ratio_dis = ratio_dis,
                        ite_method_dis="aipw",
                        ps_method_dis = "SL.xgboost",
                        oreg_method_dis = "SL.xgboost",
                        ite_method_inf = "aipw",
                        ps_method_inf = "SL.xgboost",
                        oreg_method_inf = "SL.xgboost")

  hyper_params <- list(intervention_vars = NULL,
                       offset = NULL,
                       ntrees_rf = 40,
                       ntrees_gbm = 40,
                       node_size = 20,
                       max_nodes = 4,
                       max_depth = 2,
                       t_decay = 0.025,
                       t_ext = 0.01,
                       t_corr = 1,
                       t_pvalue = 0.05,
                       replace = TRUE,
                       stability_selection = TRUE,
                       cutoff = 0.8,
                       pfer = pfer,
                       penalty_rl = 1)
}

# Set Cluster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Run Experiments (confounding x method x seed)
for (confounding in confoundings) {
  print(paste("Confounding:",confounding))
  estimation <- data.frame(matrix(ncol = 9, nrow = 0))
  # CRE
  for (ite_estimator in ite_estimators){
    # CRE (estimator i)
    time.before <- proc.time()
    estimation_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
      library("devtools")
      load_all()
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
      ite <- dataset[["ite"]]

      method_params[["ite_method_dis"]]<-ite_estimator
      method_params[["ite_method_inf"]]<-ite_estimator
      result <- cre(y, z, X, method_params, hyper_params)

      rmse <- sqrt(mean((ite - result$ite_pred)^2))
      bias <- mean((ite - result$ite_pred))

      method <- paste("CRE (",ite_estimator,")", sep = "")
      betas <- c()
      for (rule in dr){
        beta <- result$CATE$Estimate[result$CATE$Rule==rule]
        betas <- c(betas,beta)
      }
      return(c(method, effect_size, seed, rmse, bias,
               betas[1], betas[2], betas[3], betas[4]))
    }
    time.after <- proc.time()
    print(paste("CRE -", ite_estimator,
                "(Time: ",round((time.after - time.before)[[3]],2), "sec)"))
    estimation <- rbind(estimation,estimation_i)
  }
  # Benchmarks
  for (ite_estimator in ite_estimators){
    # estimator i
    time.before <- proc.time()
    estimation_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
      library("devtools")
      load_all()
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
      ite <- dataset[["ite"]]

      ite_pred <- estimate_ite(y , z, X, ite_estimator,
                               oreg_method = "SL.xgboost",
                               ps_method = "SL.xgboost")

      rmse <- sqrt(mean((ite - ite_pred)^2))
      bias <- mean((ite - ite_pred))

      return(c(ite_estimator, effect_size, seed, rmse, bias, NA, NA, NA, NA))
    }
    time.after <- proc.time()
    print(paste(ite_estimator,"(Time: ",round((time.after - time.before)[[3]],2), "sec)"))
    estimation <- rbind(estimation,estimation_i)
  }
  # HCT
  time.before <- proc.time()
  estimation_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
    library(devtools)
    library(causalTree)
    load_all()
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
    ite <- dataset[["ite"]]

    subgroups <- honest_splitting(y, z, X, ratio_dis)
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

    fit.tree <- causalTree(y_dis ~ ., data = data_dis, treatment = z_dis,
                           split.Rule = "CT", cv.option = "CT",
                           split.Honest = T, cv.Honest = T, maxdepth = 3)
    opt.cp <- fit.tree$cptable[,1][which.min(fit.tree$cptable[,4])]
    pruned <- prune(fit.tree, opt.cp)

    ite_pred <- predict(pruned, X)

    rmse <- sqrt(mean((ite - ite_pred)^2))
    bias <- mean((ite - ite_pred))

    return(c("HCT", effect_size, seed, rmse, bias,
             NA, NA, NA, NA))
  }
  time.after <- proc.time()
  print(paste("HCT (Time: ",round((time.after - time.before)[[3]],2), "sec)"))
  estimation <- rbind(estimation,estimation_i)

  colnames(estimation) <- c("method","effect_size","seed","rmse","bias",
                            "beta1", "beta2","beta3","beta4")
  rownames(estimation) <- 1:nrow(estimation)

  # Save results
  results_dir <- "../functional_tests/experiments/results/"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  exp_name <- paste(sample_size,"s_",n_rules,"r_",effect_size,"es_",confounding,
                    sep="")
  file_dir <- paste(results_dir,"estimation_",exp_name,".RData", sep="")
  save(estimation, file=file_dir)
}

stopCluster(cl)
