set.seed(2022)
library(foreach)
library(doParallel)

# Set Experiment Parameter
n_rules <- 2
sample_size <- 5000
effect_size <- 5
confoundings <- c("nc","lc","nlc")
ite_estimators <- c("ipw","aipw","sipw","cf","bcf")
n_seeds <- 196
ratio_dis <- 0.5

# Set Ground Truth
{
cdr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5",
         "x4<=0", "x5<=0.5 & x7>0.5 & x8<=0.5")
if (n_rules==2) { cdr<-cdr[1:2]
} else if (n_rules==4) {
} else {stop(paste("Synthtic dataset with", n_rules,"rules has not been
                    implemented yet. Set 'n_rules' equal to 2 or 4 (rules)."))}
}

# Set Method and Hyper Parameters
{
  method_params <- list(ratio_dis = ratio_dis,
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
                       t_pvalue = 1,
                       replace = TRUE,
                       stability_selection = TRUE,
                       cutoff = 0.9,
                       pfer = 0.5,
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
    time.before = Sys.time()
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
      X_names <- colnames(X)

      #method_params[["ite_method_dis"]]<-ite_estimator
      method_params[["ite_method_inf"]]<-ite_estimator
      hyper_params[["pfer"]] <- 1/((effect_size+1))
      result <- cre(y, z, X, method_params, hyper_params)

      rmse <- sqrt(mean((ite - result$ite_pred)^2))
      bias <- mean((ite - result$ite_pred))

      method <- paste("CRE (",ite_estimator,")", sep = "")
      betas <- c()
      for (rule in cdr){
        beta <- result$CATE$Estimate[result$CATE$Rule==rule]
        betas <- c(betas,beta)
      }
      return(c(method, effect_size, seed, rmse, bias,
               betas[1], betas[2], betas[3], betas[4]))
    }
    time.after = Sys.time()
    print(paste("CRE -", ite_estimator,"(Time: ",round(time.after - time.before,2), "sec)"))
    estimation <- rbind(estimation,estimation_i)
  }
  # CF
  time.before = Sys.time()
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

    ite_pred <- estimate_ite_cf(y, z, X, TRUE, "SL.xgboost")[[1]]

    rmse <- sqrt(mean((ite - ite_pred)^2))
    bias <- mean((ite - ite_pred))

    return(c("CF", effect_size, seed, rmse, bias,
             NA, NA, NA, NA))
  }
  time.after = Sys.time()
  print(paste("CF (Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)
  # BCF
  time.before = Sys.time()
  estimation_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
    library(devtools)
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

    ite_pred <- estimate_ite_bcf(y, z, as.matrix(X), "SL.xgboost")[[1]]

    rmse <- sqrt(mean((ite - ite_pred)^2))
    bias <- mean((ite - ite_pred))

    return(c("BCF", effect_size, seed, rmse, bias,
             NA, NA, NA, NA))
  }
  time.after = Sys.time()
  print(paste("BCF (Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)
  # IPW
  time.before = Sys.time()
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

    ite_pred <- estimate_ite_ipw(y, z, as.matrix(X), "SL.xgboost")

    rmse <- sqrt(mean((ite - ite_pred)^2))
    bias <- mean((ite - ite_pred))

    return(c("IPW", effect_size, seed, rmse, bias,
             NA, NA, NA, NA))
  }
  time.after = Sys.time()
  print(paste("IPW (Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)
  # AIPW
  time.before = Sys.time()
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

    ite_pred <- estimate_ite_aipw(y, z, as.matrix(X), "SL.xgboost")

    rmse <- sqrt(mean((ite - ite_pred)^2))
    bias <- mean((ite - ite_pred))

    return(c("AIPW", effect_size, seed, rmse, bias,
             NA, NA, NA, NA))
  }
  time.after = Sys.time()
  print(paste("AIPW (Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)
  # SIPW
  time.before = Sys.time()
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

    ite_pred <- estimate_ite_sipw(y, z, as.matrix(X), "SL.xgboost")

    rmse <- sqrt(mean((ite - ite_pred)^2))
    bias <- mean((ite - ite_pred))

    return(c("SIPW", effect_size, seed, rmse, bias,
             NA, NA, NA, NA))
  }
  time.after = Sys.time()
  print(paste("SIPW (Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)
  # HCT
  time.before = Sys.time()
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
  time.after = Sys.time()
  print(paste("HCT (Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)


  colnames(estimation) <- c("method","effect_size","seed","rmse","bias",
                            "beta1", "beta2","beta3","beta4")
  rownames(estimation) <- 1:nrow(estimation)

  # Save results
  exp_name <- paste(sample_size,"s_",n_rules,"r_",effect_size,"es_",confounding,
                    sep="")
  dir <- paste("../functional_tests/experiments/results/estimation_",
               exp_name,".rdata", sep="")
  save(estimation,
       file=dir)
}

stopCluster(cl)
