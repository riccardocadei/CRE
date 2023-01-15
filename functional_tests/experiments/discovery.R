set.seed(2022)
library(foreach)
library(doParallel)

# Set Experiment Parameter
n_rules <- 2
sample_size <- 2000
effect_sizes <- seq(0, 4, 0.2)
confoundings <- c("no","lin","nonlin")
ITE_estimators <- c("aipw","cf","bcf","slearner","tlearner","xlearner","bart")
n_seeds <- 100
ratio_dis <- 0.5

# Set Ground Truth
{
  if (n_rules==1) {
    dr <- c("x1>0.5 & x2<=0.5")
    em <- c("x1","x2")
  } else if (n_rules==2) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
    em <- c("x1","x2","x5","x6")
  } else if (n_rules==3) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5", "x4>0.5")
    em <- c("x1","x2","x5","x6","x4")
  } else if (n_rules==4) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5",
            "x4>0.5", "x5<=0.5 & x7>0.5 & x8<=0.5")
    em <- c("x1","x2","x5","x6","x4","x7","x8")
  } else {
    stop(paste("Synthtic dataset with", n_rules,"rules has not been",
               "implemented yet. Available 'n_rules' options: {1,2,3,4}."))
  }
}

# Set Method and Hyper Parameters
{
  method_params <- list(ratio_dis = ratio_dis,
                        ite_method_dis = "aipw",
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
                       max_nodes = 8,
                       max_depth = 3,
                       t_decay = 0.025,
                       t_ext = 0.01,
                       t_corr = 1,
                       t_pvalue = 0.05,
                       replace = TRUE,
                       stability_selection = TRUE,
                       cutoff = 0.9,
                       pfer = 1,
                       penalty_rl = 1)
}

# Set Cluster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Run Experiments (confounding x effect_size x method x seed)
for (confounding in confoundings) {
  print(paste("Confounding:",confounding))
  discovery <- data.frame(matrix(ncol = 9, nrow = 0))
  for(effect_size in effect_sizes){
    print(paste("Effect Size", effect_size))

    # CRE
    for (ITE_estimator in ITE_estimators){
      # CRE (estimator i)
      time.before <- proc.time()
      discovery_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
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
        X_names <- colnames(X)

        method_params[["ite_method_dis"]] <- ITE_estimator
        method_params[["ite_method_inf"]] <- ITE_estimator
        hyper_params[["pfer"]] <- 1/((effect_size+1))
        result <- cre(y, z, X, method_params, hyper_params)

        dr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
        metrics_dr <- evaluate(dr, dr_pred)

        em_pred <- extract_effect_modifiers(dr_pred, X_names)
        metrics_em <- evaluate(em, em_pred)

        method <- paste("CRE (", ITE_estimator, ")", sep = "")
        return(c(method, effect_size, seed,
                 metrics_dr$IoU,
                 metrics_dr$precision,
                 metrics_dr$recall,
                 metrics_em$IoU,
                 metrics_em$precision,
                 metrics_em$recall))
      }
      discovery <- rbind(discovery, discovery_i)
      time.after <- proc.time()
      print(paste("CRE -", ITE_estimator,"(Time: ",
                  round((time.after - time.before)[[3]],2), "sec)"))
    }

    # HCT
    time.before <- proc.time()
    discovery_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
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
      X_names <- colnames(X)

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
                             split.Honest = T, cv.Honest = T, maxdepth = 4)
      opt.cp <- fit.tree$cptable[,1][which.min(fit.tree$cptable[,4])]
      pruned <- prune(fit.tree, opt.cp)

      # Extract Causal Decision Rules
      rules <- as.numeric(row.names(pruned$frame[pruned$numresp]))
      rules.ctree <- vector("list",length(rules))
      for (k in rules[-1]){
        sub <- as.data.frame(matrix(NA,
                                    nrow = 1,
                                    ncol = nrow(as.data.frame(
                                               path.rpart(pruned,
                                                          node=k,
                                                          print.it = FALSE)))-1)
                                                )
        capture.output(for (h in 1:ncol(sub)){
          sub[,h] <- as.character(print(as.data.frame(
                                            path.rpart(pruned,
                                                       node=k,
                                                       print.it=FALSE))[h+1,1]))
          sub_pop <- noquote(paste(sub , collapse = " & "))
        })
        subset <- with(data_inf, data_inf[which(eval(parse(text=sub_pop))),])
        if (length(unique(subset$z_inf))!= 1){
          rules.ctree[[k]] <- sub_pop
        }
      }
      leaves_dfs <- rep(FALSE,length(rules))
      leaves_dfs[unique(pruned$where)] <- TRUE
      dr_pred <- unlist(rules.ctree[rules[leaves_dfs]])
      for (i in 1:length(dr_pred)){
        dr_pred[i] <- gsub("< ", "<=", dr_pred[i])
        dr_pred[i] <- gsub(">=", ">", dr_pred[i])
      }
      # Predict
      ite_pred <- predict(pruned, X)

      metrics_dr <- evaluate(dr,dr_pred)

      em_pred <- extract_effect_modifiers(dr_pred, X_names)
      metrics_em <- evaluate(em,em_pred)

      return(c("HCT", effect_size, seed,
               metrics_dr$IoU,
               metrics_dr$precision,
               metrics_dr$recall,
               metrics_em$IoU,
               metrics_em$precision,
               metrics_em$recall))
    }
    discovery <- rbind(discovery,discovery_i)
    time.after <- proc.time()
    print(paste("HCT (Time: ",round((time.after - time.before)[[3]],2), "sec)"))
  }
  colnames(discovery) <- c("method","effect_size","seed",
                           "dr_IoU","dr_Precision","dr_Recall",
                           "em_IoU","em_Precision","em_Recall")
  rownames(discovery) <- 1:nrow(discovery)

  # Save results
  results_dir <- "../functional_tests/experiments/results/"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  exp_name <- paste(sample_size,"s_",n_rules,"r_",confounding, sep="")
  file_dir <- paste(results_dir,"discovery_",exp_name,".RData", sep="")
  save(discovery,
       file=file_dir)
}

stopCluster(cl)
