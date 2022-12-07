set.seed(2021)
library(foreach)
library(doParallel)

# Set Experiment Parameter
sample_size <- 500
n_rules <- 2
effect_size <- 10
n_seeds <- 100
confounding <- "nc"
ITE_estimators <- c("ipw","aipw","sipw","cf","bcf")

exp_name <- paste(sample_size,"s_",n_rules,"r_",effect_size,"es_",confounding,
                  sep="")
seeds <- seq(1, n_seeds, 1)

# Set Method and Hyper Parameters
{
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
                       t_pvalue = 0.01,
                       replace = TRUE,
                       stability_selection = TRUE,
                       cutoff = 0.9,
                       pfer = 0.5,
                       penalty_rl = 1)
}

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

# Evaluate Estimation Performance (in parallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

estimation <- data.frame(matrix(ncol = 4, nrow = 0))
for (ITE_estimator in ITE_estimators){
  time.before = Sys.time()
  estimation_i <- foreach(seed = seeds, .combine=rbind) %dopar% {
    library("devtools")
    load_all()
    set.seed(seed)

    method_params[["ite_method_dis"]]<-ITE_estimator
    method_params[["ite_method_inf"]]<-ITE_estimator
    hyper_params[["pfer"]] <- 1/((effect_size+1))
    result <- cre(y, z, X, method_params, hyper_params)

    rmse <- sqrt(mean((ite - result$ite_pred)^2))
    method <- paste("CRE (",ITE_estimator,")", sep = "")
    return(c(method,effect_size,seed,rmse))
  }
  time.after = Sys.time()
  print(paste(ITE_estimator,"(Time: ",round(time.after - time.before,2), "sec)"))
  estimation <- rbind(estimation,estimation_i)
}
stopCluster(cl)
colnames(estimation) <- c("method","effect_size","seed","rmse")
rownames(estimation) <- 1:nrow(estimation)

# Save results
dir <- paste("../functional_tests/experiments/results/estimation_",
             exp_name,".rdata", sep="")
save(estimation,
     file=dir)
