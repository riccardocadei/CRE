set.seed(2022)
#library(causalTree)
#library(PSweight)
#library(stringr)
library(foreach)
library(doParallel)

# Set Experiment Parameter
n_rules <- 2
sample_size <- 2000
effect_sizes <- seq(0, 10, 0.2)
confoundings <- c("nc","lc","nlc")
ITE_estimators <- c("ipw","aipw","sipw","cf","bcf")
n_seeds <- 48

# Set Ground Truth
{
cdr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5",
         "x4<=0", "x5<=0.5 & x7>0.5 & x8<=0.5")
em <- c("x1","x2","x5","x6","x4","x7","x8")
if (n_rules==2) {
  cdr<-cdr[1:2]
  em <- em[1:4]
} else if (n_rules==4) {
} else {stop(paste("Synthtic dataset with", n_rules,"rules has not been
                    implemented yet. Set 'n_rules' equal to 2 or 4 (rules)."))}
}

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

# Set Cluster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Run Experiments (confounding x effect_size x method x seed)
for (confounding in confoundings) {
  print(paste("Confounding:",confounding))
  discovery <- data.frame(matrix(ncol = 9, nrow = 0))
  for(effect_size in effect_sizes){
    print(paste("Effect Size", effect_size))

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

    discovery_i <- data.frame(matrix(ncol = 9, nrow = 0))
    for (ITE_estimator in ITE_estimators){
      time.before = Sys.time()
      discovery_i <- foreach(seed = seq(1, n_seeds, 1), .combine=rbind) %dopar% {
        library("devtools")
        load_all()
        set.seed(seed)

        method_params[["ite_method_dis"]]<-ITE_estimator
        method_params[["ite_method_inf"]]<-ITE_estimator
        hyper_params[["pfer"]] <- 1/((effect_size+1))
        result <- cre(y, z, X, method_params, hyper_params)

        cdr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
        metrics_cdr <- evaluate(cdr,cdr_pred)

        em_pred <- extract_effect_modifiers(cdr_pred, X_names)
        metrics_em <- evaluate(em,em_pred)

        method <- paste("CRE (",ITE_estimator,")", sep = "")
        return(c(method, effect_size, seed,
                 metrics_cdr$IoU,
                 metrics_cdr$precision,
                 metrics_cdr$recall,
                 metrics_em$IoU,
                 metrics_em$precision,
                 metrics_em$recall))
      }
      discovery <- rbind(discovery,discovery_i)
      time.after = Sys.time()
      print(paste(ITE_estimator,"(Time: ",round(time.after - time.before,2), "sec)"))
    }
  }
  colnames(discovery) <- c("method","effect_size","seed",
                           "cdr_IoU","cdr_Precision","cdr_Recall",
                           "em_IoU","em_Precision","em_Recall")
  rownames(discovery) <- 1:nrow(discovery)

  # Save results
  exp_name <- paste(sample_size,"s_",n_rules,"r_",confounding, sep="")
  dir <- paste("../functional_tests/experiments/results/discovery_",
               exp_name,".rdata", sep="")
  save(discovery,
       file=dir)
}

stopCluster(cl)
