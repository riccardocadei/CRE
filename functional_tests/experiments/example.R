set.seed(201)
source("../functional_tests/experiments/utils.R")

# Set Method and Hyper Parameters
method_params <- list(ratio_dis = 0.5,
                      ite_method_dis="cf",
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      include_ps_dis = TRUE,
                      ite_method_inf = "cf",
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
                     ntrees_gbm = 0,
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

# Set Ground Truth
cdr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5", "x4<=0", "x5<=0.5 & x7>0.5 & x8<=0.5")
em <- c("x1","x2","x5","x6","x4","x7","x8")
if (n_rules==2){
  cdr <- cdr[1:2]
  em <- em[1:4]
}

# Generate Dataset
dataset <- generate_cre_dataset(n = 2000,
                                rho = 0,
                                p = 10,
                                effect_size = 1,
                                n_rules = 2,
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = "nc")
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]
ite <- dataset[["ite"]]
X_names <- colnames(X)

hyper_params[["pfer"]] <- 1/((effect_size+1))
result <- cre(y, z, X, method_params, hyper_params)
summary(result)
plot(result)

# Discovery
cdr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
metrics_cdr <- metrics(cdr,cdr_pred)
print(paste("Causal Decision Rules: ",
            "IoU=",round(metrics_cdr$IoU,2),
            ", Recall=",round(metrics_cdr$recall,2),
            ", Precision=",round(metrics_cdr$precision,2),
            sep=""))

em_pred <- extract_effect_modifiers(cdr_pred, X_names)
metrics_em <- metrics(em,em_pred)
print(paste("Effect Modifiers:      ",
            "IoU=",round(metrics_em$IoU,2),
            ", Recall=",round(metrics_em$recall,2),
            ", Precision=",round(metrics_em$precision,2),
            sep=""))

#Estimation
rmse <- sqrt(mean((ite - result$ite_pred)^2))
print(paste("RMSE: ", rmse))
