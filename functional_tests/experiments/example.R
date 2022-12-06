set.seed(2021)
source("../functional_tests/experiments/utils.R")

# Set Method and Hyper Parameters
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

# Generate Dataset
dataset <- generate_syn_dataset(n = 1000,
                                rho = 0,
                                p = 10,
                                effect_size = 5,
                                n_rules = 2,
                                binary_covariates = TRUE,
                                binary_outcome = FALSE)
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]
ite <- dataset[["ite"]]
X_names <- colnames(X)


result <- cre(y, z, X, method_params, hyper_params)
summary(result)
plot(result)

# Discovery
cdr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
metrics_cdr <- metrics(cdr,cdr_pred)
print(paste("Causal Decision Rules: ",
            "IoU=",metrics_cdr$IoU,
            "Recall=",metrics_cdr$recall,
            "Precision=",metrics_cdr$precision,
            sep=""))

em_pred <- extract_effect_modifiers(cdr_pred, X_names)
metrics_em <- metrics(em,em_pred)
print(paste("EFfect Modifiers:      ",
            "IoU=",metrics_em$IoU,
            "Recall=",metrics_em$recall,
            "Precision=",metrics_em$precision,
            sep=""))

#Estimation
rmse <- sqrt(mean((ite - result$ite_pred)^2))
print(paste("RMSE: ", rmse))



