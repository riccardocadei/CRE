set.seed(2021)

# Set Experiment Parameter
n_rules <- 2
sample_size <- 2000
effect_size <- 2
confounding <- "no"
ite_estimator_dis <- "aipw"
ite_estimator_inf <- "aipw"
#pfer <- 1
pfer <- 1/((effect_size+1))

# Set Method and Hyper Parameters
method_params <- list(ratio_dis = 0.5,
                      ite_method_dis = ite_estimator_dis,
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      include_ps_dis = TRUE,
                      ite_method_inf = ite_estimator_inf,
                      ps_method_inf = "SL.xgboost",
                      oreg_method_inf = "SL.xgboost",
                      include_ps_inf = TRUE,
                      cate_method = "linreg",
                      cate_SL_library = "SL.xgboost",
                      filter_cate = TRUE,
                      offset = NULL)

hyper_params <- list(intervention_vars = NULL,
                     ntrees_rf = 100,
                     ntrees_gbm = 0,
                     node_size = 20,
                     max_nodes = 5,
                     max_depth = 3,
                     max_decay = 0,
                     type_decay = 2,
                     t_ext = 0.025,
                     t_corr = 1,
                     t_pvalue = 0.01,
                     replace = TRUE,
                     stability_selection = TRUE,
                     cutoff = 0.9,
                     pfer = pfer,
                     penalty_rl = 1)

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

result <- cre(y, z, X, method_params, hyper_params)
summary(result, method_params, hyper_params)
plot(result)

# Discovery
cdr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
metrics_cdr <- evaluate(cdr,cdr_pred)
print(paste("Causal Decision Rules: ",
            "IoU=",round(metrics_cdr$IoU,2),
            ", Recall=",round(metrics_cdr$recall,2),
            ", Precision=",round(metrics_cdr$precision,2),
            sep=""))

em_pred <- extract_effect_modifiers(cdr_pred, X_names)
metrics_em <- evaluate(em,em_pred)
print(paste("Effect Modifiers:      ",
            "IoU=",round(metrics_em$IoU,2),
            ", Recall=",round(metrics_em$recall,2),
            ", Precision=",round(metrics_em$precision,2),
            sep=""))

#Estimation
rmse <- sqrt(mean((ite - result$ite_pred)^2))
print(paste("RMSE: ", rmse))
bias <- mean((ite - result$ite_pred))
print(paste("Bias: ", bias))
