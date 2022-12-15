# Generate sample data
set.seed(2021)
dataset <- generate_cre_dataset(n = 500,
                                rho = 0,
                                n_rules = 4,
                                p = 10,
                                effect_size = 2,
                                binary_covariates = TRUE,
                                binary_outcome = FALSE)

y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]

method_params = list(ratio_dis = 0.25,
                     ite_method_dis = "bart",
                     include_ps_dis = TRUE,
                     ps_method_dis = "SL.xgboost",
                     ps_method_inf = "SL.xgboost",
                     oreg_method_dis = "SL.xgboost",
                     oreg_method_inf = "SL.xgboost",
                     ite_method_inf = "bart",
                     include_ps_inf = TRUE,
                     cate_method = "linreg",
                     cate_SL_library = "SL.xgboost",
                     offset = NULL,
                     random_state = 3591)

hyper_params = list(intervention_vars = c(),
                    ntrees_rf = 200,
                    ntrees_gbm = 200,
                    node_size = 20,
                    max_nodes = 5,
                    max_depth = 15,
                    replace = TRUE,
                    max_decay = 0.025,
                    type_decay = 2,
                    t_ext = 0.01,
                    t_corr = 1,
                    t_pvalue = 0.05,
                    stability_selection = TRUE,
                    cutoff = 0.6,
                    pfer = 1,
                    penalty_rl = 1)

cre_result <- cre(y, z, X, method_params, hyper_params)
summary(cre_result, method_params, hyper_params)
plot(cre_result)
