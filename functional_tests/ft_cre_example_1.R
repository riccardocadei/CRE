# Example 1


# Generate sample data
set.seed(200)
dataset_cont <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                     effect_size = 2, binary = FALSE)
y <- dataset_cont[["y"]]
z <- dataset_cont[["z"]]
X <- as.data.frame(dataset_cont[["X"]])
X_names <- names(as.data.frame(X))

method_params = list(ratio_dis = 0.25,
                     ite_method_dis="ipw",
                     include_ps_dis = TRUE,
                     ps_method_dis = "SL.xgboost",
                     ps_method_inf = "SL.xgboost",
                     ite_method_inf = "ipw",
                     include_ps_inf = TRUE,
                     include_offset = FALSE,
                     cate_method = "DRLearner",
                     cate_SL_library = "SL.xgboost",
                     filter_cate = FALSE,
                     offset_name = NA,
                     random_state = 3591)

hyper_params = list(ntrees_rf = 100,
                    ntrees_gbm = 50,
                    min_nodes = 20,
                    max_nodes = 5,
                    t = 0.025,
                    q = 0.8)

cre_obj <- cre(y, z, X, method_params, hyper_params)
plot(cre_obj)
