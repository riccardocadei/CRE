
set.seed(904561)
dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                               effect_size = 2, binary = FALSE)
# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
include_ps <- TRUE
ps_method <- "SL.xgboost"
ite_list <- estimate_ite_xbart(y, z, X, include_ps, ps_method)

