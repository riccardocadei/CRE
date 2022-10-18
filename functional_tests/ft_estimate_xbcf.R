dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                               effect_size = 2, binary = FALSE)
# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
ps_method <- "SL.xgboost"
if (requireNamespace("XBCF", quietly = TRUE)) {
   ite_list <- estimate_ite_xbcf(y, z, X, ps_method)
}
