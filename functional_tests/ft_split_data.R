dataset <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
                               effect_size = 2, binary = FALSE)
# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
ratio_dis <- 0.25
# Split data
X <- as.matrix(X)
y <- as.matrix(y)
z <- as.matrix(z)
subgroups <- CRE:::split_data(y, z, X, ratio_dis)
discovery <- subgroups[[1]]
inference <- subgroups[[2]]
