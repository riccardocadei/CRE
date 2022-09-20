# Test 1: estimating propensity score.
#

set.seed(458)
data_1 <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                               effect_size = 2, binary = FALSE)

# Initialize parameters
z <- data_1[["z"]]
X <- as.data.frame(data_1[["X"]])
ps_method <- "SL.xgboost"

#
#set.seed(233)
est_ps <- CRE:::estimate_ps(z, X, ps_method)

expect_equal(est_ps[1], 0.4576640129,tolerance = 0.000001)
expect_equal(est_ps[136], 0.06275125593,tolerance = 0.000001)
expect_equal(est_ps[522], 0.6012128592,tolerance = 0.000001)
expect_equal(est_ps[718], 0.3583947122,tolerance = 0.000001)
