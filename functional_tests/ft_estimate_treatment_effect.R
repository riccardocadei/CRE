# This file tests estimating treatment effect with different input parameters.


# ==============================================================================
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

# ==============================================================================
# Test 2: estimate_ite_ipw

set.seed(9045)
dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
ps_method <- "SL.xgboost"
ite_list <- estimate_ite_ipw(y, z, X, ps_method)

expect_equal(ite_list[1], -1.028127216, tolerance = 0.000001)
expect_equal(ite_list[33], -3.597401923, tolerance = 0.000001)
expect_equal(ite_list[267], -2.372387354, tolerance = 0.000001)
expect_equal(ite_list[675], -2.360399952, tolerance = 0.000001)

# ==============================================================================
# Test 3:
#
#
#
set.seed(182)
data_1 <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
                               effect_size = 2, binary = FALSE)

ite_method <- "ipw"
include_ps <- "TRUE"
ps_method <- "SL.xgboost"
oreg_method <- NA
ntrees <- 100
min_nodes <- 20
max_nodes <- 5
random_state <- 121


# Check for binary outcome
binary <- ifelse(length(unique(data_1$y)) == 2, TRUE, FALSE)

eite_1 <- estimate_ite(y = data_1$y, z=data_1$z, data_1$X,
                       ite_method, binary,
                       include_ps = include_ps,
                       ps_method = ps_method,
                       oreg_method = oreg_method,
                       random_state = random_state)


#===============================================================================



set.seed(17)
print(.Random.seed[1])

set.seed(1007)
print(.Random.seed[1])

set.seed(127)
print(.Random.seed[1])
