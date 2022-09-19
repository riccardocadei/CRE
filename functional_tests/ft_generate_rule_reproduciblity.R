# Test for reproducibility

set.seed(181)
dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                     effect_size = 2, binary = FALSE)
y <- dataset_cont[["y"]]
z <- dataset_cont[["z"]]
X <- dataset_cont[["X"]]
ite_method <- "bart"
include_ps <- "TRUE"
ps_method <- "SL.xgboost"
oreg_method <- NA
ntrees <- 100
min_nodes <- 20
max_nodes <- 5
random_state <- 121

# Check for binary outcome
binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

# Step 1: Split data
X <- as.matrix(X)
y <- as.matrix(y)
z <- as.matrix(z)

# Step 2: Estimate ITE
ite_list <- estimate_ite(y, z, X, ite_method, binary,
                         include_ps = include_ps,
                         ps_method = ps_method,
                         oreg_method = oreg_method,
                         random_state = random_state)
ite <- ite_list[["ite"]]
ite_std <- ite_list[["ite_std"]]

# Set parameters
N <- dim(X)[1]
sf <- min(1, (11 * sqrt(N) + 1) / N)
mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))

# Random Forest
set.seed(286)
forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                      sampsize = sf * N,
                                                      replace = FALSE,
                                                      ntree = 1, maxnodes = mn,
                                                      nodesize = min_nodes))
for(i in 2:ntrees) {
  mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
  set.seed(1281)
  model1_RF <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
                                                           sampsize = sf * N,
                                                           replace = FALSE,
                                                           ntree = 1, maxnodes = mn,
                                                           nodesize = min_nodes))
  forest <- randomForest::combine(forest, model1_RF)
}
treelist <- inTrees_RF2List(forest)
take_1 <- FALSE
type_decay <- 2

###### Run Tests ######

# Incorrect inputs
expect_error(extract_rules(treelist = NA, X, ntrees, ite_std, take_1, type_decay))
expect_error(extract_rules(treelist, X = NA, ntrees, ite_std, take_1, type_decay))
expect_error(extract_rules(treelist, X, ntrees = -100, ite_std, take_1, type_decay))
expect_error(extract_rules(treelist, X, ntrees, ite_std = NA, take_1, type_decay))
expect_error(extract_rules(treelist, X, ntrees, ite_std, take_1 = "test", type_decay))

# Correct outputs
rules_RF <- extract_rules(treelist, X, ntrees, ite_std, take_1, type_decay)
expect_true(class(rules_RF) == "character")
expect_equal(length(rules_RF), 6)
expect_equal(rules_RF[3], "X[,3]>0.5 & X[,10]<=0.5")

print(rules_RF[3])
