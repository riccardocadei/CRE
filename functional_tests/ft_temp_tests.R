set.seed(3784)
dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                     effect_size = 0.5, binary = FALSE)
y <- dataset_cont[["y"]]
z <- dataset_cont[["z"]]
X <- dataset_cont[["X"]]
ite_method <- "bart"
include_ps <- "TRUE"
ps_method <- "SL.xgboost"
or_method <- NA
ntrees_rf <- 100
ntrees_gbm <- 50
min_nodes <- 20
max_nodes <- 5

# Check for binary outcome
binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

# Step 1: Split data
X <- as.matrix(X)
y <- as.matrix(y)
z <- as.matrix(z)

# Step 2: Estimate ITE
set.seed(37841)
ite_list <- estimate_ite(y, z, X, ite_method, binary,
                         include_ps = include_ps,
                         ps_method = ps_method,
                         or_method = or_method)


ite <- ite_list[["ite"]]
ite_std <- ite_list[["ite_std"]]

print(ite)
print(ite_std)

###### Run Tests ######

# Incorrect inputs
# expect_error(generate_rules(X = "test", ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes))
# expect_error(generate_rules(X, ite_std = "test", ntrees_rf, ntrees_gbm, min_nodes, max_nodes))
# expect_error(generate_rules(X, ite_std, ntrees_rf = "test", ntrees_gbm, min_nodes, max_nodes))
# expect_error(generate_rules(X, ite_std, ntrees_rf, ntrees_gbm = "test", min_nodes, max_nodes))
# expect_error(generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes = "test", max_nodes))
# expect_error(generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes = "test"))

# Correct outputs
initial_rules <- generate_rules(X,
                                ite_std,
                                ntrees_rf,
                                ntrees_gbm,
                                min_nodes,
                                max_nodes,
                                random_state = 215)
expect_true(class(initial_rules) == "character")
expect_equal(length(initial_rules), 127)
expect_equal(initial_rules[23], "X[,8]>0.5")


# ------------------------------------------------------------------------------

## Estimate bart

set.seed(167)
dataset <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
include_ps <- TRUE
ps_method <- "SL.xgboost"

ite_list <- estimate_ite_bart(y, z, X, include_ps, ps_method, random_state = 345)

print(ite_list)

# ------------------------------------------------------------------------------

library(bartCause)
n <- 100L
beta.z <- c(.75, -0.5,  0.25)
beta.y <- c(.5,   1.0, -1.5)
sigma <- 2

set.seed(725)
x <- matrix(rnorm(3 * n), n, 3)
tau <- rgamma(1L, 0.25 * 16 * rgamma(1L, 1 * 32, 32), 16)

p.score <- pnorm(x %*% beta.z)
z <- rbinom(n, 1, p.score)

mu.0 <- x %*% beta.y
mu.1 <- x %*% beta.y + tau

y <- mu.0 * (1 - z) + mu.1 * z + rnorm(n, 0, sigma)

# low parameters only for example
fit <- bartc(y, z, x, n.samples = 100L, n.burn = 15L, n.chains = 2L, seed = 398)
summary(fit)

## example to show refitting under the common support rule
fit2 <- refit(fit, commonSup.rule = "sd")
fit3 <- bartc(y, z, x, subset = fit2$commonSup.sub,
              n.samples = 100L, n.burn = 15L, n.chains = 2L)

# ------------------------------------------------------------------------------


dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
X_names <- names(as.data.frame(X))
ratio_dis <- 0.25
ite_method_dis <- "bart"
include_ps_dis <- TRUE
ps_method_dis <- "SL.xgboost"
oreg_method_dis <- NA
ite_method_inf <- "bart"
include_ps_inf <- TRUE
ps_method_inf <- "SL.xgboost"
oreg_method_inf <- NA
ntrees_rf <- 100
ntrees_gbm <- 50
min_nodes <- 20
max_nodes <- 5
t <- 0.025
q <- 0.8
rules_method <- NA
include_offset <- FALSE
offset_name <- NA
binary <- FALSE
cate_method <- "DRLearner"
cate_SL_library <- "SL.xgboost"
filter_cate <- FALSE

# Split data
X <- as.matrix(X)
y <- as.matrix(y)
z <- as.matrix(z)
subgroups <- CRE:::split_data(y, z, X, ratio_dis)
discovery <- subgroups[[1]]
inference <- subgroups[[2]]

# Generate y, z, and X for discovery and inference data
y_dis <- discovery[,1]
z_dis <- discovery[,2]
X_dis <- discovery[,3:ncol(discovery)]

y_inf <- inference[,1]
z_inf <- inference[,2]
X_inf <- inference[,3:ncol(inference)]

# Estimate ITE on Discovery Subsample
ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
                             ite_method =ite_method_dis,
                             include_ps = include_ps_dis,
                             ps_method = ps_method_dis,
                             oreg_method = oreg_method_dis,
                             is_y_binary = binary,
                             X_names = X_names,
                             include_offset = include_offset,
                             offset_name = offset_name)
ite_dis <- ite_list_dis[["ite"]]
ite_std_dis <- ite_list_dis[["ite_std"]]

# Generate rules list
initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                          min_nodes, max_nodes)

# Generate rules matrix
rules_all_dis <- CRE:::generate_rules_matrix(X_dis, initial_rules_dis, t)
rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
rules_list_dis <- rules_all_dis[["rules_list"]]

# Select important rules
select_rules_dis <- as.character(CRE:::select_causal_rules(rules_matrix_std_dis, rules_list_dis,
                                                           ite_std_dis, binary, q, rules_method))
select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]
if (length(select_rules_dis) == 0) stop("No significant rules were discovered. Ending Analysis.")

# Estimate Inference ITE and CATE
rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1], ncol = length(select_rules_dis))
for (i in 1:length(select_rules_dis)) {
  rules_matrix_inf[eval(parse(text = select_rules_dis[i]), list(X = X_inf)), i] <- 1
}
select_rules_interpretable <- CRE:::interpret_select_rules(select_rules_dis, X_names)

ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf,
                             ite_method = ite_method_inf,
                             include_ps = include_ps_inf,
                             ps_method = ps_method_inf,
                             oreg_method = oreg_method_inf,
                             is_y_binary = binary,
                             X_names = X_names,
                             include_offset = include_offset,
                             offset_name = offset_name)
ite_inf <- ite_list_inf[["ite"]]
ite_std_inf <- ite_list_inf[["ite_std"]]

cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
                          rules_matrix_inf, select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, filter_cate)

# ------------------------------------------------------------------------------

dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

cre_results <- cre(y = dataset[["y"]],
                   z = dataset[["z"]],
                   X = as.data.frame(dataset[["X"]]),
                   method_params = list(ratio_dis = 0.25,
                                        ite_method_dis = "oreg",
                                        include_ps_dis = TRUE,
                                        ps_method_dis = "SL.xgboost",
                                        ps_method_inf = "SL.xgboost",
                                        ite_method_inf = "oreg",
                                        include_ps_inf = TRUE,
                                        include_offset = FALSE,
                                        cate_method = "DRLearner",
                                        cate_SL_library = "SL.xgboost",
                                        filter_cate = FALSE,
                                        offset_name = NA),
                   hyper_params = list(ntrees_rf = 100,
                                       ntrees_gbm = 50,
                                       min_nodes = 20,
                                       max_nodes = 5,
                                       t = 0.025,
                                       q = 0.8))


# -----------------------------

set.seed(192)
dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

cre_results <- cre(y = dataset[["y"]],
                   z = dataset[["z"]],
                   X = as.data.frame(dataset[["X"]]),
                   method_params = list(ratio_dis = 0.25,
                                        ite_method_dis="bart",
                                        include_ps_dis = TRUE,
                                        ps_method_dis = "SL.xgboost",
                                        ps_method_inf = "SL.xgboost",
                                        ite_method_inf = "bart",
                                        include_ps_inf = TRUE,
                                        include_offset = FALSE,
                                        cate_method = "DRLearner",
                                        cate_SL_library = "SL.xgboost",
                                        filter_cate = FALSE,
                                        offset_name = NA),
                   hyper_params = list(ntrees_rf = 100,
                                       ntrees_gbm = 50,
                                       min_nodes = 20,
                                       max_nodes = 5,
                                       t = 0.025,
                                       q = 0.8))

# -----------------------------------------------

dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

cre_cross_results <- cre_crossfit(y = dataset[["y"]], z = dataset[["z"]],
                                  X = as.data.frame(dataset[["X"]]),
                                  ite_method_dis = "bart",
                                  include_ps_dis = TRUE,
                                  ite_method_inf = "bart",
                                  include_ps_inf = TRUE,
                                  ntrees_rf = 100, ntrees_gbm = 50,
                                  min_nodes = 20, max_nodes = 5,
                                  t = 0.025, q = 0.8)

# ------------------------------------------------------------------------------

dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

cre_results <- cre(y = dataset[["y"]],
                   z = dataset[["z"]],
                   X = as.data.frame(dataset[["X"]]),
                   method_params = list(ratio_dis = 0.25,
                                        ite_method_dis = "oreg",
                                        include_ps_dis = TRUE,
                                        ps_method_dis = "SL.xgboost",
                                        ps_method_inf = "SL.xgboost",
                                        ite_method_inf = "oreg",
                                        include_ps_inf = TRUE,
                                        include_offset = FALSE,
                                        cate_method = "DRLearner",
                                        cate_SL_library = "SL.xgboost",
                                        filter_cate = FALSE,
                                        offset_name = NA),
                   hyper_params = list(ntrees_rf = 100,
                                       ntrees_gbm = 50,
                                       min_nodes = 20,
                                       max_nodes = 5,
                                       t = 0.025,
                                       q = 0.8))


# -----------------------------

set.seed(192)
dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

cre_results <- cre(y = dataset[["y"]],
                   z = dataset[["z"]],
                   X = as.data.frame(dataset[["X"]]),
                   method_params = list(ratio_dis = 0.25,
                                        ite_method_dis="bart",
                                        include_ps_dis = TRUE,
                                        ps_method_dis = "SL.xgboost",
                                        ps_method_inf = "SL.xgboost",
                                        ite_method_inf = "bart",
                                        include_ps_inf = TRUE,
                                        include_offset = FALSE,
                                        cate_method = "DRLearner",
                                        cate_SL_library = "SL.xgboost",
                                        filter_cate = FALSE,
                                        offset_name = NA),
                   hyper_params = list(ntrees_rf = 100,
                                       ntrees_gbm = 50,
                                       min_nodes = 20,
                                       max_nodes = 5,
                                       t = 0.025,
                                       q = 0.8))

# -=---------------------------------------------

dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)

cre_cross_results <- cre_crossfit(y = dataset[["y"]], z = dataset[["z"]],
                                  X = as.data.frame(dataset[["X"]]),
                                  ite_method_dis = "bart",
                                  include_ps_dis = TRUE,
                                  ite_method_inf = "bart",
                                  include_ps_inf = TRUE,
                                  ntrees_rf = 100, ntrees_gbm = 50,
                                  min_nodes = 20, max_nodes = 5,
                                  t = 0.025, q = 0.8)

# ------------------------------------------------------------------------------

dataset <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                                effect_size = 2, binary = FALSE)
# Initialize parameters
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- as.data.frame(dataset[["X"]])
X_names <- names(as.data.frame(X))
ratio_dis <- 0.25
ite_method_dis <- "bart"
include_ps_dis <- TRUE
ps_method_dis <- "SL.xgboost"
oreg_method_dis <- NA
ntrees_rf <- 100
ntrees_gbm <- 50
min_nodes <- 20
max_nodes <- 5
t <- 0.025
q <- 0.8
rules_method <- NA
include_offset <- FALSE
offset_name <- NA
binary <- FALSE
# Split data
X <- as.matrix(X)
y <- as.matrix(y)
z <- as.matrix(z)
subgroups <- CRE:::split_data(y, z, X, ratio_dis)
discovery <- subgroups[[1]]
inference <- subgroups[[2]]
# Generate y, z, and X for discovery and inference data
y_dis <- discovery[,1]
z_dis <- discovery[,2]
X_dis <- discovery[,3:ncol(discovery)]
# Estimate ITE on Discovery Subsample
ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
                             ite_method = ite_method_dis,
                             include_ps = include_ps_dis,
                             ps_method = ps_method_dis,
                             oreg_method = oreg_method_dis,
                             is_y_binary = binary,
                             X_names = X_names,
                             include_offset = include_offset,
                             offset_name = offset_name)
ite_dis <- ite_list_dis[["ite"]]
ite_std_dis <- ite_list_dis[["ite_std"]]
# Generate rules list
initial_rules_dis <- CRE:::generate_rules(X_dis,
                                          ite_std_dis,
                                          ntrees_rf,
                                          ntrees_gbm,
                                          min_nodes,
                                          max_nodes)
# Generate rules matrix
rules_all_dis <- CRE:::generate_rules_matrix(X_dis, initial_rules_dis, t)
rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
rules_list_dis <- rules_all_dis[["rules_list"]]
# Select important rules
select_rules_dis <- as.character(CRE:::select_causal_rules(
  rules_matrix_std_dis,
  rules_list_dis,
  ite_std_dis,
  binary,
  q,
  rules_method))

#-------------------------------------------------------------------------------
