
# set.seed(128)
# dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#                                effect_size = 2, binary = FALSE)
# # Initialize parameters
# y <- dataset[["y"]]
# z <- dataset[["z"]]
# X <- as.data.frame(dataset[["X"]])
# X_names <- names(as.data.frame(X))
# ratio_dis <- 0.25
# ite_method_dis <- "bart"
# include_ps_dis <- TRUE
# ps_method_dis <- "SL.xgboost"
# oreg_method_dis <- NA
# ite_method_inf <- "bart"
# include_ps_inf <- TRUE
# ps_method_inf <- "SL.xgboost"
# oreg_method_inf <- NA
# ntrees_rf <- 100
# ntrees_gbm <- 50
# min_nodes <- 20
# max_nodes <- 5
# t <- 0.025
# q <- 0.8
# rules_method <- NA
# include_offset <- FALSE
# offset_name <- NA
# binary <- FALSE
# cate_method <- "DRLearner"
# cate_SL_library <- "SL.xgboost"
# filter_cate <- FALSE
# # Split data
# X <- as.matrix(X)
# y <- as.matrix(y)
# z <- as.matrix(z)
# subgroups <- CRE:::split_data(y, z, X, ratio_dis)
# discovery <- subgroups[[1]]
# inference <- subgroups[[2]]
# # Generate y, z, and X for discovery and inference data
# y_dis <- discovery[,1]
# z_dis <- discovery[,2]
# X_dis <- discovery[,3:ncol(discovery)]
# y_inf <- inference[,1]
# z_inf <- inference[,2]
# X_inf <- inference[,3:ncol(inference)]
# # Estimate ITE on Discovery Subsample
# ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis,
#                             ite_method =ite_method_dis,
#                             include_ps = include_ps_dis,
#                             ps_method = ps_method_dis,
#                             oreg_method = oreg_method_dis,
#                             is_y_binary = binary,
#                             X_names = X_names,
#                             include_offset = include_offset,
#                             offset_name = offset_name,
#                             random_state = 234)
# ite_dis <- ite_list_dis[["ite"]]
# ite_std_dis <- ite_list_dis[["ite_std"]]
# # Generate rules list
# initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
#                                    min_nodes, max_nodes, random_state = 214)
# # Generate rules matrix
# rules_all_dis <- CRE:::generate_rules_matrix(X_dis, initial_rules_dis, t)
# rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
# rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
# rules_list_dis <- rules_all_dis[["rules_list"]]
# # Select important rules
# select_rules_dis <- as.character(CRE:::select_causal_rules(rules_matrix_std_dis, rules_list_dis,
#                                                     ite_std_dis, binary, q, rules_method))
# select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
# select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]
# if (length(select_rules_dis) == 0) stop("No significant rules were discovered. Ending Analysis.")
# # Estimate Inference ITE and CATE
# rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1], ncol = length(select_rules_dis))
# for (i in 1:length(select_rules_dis)) {
#  rules_matrix_inf[eval(parse(text = select_rules_dis[i]), list(X = X_inf)), i] <- 1
# }
# select_rules_interpretable <- CRE:::interpret_select_rules(select_rules_dis, X_names)
# ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf,
#                             ite_method = ite_method_inf,
#                             include_ps = include_ps_inf,
#                             ps_method = ps_method_inf,
#                             oreg_method = oreg_method_inf,
#                             is_y_binary = binary,
#                             X_names = X_names,
#                             include_offset = include_offset,
#                             offset_name = offset_name,
#                             random_state = 324)
# ite_inf <- ite_list_inf[["ite"]]
# ite_std_inf <- ite_list_inf[["ite_std"]]
# cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
#                         rules_matrix_inf, select_rules_interpretable,
#                         cate_method, ite_inf, sd_ite_inf,
#                         cate_SL_library, filter_cate)

#===============================================================================

set.seed(99687)
dataset <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
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
cate_method <- "bart-baggr"
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
                             offset_name = offset_name,
                             random_state = 234)
ite_dis <- ite_list_dis[["ite"]]
ite_std_dis <- ite_list_dis[["ite_std"]]

# Generate rules list
initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                          min_nodes, max_nodes, random_state = 214)
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
                             offset_name = offset_name,
                             random_state = 324)
ite_inf <- ite_list_inf[["ite"]]
ite_std_inf <- ite_list_inf[["ite_std"]]
sd_ite_inf <- ite_list_inf[["sd_ite"]]
cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
                          rules_matrix_inf, select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, filter_cate)



#===============================================================================
# Raises error with negative values.
set.seed(929687)
dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
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
cate_method <- "poisson"
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
                             offset_name = offset_name,
                             random_state = 234)
ite_dis <- ite_list_dis[["ite"]]
ite_std_dis <- ite_list_dis[["ite_std"]]

# Generate rules list
initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                          min_nodes, max_nodes, random_state = 214)
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
                             offset_name = offset_name,
                             random_state = 324)
ite_inf <- ite_list_inf[["ite"]]
ite_std_inf <- ite_list_inf[["ite_std"]]
sd_ite_inf <- ite_list_inf[["sd_ite"]]
cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
                          rules_matrix_inf, select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, filter_cate)


#===============================================================================

set.seed(95687)
dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
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
cate_method <- "cf-means"
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
                             offset_name = offset_name,
                             random_state = 234)
ite_dis <- ite_list_dis[["ite"]]
ite_std_dis <- ite_list_dis[["ite_std"]]

# Generate rules list
initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                          min_nodes, max_nodes, random_state = 214)
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
                             offset_name = offset_name,
                             random_state = 324)
ite_inf <- ite_list_inf[["ite"]]
ite_std_inf <- ite_list_inf[["ite_std"]]
sd_ite_inf <- ite_list_inf[["sd_ite"]]
cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
                          rules_matrix_inf, select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, filter_cate)


# ==============================================================================

# Raises error with dplyer::filter()
set.seed(687)
dataset <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
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
cate_method <- "linreg"
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
                             offset_name = offset_name,
                             random_state = 234)
ite_dis <- ite_list_dis[["ite"]]
ite_std_dis <- ite_list_dis[["ite_std"]]

# Generate rules list
initial_rules_dis <- CRE:::generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                          min_nodes, max_nodes, random_state = 214)
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
                             offset_name = offset_name,
                             random_state = 324)
ite_inf <- ite_list_inf[["ite"]]
ite_std_inf <- ite_list_inf[["ite_std"]]
sd_ite_inf <- ite_list_inf[["sd_ite"]]
cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
                          rules_matrix_inf, select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, filter_cate)

