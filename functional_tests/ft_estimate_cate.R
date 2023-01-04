set.seed(2021)
dataset_cont <- generate_cre_dataset(n = 2000, rho = 0, n_rules = 2, p = 10,
                                     effect_size = 2, binary_outcome = FALSE)
y <- dataset_cont[["y"]]
z <- dataset_cont[["z"]]
X <- as.data.frame(dataset_cont[["X"]])
X_names <- names(as.data.frame(X))
ratio_dis <- 0.25
ite_method_dis <- "bart"
include_ps_dis <- "TRUE"
ps_method_dis <- "SL.xgboost"
oreg_method_dis <- NA
ite_method_inf <- "bart"
include_ps_inf <- "FALSE"
ps_method_inf <- "SL.xgboost"
oreg_method_inf <- "SL.xgboost"
ntrees_rf <- 100
ntrees_gbm <- 50
node_size <- 20
max_nodes <- 5
max_depth <- 15
replace <- FALSE
max_decay <- 0.025
type_decay <- 2
t <- 0.025
cutoff <- 0.8
pfer <- 0.1
stability_selection <- TRUE
offset <- NULL
cate_method <- "DRLearner"
cate_SL_library <- "SL.xgboost"
t_pvalue <- 0.5
intervention_vars <- c()
penalty_rl <- 1

# Check for binary outcome
binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

# Step 1: Split data
X <- as.matrix(X)
y <- as.matrix(y)
z <- as.matrix(z)
subgroups <- honest_splitting(y, z, X, ratio_dis)
discovery <- subgroups[[1]]
inference <- subgroups[[2]]

# Generate y, z, and X for discovery and inference data
y_dis <- discovery$y
z_dis <- discovery$z
X_dis <- discovery$X

y_inf <- inference$y
z_inf <- inference$z
X_inf <- inference$X

# Step 2: Estimate ITE
ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis,
                             binary_outcome,
                             include_ps = include_ps_dis,
                             ps_method = ps_method_dis,
                             oreg_method = oreg_method_dis,
                             X_names = X_names,
                             offset = offset)
ite_dis <- ite_list_dis[["ite"]]


# Step 3: Generate rules list
initial_rules_dis <- generate_rules(X_dis, ite_dis, intervention_vars,
                                    ntrees_rf, ntrees_gbm, node_size,
                                    max_nodes, max_depth, replace)

rules_list_dis <- filter_irrelevant_rules(initial_rules_dis, X_dis,
                                          ite_dis, max_decay, type_decay)

# Step 4: Generate rules matrix
rules_matrix_dis <- generate_rules_matrix(X_dis, rules_list_dis)
rules_matrix_std_dis <- standardize_rules_matrix(rules_matrix_dis)

# Step 5: Select important rules
select_rules_dis <- as.character(discover_causal_rules(rules_matrix_std_dis,
                                                       rules_list_dis,
                                                       ite_dis,
                                                       stability_selection,
                                                       cutoff,
                                                       pfer,
                                                       penalty_rl))
select_rules_matrix_dis <- rules_matrix_dis[,
                                            which(rules_list_dis %in%
                                                    select_rules_dis)]
select_rules_matrix_std_dis <- rules_matrix_std_dis[,
                                                    which(rules_list_dis %in%
                                                            select_rules_dis)]

# Step 6: Estimate CATE
ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf,
                             binary_outcome,
                             include_ps = include_ps_inf,
                             ps_method = ps_method_inf,
                             oreg_method = oreg_method_inf,
                             X_names = X_names,
                             offset = offset)
ite_inf <- ite_list_inf[["ite"]]
ite_std_inf <- ite_list_inf[["ite_std"]]
sd_ite_inf <- ite_list_inf[["sd_ite"]]

if (length(select_rules_dis)==0){
  rules_matrix_inf <- NA
  select_rules_interpretable <- NA
} else {
  rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
  select_rules_interpretable <- interpret_select_rules(select_rules_dis,
                                                       X_names)
}

###### Run Tests ######

# TO DO: add test to check wrong arguments

# Correct outputs
cate_inf <- estimate_cate(y_inf,
                          z_inf,
                          X_inf,
                          X_names,
                          offset,
                          rules_matrix_inf,
                          select_rules_interpretable,
                          cate_method,
                          ite_inf,
                          sd_ite_inf,
                          cate_SL_library,
                          t_pvalue)

expect_true(class(cate_inf$summary) == "data.frame")

cate_method <- NULL
expect_error(estimate_cate(y_inf, z_inf, X_inf, X_names, offset,
                           rules_matrix_inf, select_rules_interpretable,
                           cate_method, ite_inf, sd_ite_inf, cate_SL_library,
                           t_pvalue))
