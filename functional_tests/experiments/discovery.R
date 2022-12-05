rm(list=ls())
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE)))
library(doParallel)
library(foreach)

################################################################################
# Set Up Parameters
method_params <- list(ratio_dis = 0.5,
                      ite_method_dis="bart",
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      include_ps_dis = TRUE,
                      ite_method_inf = "bart",
                      ps_method_inf = "SL.xgboost",
                      oreg_method_inf = "SL.xgboost",
                      include_ps_inf = TRUE,
                      include_offset = FALSE,
                      cate_method = "DRLearner",
                      cate_SL_library = "SL.xgboost",
                      filter_cate = FALSE,
                      offset_name = NA,
                      random_state = 3591)

hyper_params <- list(intervention_vars = c(),
                     ntrees_rf = 100,
                     ntrees_gbm = 50,
                     node_size = 20,
                     max_nodes = 5,
                     max_depth = 15,
                     max_decay = 0.025,
                     type_decay = 2,
                     t_ext = 0.025,
                     t_corr = 1,
                     replace = FALSE,
                     stability_selection = TRUE,
                     cutoff = 0.8,
                     pfer = 0.1)

nsim <- 3
eff_size <- seq(0, 3, 1)
rules_method <- NA

################################################################################
ipw.discovery = matrix(NA, nrow = length(eff_size), ncol = 3)
sipw.discovery = matrix(NA, nrow = length(eff_size), ncol = 3)
aipw.discovery = matrix(NA, nrow = length(eff_size), ncol = 3)
bcf.discovery = matrix(NA, nrow = length(eff_size), ncol = 3)
cf.discovery = matrix(NA, nrow = length(eff_size), ncol = 3)
ctree.discovery = matrix(NA, nrow = length(eff_size), ncol = 3)

################################################################################
multiResultClass <- function(ipw.list = NULL, sipw.list = NULL, aipw.list = NULL,
                             bcf.list = NULL, cf.list = NULL, ctree.list = NULL)
{
  me <- list(
    ipw.list = ipw.list,
    sipw.list = sipw.list,
    aipw.list = aipw.list,
    bcf.list = bcf.list,
    cf.list = cf.list,
    ctree.list = ctree.list
  )

  ## Set the name for the class
  class(me) <- append(class(me),"multiResultClass")
  return(me)
}
cl <- makeCluster(4)
registerDoParallel(cl)
################################################################################
# 2 Effect Modifiers

time.before = Sys.time()
matrix <- foreach(i = 1:nsim) %dopar% {
  for (j in 1:length(eff_size)) {
    tryCatch({
      # Set Seed
      set.seed(42+j+i)
      print(i)
      print(j)
      print(seed)

      # Libraries
      library(SuperLearner)
      library(causalTree)
      library(PSweight)
      library(stringr)

      # Data Generating Process
      #dataset_cont <- generate_cre_dataset(n = 2000, rho = 0, effect_modifiers = 2, p = 9,
                                           #effect_size = eff_size[j], binary = FALSE, no_tree = FALSE,
                                           #no_overlap = FALSE)
      dataset_cont <- generate_cre_dataset(n = 2000, rho = 0,  p = 10,
                                           effect_size = eff_size[j], binary = TRUE)

      y <- dataset_cont[["y"]]
      z <- dataset_cont[["z"]]
      X <- as.data.frame(dataset_cont[["X"]])
      tau <- dataset_cont[["tau"]]
      X_names <- names(as.data.frame(X))

      # DISCOVERY
      em <- c("x1", "x2")
      notEM <- names(X)[-which(names(X) %in% em)]

      ############################################################################
      # DRLearner CATE with IPW for ITE estimation
      print("Testing Discovery with IPW ITE estimation")

      cre_result <- cre(y, z, X, method_params, hyper_params)
      cre_result$CATE
      print(cre_result$CATE)

      # Discovered Rules
      DR_cre <- nrow(cre_result$CATE) - 1
      TD_cre <- which(grepl(em[1], cre_result$CATE[["Rule"]]) & grepl(em[2], cre_result$CATE[["Rule"]]))
      FD_cre <- sapply(1:length(notEM), function(x) grepl(notEM[x], cre_result$CATE[["Rule"]]))
      FD_cre <- FD_function(FD_cre)
      # FD_cre <- em_vector_length(FD_cre, cre_result$CATE$Rule, length = 20)
      FD_cre <- unique(c(FD_cre, which(nchar(as.character(cre_result$CATE$Rule))>20)))
      TD_cre <- fun_int(FD_cre, TD_cre)
      TP_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value <= 0.1))
      FN_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value > 0.1))
      FP_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value <= 0.1))
      TN_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value > 0.1))

      # True Positive Rate
      TPR_cre <- tpr(TP = TP_cre, FP = FP_cre)

      # F_score
      F_score_cre <- f_score(TP = TP_cre, FP = FP_cre, FN = FN_cre)

      # False Positive Rate
      FPR_cre <- fpr(FP = FP_cre, TN = TN_cre)

      # Store Results
      ipw.discovery[j,] <- c(TPR_cre, F_score_cre, FPR_cre)
      rm(TPR_cre, F_score_cre, FPR_cre)

      ############################################################################
      # DRLearner CATE  with SIPW for ITE estimation
      print("Testing Discovery with SIPW ITE estimation")

      cre_result <- cre(y, z, X, method_params, hyper_params)
      cre_result$CATE

      # Discovered Rules
      DR_cre <- nrow(cre_result$CATE) - 1
      TD_cre <- which(grepl(em[1], cre_result$CATE[["Rule"]]) & grepl(em[2], cre_result$CATE[["Rule"]]))
      FD_cre <- sapply(1:length(notEM), function(x) grepl(notEM[x], cre_result$CATE[["Rule"]]))
      FD_cre <- FD_function(FD_cre)
      #FD_cre <- em_vector_length(FD_cre, cre_result$CATE$Rule, length = 20)
      FD_cre <- unique(c(FD_cre, which(nchar(as.character(cre_result$CATE$Rule))>20)))
      TD_cre <- fun_int(FD_cre, TD_cre)
      TP_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value <= 0.1))
      FN_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value > 0.1))
      FP_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value <= 0.1))
      TN_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value > 0.1))

      # True Positive Rate
      TPR_cre <- tpr(TP = TP_cre, FP = FP_cre)

      # F_score
      F_score_cre <- f_score(TP = TP_cre, FP = FP_cre, FN = FN_cre)

      # False Positive Rate
      FPR_cre <- fpr(FP = FP_cre, TN = TN_cre)

      # Store Results
      sipw.discovery[j,] <- c(TPR_cre, F_score_cre, FPR_cre)
      rm(TPR_cre, F_score_cre, FPR_cre)

      ############################################################################
      # DRLearner CATE with AIPW for ITE estimation
      print("Testing Discovery with AIPW ITE estimation")

      cre_result <- cre(y, z, X, method_params, hyper_params)
      cre_result$CATE

      # Discovered Rules
      DR_cre <- nrow(cre_result$CATE) - 1
      TD_cre <- which(grepl(em[1], cre_result$CATE[["Rule"]]) & grepl(em[2], cre_result$CATE[["Rule"]]))
      FD_cre <- sapply(1:length(notEM), function(x) grepl(notEM[x], cre_result$CATE[["Rule"]]))
      FD_cre <- FD_function(FD_cre)
      # FD_cre <- em_vector_length(FD_cre, cre_result$CATE$Rule, length = 20)
      FD_cre <- unique(c(FD_cre, which(nchar(as.character(cre_result$CATE$Rule))>20)))
      TD_cre <- fun_int(FD_cre, TD_cre)
      TP_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value <= 0.1))
      FN_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value > 0.1))
      FP_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value <= 0.1))
      TN_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value > 0.1))

      # True Positive Rate
      TPR_cre <- tpr(TP = TP_cre, FP = FP_cre)

      # F_score
      F_score_cre <- f_score(TP = TP_cre, FP = FP_cre, FN = FN_cre)

      # False Positive Rate
      FPR_cre <- fpr(FP = FP_cre, TN = TN_cre)

      # Store Results
      aipw.discovery[j,] <- c(TPR_cre, F_score_cre, FPR_cre)
      rm(TPR_cre, F_score_cre, FPR_cre)

      ############################################################################
      # DRLearner CATE with BCF for ITE estimation
      print("Testing Discovery with BCF ITE estimation")

      cre_result <- cre(y, z, X, method_params, hyper_params)
      cre_result$CATE

      # Discovered Rules
      DR_cre <- nrow(cre_result$CATE) - 1
      TD_cre <- which(grepl(em[1], cre_result$CATE[["Rule"]]) & grepl(em[2], cre_result$CATE[["Rule"]]))
      FD_cre <- sapply(1:length(notEM), function(x) grepl(notEM[x], cre_result$CATE[["Rule"]]))
      FD_cre <- FD_function(FD_cre)
      # FD_cre <- em_vector_length(FD_cre, cre_result$CATE$Rule, length = 20)
      FD_cre <- unique(c(FD_cre, which(nchar(as.character(cre_result$CATE$Rule))>20)))
      TD_cre <- fun_int(FD_cre, TD_cre)
      TP_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value <= 0.1))
      FN_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value > 0.1))
      FP_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value <= 0.1))
      TN_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value > 0.1))

      # True Positive Rate
      TPR_cre <- tpr(TP = TP_cre, FP = FP_cre)

      # F_score
      F_score_cre <- f_score(TP = TP_cre, FP = FP_cre, FN = FN_cre)

      # False Positive Rate
      FPR_cre <- fpr(FP = FP_cre, TN = TN_cre)

      # Store Results
      bcf.discovery[j,] <- c(TPR_cre, F_score_cre, FPR_cre)
      rm(TPR_cre, F_score_cre, FPR_cre)

      ############################################################################
      # DRLearner CATE with CF for ITE estimation
      print("Testing Discovery with CF ITE estimation")

      cre_result <- cre(y, z, X, method_params, hyper_params)
      cre_result$CATE

      # Discovered Rules
      DR_cre <- nrow(cre_result$CATE) - 1
      TD_cre <- which(grepl(em[1], cre_result$CATE[["Rule"]]) & grepl(em[2], cre_result$CATE[["Rule"]]))
      FD_cre <- sapply(1:length(notEM), function(x) grepl(notEM[x], cre_result$CATE[["Rule"]]))
      FD_cre <- FD_function(FD_cre)
      # FD_cre <- em_vector_length(FD_cre, cre_result$CATE$Rule, length = 20)
      FD_cre <- unique(c(FD_cre, which(nchar(as.character(cre_result$CATE$Rule))>20)))
      TD_cre <- fun_int(FD_cre, TD_cre)
      TP_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value <= 0.1))
      FN_cre <- length(which(cre_result$CATE[TD_cre,]$P_Value > 0.1))
      FP_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value <= 0.1))
      TN_cre <- length(which(cre_result$CATE[FD_cre,]$P_Value > 0.1))

      # True Positive Rate
      TPR_cre <- tpr(TP = TP_cre, FP = FP_cre)

      # F_score
      F_score_cre <- f_score(TP = TP_cre, FP = FP_cre, FN = FN_cre)

      # False Positive Rate
      FPR_cre <- fpr(FP = FP_cre, TN = TN_cre)

      # Store Results
      cf.discovery[j,] <- c(TPR_cre, F_score_cre, FPR_cre)
      rm(TPR_cre, F_score_cre, FPR_cre)

      ############################################################################
      # Honest Causal Tree
      print("Testing Discovery with Honest Causal Tree")
      y_inf <- cre_result$outcome_vector_inf
      z_inf <- cre_result$treatment_vector_inf
      X_inf <- cre_result$covariates_matrix_inf
      index <- which(y %in% y_inf)
      y_dis <- y[-index]
      z_dis <- z[-index]
      X_dis <- X[-index,]
      data_dis <- as.data.frame(cbind(y_dis, X_dis))
      data_inf <- as.data.frame(cbind(y_inf, z_inf, X_inf))
      ps.formula <- as.formula(paste("z_inf ~ ", paste(X_names, collapse= "+")))
      out.formula <- as.formula(paste("y_inf ~ ", paste(X_names, collapse= "+")))

      # Honest Causal Tree Model
      fit.tree <- causalTree(y_dis ~ ., data = data_dis, treatment = z_dis,
                             split.Rule = "CT", cv.option = "CT",
                             split.Honest = T, cv.Honest = T, maxdepth = 3)
      opt.cp <- fit.tree$cptable[,1][which.min(fit.tree$cptable[,4])]

      # Pruned Tree
      pruned <- prune(fit.tree, opt.cp)

      # Generate Rules
      rules <- as.numeric(row.names(pruned$frame[pruned$numresp]))

      # Generate Leaves Indicator
      lvs <- leaves <- numeric(length(rules))
      lvs[unique(pruned$where)] <- 1
      leaves[rules[lvs==1]] <- 1

      # Initialize Outputs
      PvalueVec <- vector("logical", length(rules))
      rules.ctree <- vector("list",length(rules))

      # Extract Rules
      for (k in rules[-1]){
        # Create a Vector to Store all the Dimensions of a Rule
        sub <- as.data.frame(matrix(NA, nrow = 1,
                                    ncol = nrow(as.data.frame(path.rpart(pruned, node=k, print.it = FALSE)))-1))
        quiet(capture.output(for (h in 1:ncol(sub)){
          # Store each Rule as a Sub-population
          sub[,h] <- as.character(print(as.data.frame(path.rpart(pruned,node=k,print.it=FALSE))[h+1,1]))
          sub_pop <- noquote(paste(sub , collapse = " & "))
        }))
        subset <- with(data_inf, data_inf[which(eval(parse(text=sub_pop))),])
        # Treatment Effect Estimation via IPW
        if (length(unique(subset$z_inf))!= 1){
          ato <- PSweight(ps.formula = ps.formula, yname = 'y_inf', data = subset, weight = 'IPW', bootstrap = TRUE, R = 25)
          PvalueVec[k] <- summary(ato)[[1]][6]
          rules.ctree[[k]] <- sub_pop
        }
      }

      rule.sel.ctree <- unlist(rules.ctree)
      int.rule.sel.ctree <- interpretable_select_rules(rule.sel.ctree, X_names)
      rules_matrix_ctree <- generate_rules_matrix(X_inf, int.rule.sel.ctree, t)
      matrix_ctree <- rules_matrix_ctree$rules_matrix

      # Select the Matrices from Matrix Generation
      rule.sel.ctree <- rule.sel.ctree[which(int.rule.sel.ctree %in% rules_matrix_ctree$rules_list)]
      PvalueVec <- PvalueVec[which(int.rule.sel.ctree %in% rules_matrix_ctree$rules_list)]

      # Discovery
      DR_ctree <- length(rule.sel.ctree)
      TD_ctree <- which(grepl(em[1], rule.sel.ctree) & grepl(em[2], rule.sel.ctree) |
                          grepl(em[2], rule.sel.ctree) & grepl(em[3], rule.sel.ctree) )
      FD_ctree <- sapply(1:length(notEM), function(x) grepl(notEM[x], rule.sel.ctree ))
      FD_ctree <- FD_function(FD_ctree)
      # FD_ctree <- em_vector_length(FD_ctree, rule.sel.ctree, length = 20)
      FD_ctree <- unique(c(FD_ctree, which(nchar(as.character(rule.sel.ctree))>20)))
      TD_ctree <- fun_int(FD_ctree, TD_ctree)
      TP_ctree <- length(which(PvalueVec[TD_ctree] <= 0.1))
      FN_ctree <- length(which(PvalueVec[TD_ctree] > 0.1))
      FP_ctree <- length(which(PvalueVec[FD_ctree] <= 0.1))
      TN_ctree <- length(which(PvalueVec[FD_ctree] > 0.1))

      # True Positive Rate
      TPR_ctree <- tpr(TP = TP_ctree, FP = FP_ctree)

      # F_score
      F_score_ctree <- f_score(TP = TP_ctree, FP = FP_ctree, FN = FN_ctree)

      # False Positive Rate
      FPR_ctree <- fpr(FP = FP_ctree, TN = TN_ctree)

      # Store Results
      ctree.discovery[j,] <- c(TPR_ctree, F_score_ctree, FPR_ctree)
      rm(TPR_ctree, F_score_ctree, FPR_ctree)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  # Store Results
  result <- multiResultClass()
  result$ipw.list <- ipw.discovery
  result$sipw.list <- sipw.discovery
  result$aipw.list <- aipw.discovery
  result$bcf.list <- bcf.discovery
  result$cf.list <- cf.discovery
  result$ctree.list <- ctree.discovery
  return(result)
}
time.after = Sys.time()
time.after - time.before
stopCluster(cl)

# Extract and Store IPW
ipw.list <- c()
for (i in 1:nsim) {
  ipw.list[[i]] <- matrix[[i]]$ipw.list
}
ipw_mat_2_eff_1000 <- Reduce(`+`, ipw.list)/length(ipw.list)
ipw_mat_2_eff_1000 <- cbind(ipw_mat_2_eff_1000)
colnames(ipw_mat_2_eff_1000) <- c("TPR", "F1-score", "FPR")
ipw_mat_2_eff_1000 <- as.data.frame(ipw_mat_2_eff_1000)
ipw_long_2_eff_1000 <- gather(ipw_mat_2_eff_1000, measure, result, factor_key=TRUE)
ipw_long_2_eff_1000 <- cbind(ipw_long_2_eff_1000, rep(1000, nrow(ipw_long_2_eff_1000)))
colnames(ipw_long_2_eff_1000) <- c("measure", "result", "size")
method <- rep("CRE-IPW", nrow(ipw_long_2_eff_1000))
effect_size <- rep(eff_size, nrow(ipw_long_2_eff_1000)/length(eff_size))
ipw_long_2_eff_1000 <- cbind(ipw_long_2_eff_1000, method, effect_size)

# Extract and Store SIPW
sipw.list <- c()
for (i in 1:nsim) {
  sipw.list[[i]] <- matrix[[i]]$sipw.list
}
sipw_mat_2_eff_1000 <- Reduce(`+`, sipw.list)/length(sipw.list)
sipw_mat_2_eff_1000 <- cbind(sipw_mat_2_eff_1000)
colnames(sipw_mat_2_eff_1000) <- c("TPR", "F1-score", "FPR")
sipw_mat_2_eff_1000 <- as.data.frame(sipw_mat_2_eff_1000)
sipw_long_2_eff_1000 <- gather(sipw_mat_2_eff_1000, measure, result, factor_key=TRUE)
sipw_long_2_eff_1000 <- cbind(sipw_long_2_eff_1000, rep(1000, nrow(sipw_long_2_eff_1000)))
colnames(sipw_long_2_eff_1000) <- c("measure", "result", "size")
method <- rep("CRE-SIPW", nrow(sipw_long_2_eff_1000))
effect_size <- rep(eff_size, nrow(sipw_long_2_eff_1000)/length(eff_size))
sipw_long_2_eff_1000 <- cbind(sipw_long_2_eff_1000, method, effect_size)

# Extract and Store AIPW
aipw.list <- c()
for (i in 1:nsim) {
  aipw.list[[i]] <- matrix[[i]]$aipw.list
}
aipw_mat_2_eff_1000 <- Reduce(`+`, aipw.list)/length(aipw.list)
aipw_mat_2_eff_1000 <- cbind(aipw_mat_2_eff_1000)
colnames(aipw_mat_2_eff_1000) <- c("TPR", "F1-score", "FPR")
aipw_mat_2_eff_1000 <- as.data.frame(aipw_mat_2_eff_1000)
aipw_long_2_eff_1000 <- gather(aipw_mat_2_eff_1000, measure, result, factor_key=TRUE)
aipw_long_2_eff_1000 <- cbind(aipw_long_2_eff_1000, rep(1000, nrow(aipw_long_2_eff_1000)))
colnames(aipw_long_2_eff_1000) <- c("measure", "result", "size")
method <- rep("CRE-AIPW", nrow(aipw_long_2_eff_1000))
effect_size <- rep(eff_size, nrow(aipw_long_2_eff_1000)/length(eff_size))
aipw_long_2_eff_1000 <- cbind(aipw_long_2_eff_1000, method, effect_size)

# Extract and Store BCF
bcf.list <- c()
for (i in 1:nsim) {
  bcf.list[[i]] <- matrix[[i]]$bcf.list
}
bcf_mat_2_eff_1000 <- Reduce(`+`, bcf.list)/length(bcf.list)
bcf_mat_2_eff_1000 <- cbind(bcf_mat_2_eff_1000)
colnames(bcf_mat_2_eff_1000) <- c("TPR", "F1-score", "FPR")
bcf_mat_2_eff_1000 <- as.data.frame(bcf_mat_2_eff_1000)
bcf_long_2_eff_1000 <- gather(bcf_mat_2_eff_1000, measure, result, factor_key=TRUE)
bcf_long_2_eff_1000 <- cbind(bcf_long_2_eff_1000, rep(1000, nrow(bcf_long_2_eff_1000)))
colnames(bcf_long_2_eff_1000) <- c("measure", "result", "size")
method <- rep("CRE-BCF", nrow(bcf_long_2_eff_1000))
effect_size <- rep(eff_size, nrow(bcf_long_2_eff_1000)/length(eff_size))
bcf_long_2_eff_1000 <- cbind(bcf_long_2_eff_1000, method, effect_size)

# Extract and Store AIPW
cf.list <- c()
for (i in 1:nsim) {
  cf.list[[i]] <- matrix[[i]]$cf.list
}
cf_mat_2_eff_1000 <- Reduce(`+`, cf.list)/length(cf.list)
cf_mat_2_eff_1000 <- cbind(cf_mat_2_eff_1000)
colnames(cf_mat_2_eff_1000) <- c("TPR", "F1-score", "FPR")
cf_mat_2_eff_1000 <- as.data.frame(cf_mat_2_eff_1000)
cf_long_2_eff_1000 <- gather(cf_mat_2_eff_1000, measure, result, factor_key=TRUE)
cf_long_2_eff_1000 <- cbind(cf_long_2_eff_1000, rep(1000, nrow(cf_long_2_eff_1000)))
colnames(cf_long_2_eff_1000) <- c("measure", "result", "size")
method <- rep("CRE-CF", nrow(cf_long_2_eff_1000))
effect_size <- rep(eff_size, nrow(cf_long_2_eff_1000)/length(eff_size))
cf_long_2_eff_1000 <- cbind(cf_long_2_eff_1000, method, effect_size)

# Extract and Store CTREE
ctree.list <- c()
for (i in 1:nsim) {
  ctree.list[[i]] <- matrix[[i]]$ctree.list
}
ctree_mat_2_eff_1000 <- Reduce(`+`, ctree.list)/length(ctree.list)
ctree_mat_2_eff_1000 <- cbind(ctree_mat_2_eff_1000)
colnames(ctree_mat_2_eff_1000) <- c("TPR", "F1-score", "FPR")
ctree_mat_2_eff_1000 <- as.data.frame(ctree_mat_2_eff_1000)
ctree_long_2_eff_1000 <- gather(ctree_mat_2_eff_1000, measure, result, factor_key=TRUE)
ctree_long_2_eff_1000 <- cbind(ctree_long_2_eff_1000, rep(1000, nrow(ctree_long_2_eff_1000)))
colnames(ctree_long_2_eff_1000) <- c("measure", "result", "size")
method <- rep("HCT", nrow(ctree_long_2_eff_1000))
effect_size <- rep(eff_size, nrow(ctree_long_2_eff_1000)/length(eff_size))
ctree_long_2_eff_1000 <- cbind(ctree_long_2_eff_1000, method, effect_size)

discovery_long_1000 <- rbind(ipw_long_2_eff_1000, sipw_long_2_eff_1000, aipw_long_2_eff_1000,
                             bcf_long_2_eff_1000, cf_long_2_eff_1000, ctree_long_2_eff_1000)
discovery_long_2em <- discovery_long_1000
write.csv(discovery_long_2em, file = paste0("output_sim/discovery_long_2em", process, ".csv"))


### PLOTs

#rm(list=ls())
#library(readr)
#discovery_long_1000 <- read_csv("~/OneDrive - Harvard University/Research/CRE/Output/Server/discovery_long_2em_1000.csv")
#discovery_long_2000 <- read_csv("~/OneDrive - Harvard University/Research/CRE/Output/Server/discovery_long_2em_2000.csv")
#discovery_long_2em <- rbind(discovery_long_1000, discovery_long_2000)


methods <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "HCT")
discovery_long_2em$method = factor(discovery_long_2em$method, levels = methods)
measures <- c('TPR','F1-score','FPR')
discovery_long_2em$measure_n = factor(discovery_long_2em$measure, levels = measures)


gbase  = ggplot(discovery_long_2em, aes(y = result, colour = method)) +
  facet_grid(measure_n ~ size) +
  geom_line(size = 1.25)
gline = gbase + geom_line(data = discovery_long_2em,
                          aes(x = effect_size, y = result, colour = method))
print(gline + aes(x = effect_size))

pdf("mt_discovery_1000_2000_v1.pdf")
print(gline + aes(x = effect_size))
dev.off()

#print(gbase + aes(x = effect_size) + geom_smooth(method = "loess", se = FALSE))
#print(gbase + aes(x = effect_size) + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE))
