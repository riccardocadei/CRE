# Simulations for CRE estimation
rm(list=ls())
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE)))
setwd("/Users/falco/OneDrive - Harvard University/Research/CRE/Code/Functions")
source("CRE_functions.R")
library(tidyr)
library(doParallel)
library(foreach)

################################################################################
# Set Up Parameters
nsim <- 200
n <- c(1000, 2000, 3000, 4000)
eff_mod <- 2
ratio_dis <- 0.50
ntrees_rf <- 100
ntrees_gbm <- 50
min_nodes <- 20
max_nodes <- 5
t <- 0.025
q <- 0.8
rules_method <- NA
include_offset <- FALSE
offset_name <- NA
filter_cate <- FALSE

mse.mat = matrix(NA, nrow = length(n), ncol = 7)
bias.mat = matrix(NA, nrow = length(n), ncol = 7)

################################################################################
multiResultClass <- function(bias.list = NULL, mse.list = NULL)
{
  me <- list(
    bias.list = bias.list,
    mse.list = mse.list
  )

  ## Set the name for the class
  class(me) <- append(class(me),"multiResultClass")
  return(me)
}
cl <- makeCluster(4)
registerDoParallel(cl)


################################################################################
# Two Effect Modifiers

time.before = Sys.time()
matrix <- foreach(i = 1:nsim) %dopar% {
  for (j in 1:length(n)) {
    tryCatch({
      # Set Seed
      set.seed(42+j+i)

      #Packages
      library(SuperLearner)

      # Data Generating Process
      dataset_cont <- generate_cre_dataset(n = n[j], rho = 0, effect_modifiers = eff_mod, p = 9,
                                           effect_size = 1, binary = FALSE, no_tree = FALSE,
                                           no_overlap = FALSE)
      y <- dataset_cont[["y"]]
      z <- dataset_cont[["z"]]
      X <- as.data.frame(dataset_cont[["X"]])
      tau <- dataset_cont[["tau"]]
      X_names <- names(as.data.frame(X))

      # Estimate CRE
      print("Testing DRLearner CATE estimation with AIPW ITE estimation")
      cre_result <- cre(y, z, X, ratio_dis, ite_method_dis = "bcf",
                        include_ps_dis = NA, ps_method_dis = "SL.xgboost",
                        or_method_dis = "SL.xgboost", ite_method_inf = "aipw",
                        include_ps_inf = NA, ps_method_inf = "SL.xgboost",
                        or_method_inf = "SL.xgboost", ntrees_rf, ntrees_gbm,
                        min_nodes, max_nodes, t, q, stability_selection = TRUE,
                        rules_method, pfer_val = 1, filter_cate = FALSE,
                        cate_method = "DRLearner", cate_SL_library = "SL.xgboost")

      # Extract Vectors
      y_inf <- cre_result$outcome_vector_inf
      z_inf <- cre_result$treatment_vector_inf
      X_inf <- cre_result$covariates_matrix_inf
      index <- which(y %in% y_inf)
      tX <- cre_result$rules_matrix_inf

      # CRE-AIPW
      est.tau.aipw <- cre_result$ite_list_inf$ite
      est.aipw = solve(t(tX) %*% tX) %*% (t(tX) %*% est.tau.aipw)
      tau.hat.aipw = as.vector(tX %*% est.aipw)

      # CRE-IPW
      est.tau.ipw <- estimate_ite_ipw(y_inf, z_inf, X_inf, ps_method = "SL.glm")
      est.ipw = solve(t(tX) %*% tX) %*% (t(tX) %*% est.tau.ipw)
      tau.hat.ipw = as.vector(tX %*% est.ipw)

      # CRE-SIPW
      est.tau.sipw <- estimate_ite_sipw(y_inf, z_inf, X_inf, ps_method = "SL.glm")
      est.sipw = solve(t(tX) %*% tX) %*% (t(tX) %*% est.tau.sipw)
      tau.hat.sipw = as.vector(tX %*% est.sipw)

      # CRE-CF
      est.tau.cf <- estimate_ite_cf(y_inf, z_inf, X_inf, include_ps = FALSE)[[1]]
      est.cf = solve(t(tX) %*% tX) %*% (t(tX) %*% est.tau.cf)
      tau.hat.cf = as.vector(tX %*% est.cf)

      # CRE-BCF
      est.tau.bcf <- estimate_ite_bcf(y_inf, z_inf, X_inf, ps_method = "SL.glm")[[1]]
      est.bcf = solve(t(tX) %*% tX) %*% (t(tX) %*% est.tau.bcf)
      tau.hat.bcf = as.vector(tX %*% est.bcf)

      # Output Predictions
      predictions <- cbind(tau.hat.ipw, tau.hat.sipw, tau.hat.aipw, tau.hat.bcf, tau.hat.cf, est.tau.cf, est.tau.bcf)
      bias.mat[j,] = apply(predictions, 2, get_bias, tau[index])
      mse.mat[j,] = apply(predictions, 2, get_mse, tau[index])
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  # Store Results
  result <- multiResultClass()
  result$bias.list <- bias.mat
  result$mse.list <- mse.mat
  return(result)
}
time.after = Sys.time()
time.after - time.before
stopCluster(cl)

# Extract and Store MSE
mse_mat_500 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  mse_mat_500[i,] <- matrix[[i]]$mse.list[1,]
}
colnames(mse_mat_500) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
mse_mat_500 <- as.data.frame(mse_mat_500)
mse_long_500 <- gather(mse_mat_500, method, mse, factor_key=TRUE)
mse_long_500 <- cbind(mse_long_500, rep(500, nrow(mse_long_500)))
colnames(mse_long_500) <- c("method", "mse", "size")

mse_mat_1000 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  mse_mat_1000[i,] <- matrix[[i]]$mse.list[2,]
}
colnames(mse_mat_1000) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
mse_mat_1000 <- as.data.frame(mse_mat_1000)
mse_long_1000 <- gather(mse_mat_1000, method, mse, factor_key=TRUE)
mse_long_1000 <- cbind(mse_long_1000, rep(1000, nrow(mse_long_1000)))
colnames(mse_long_1000) <- c("method", "mse", "size")

mse_mat_1500 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  mse_mat_1500[i,] <- matrix[[i]]$mse.list[3,]
}
colnames(mse_mat_1500) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
mse_mat_1500 <- as.data.frame(mse_mat_1500)
mse_long_1500 <- gather(mse_mat_1500, method, mse, factor_key=TRUE)
mse_long_1500 <- cbind(mse_long_1500, rep(1500, nrow(mse_long_1500)))
colnames(mse_long_1500) <- c("method", "mse", "size")

mse_mat_2000 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  mse_mat_2000[i,] <- matrix[[i]]$mse.list[4,]
}
colnames(mse_mat_2000) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
mse_mat_2000 <- as.data.frame(mse_mat_2000)
mse_long_2000 <- gather(mse_mat_2000, method, mse, factor_key=TRUE)
mse_long_2000 <- cbind(mse_long_2000, rep(2000, nrow(mse_long_2000)))
colnames(mse_long_2000) <- c("method", "mse", "size")

mse_long <- rbind(mse_long_500, mse_long_1000, mse_long_1500, mse_long_2000)
write.csv(mse_long, file = paste0("output_sim/mse_long_mt", process, ".csv"))

# Extract and Store Bias
bias_mat_500 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  bias_mat_500[i,] <- matrix[[i]]$bias.list[1,]
}
colnames(bias_mat_500) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
bias_mat_500 <- as.data.frame(bias_mat_500)
bias_long_500 <- gather(bias_mat_500, method, bias, factor_key=TRUE)
bias_long_500 <- cbind(bias_long_500, rep(500, nrow(bias_long_500)))
colnames(bias_long_500) <- c("method", "bias", "size")

bias_mat_1000 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  bias_mat_1000[i,] <- matrix[[i]]$bias.list[2,]
}
colnames(bias_mat_1000) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
bias_mat_1000 <- as.data.frame(bias_mat_1000)
bias_long_1000 <- gather(bias_mat_1000, method, bias, factor_key=TRUE)
bias_long_1000 <- cbind(bias_long_1000, rep(1000, nrow(bias_long_1000)))
colnames(bias_long_1000) <- c("method", "bias", "size")

bias_mat_1500 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  bias_mat_1500[i,] <- matrix[[i]]$bias.list[3,]
}
colnames(bias_mat_1500) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
bias_mat_1500 <- as.data.frame(bias_mat_1500)
bias_long_1500 <- gather(bias_mat_1500, method, bias, factor_key=TRUE)
bias_long_1500 <- cbind(bias_long_1500, rep(1500, nrow(bias_long_1500)))
colnames(bias_long_1500) <- c("method", "bias", "size")

bias_mat_2000 <- matrix(NA, nrow = nsim, ncol = 7)
for (i in 1:nsim) {
  bias_mat_2000[i,] <- matrix[[i]]$bias.list[4,]
}
colnames(bias_mat_2000) <- c("CRE-IPW", "CRE-SIPW", "CRE-AIPW", "CRE-BCF", "CRE-CF", "CF", "BCF")
bias_mat_2000 <- as.data.frame(bias_mat_2000)
bias_long_2000 <- gather(bias_mat_2000, method, bias, factor_key=TRUE)
bias_long_2000 <- cbind(bias_long_2000, rep(2000, nrow(bias_long_2000)))
colnames(bias_long_2000) <- c("method", "bias", "size")

bias_long <- rbind(bias_long_500, bias_long_1000, bias_long_1500, bias_long_2000)
write.csv(bias_long, file = paste0("output_sim/bias_long_mt", process, ".csv"))


### PLOT

library(ggplot2)
library(gridExtra)
library(gapminder)
library(dplyr)

margin_spacer <- function(x) {
  # where x is the column in your dataset
  left_length <- nchar(levels(factor(x)))[1]
  if (left_length > 8) {
    return((left_length - 8) * 4)
  }
  else
    return(0)
}

pdf("app_mse.pdf")
mse_plot <- ggplot(mse_long, aes(x = method, y = mse, fill = method)) +
  facet_wrap(~size) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(name = "Mean Squared Error (MSE)", limits=c(0, 0.35)) +
  scale_x_discrete(name = "Method") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
mse_plot
dev.off()

pdf("app_bias.pdf")
bias_plot <- ggplot(bias_long, aes(x = method, y = bias, fill = method)) +
  facet_wrap(~size) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(name = "Bias", limits=c(-0.35, 0.35)) +
  scale_x_discrete(name = "Method") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))  +
  geom_hline(aes(yintercept = 0))
bias_plot
dev.off()
