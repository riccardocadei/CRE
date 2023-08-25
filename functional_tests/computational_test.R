library("CRE")

method_params <- list(ratio_dis = 0.5,
                      method = "tlearner",
                      learner_ps = "SL.xgboost",
                      learner_y = "SL.xgboost")


computation_time <- data.frame(p = numeric(0),
                               seed = numeric(0),
                               n = numeric(0),
                               time = numeric(0))

for (p in c(5,10,50)) {
  print(p)
  for (seed in c(1,2,3,4,5)) {
    set.seed(seed)
    print(seed)
    for (n in round(10^seq(2, 6, length.out = 5))){
      print(n)
      dataset <- generate_cre_dataset(n = n,
                                      rho = 0,
                                      p = p,
                                      effect_size = 5,
                                      n_rules = 2,
                                      binary_covariates = TRUE,
                                      binary_outcome = FALSE,
                                      confounding = "no")
      y <- dataset[["y"]]
      z <- dataset[["z"]]
      X <- dataset[["X"]]

      hyper_params <- list(intervention_vars = NULL,
                           offset = NULL,
                           ntrees = 20,
                           node_size = 20,
                           max_rules = 50,
                           max_depth = 2,
                           t_decay = 0,
                           t_ext = 0.02,
                           t_corr = 0.5,
                           t_pvalue = 0.05,
                           stability_selection = "vanilla",
                           cutoff = 0.8,
                           pfer = 0.5,
                           B = 20,
                           subsample = 0.5,
                           verbose = FALSE)

      start_time <- Sys.time()
      cre(y, z, X, method_params, hyper_params)
      end_time <- Sys.time()
      time_i <- as.numeric(difftime(end_time, start_time, units = "secs"))

      time <- data.frame(p = p, seed = seed, n = n, time = time_i)
      computation_time <- rbind(computation_time, time)
    }
  }
}
computation_time
write.csv(computation_time,
          "functional_tests/results/computation_time_JOSS.csv",
          row.names = FALSE)

