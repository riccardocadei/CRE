#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Estimates the Conditional Average Treatment Effect given a standardized
#' vector of Individual Treatment Effects, a standardized matrix of causal
#' rules, a list of causal rules.
#'
#' @param y_inf The outcome vector for the inference subsample.
#' @param z_inf The treatment vector for the inference subsample.
#' @param X_inf The covariate vector for the inference subsample.
#' @param X_names The names of the covariates.
#' @param include_offset Whether or not to include an offset when estimating the
#'  ITE, for poisson only.
#' @param offset_name The name of the offset, if it is to be included.
#' @param rules_matrix_inf The standardized causal rules matrix for the
#' inference subsample.
#' @param select_rules_interpretable The list of select causal rules in terms of
#' coviariate names.
#' @param cate_method The method to estimate the CATE values.
#' @param ite_inf The estimated ITEs for the inference subsample.
#' @param sd_ite_inf The standard deviations for the estimated ITEs for the
#'   inference subsample.
#' @param cate_SL_library The library used if cate_method is DRLearner.
#' @param t_pvalue The threshold to define statistically significant rules
#' (filter only causal decision rules with p-value <= t_pvalue).
#'
#' @return
#' a matrix of CATE estimates
#'
#' @import stats
#' @keywords internal
#'
#'
estimate_cate <- function(y_inf, z_inf, X_inf, X_names, include_offset,
                          offset_name, rules_matrix_inf,
                          select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, t_pvalue) {

  # TODO: Move different methods to a new function.
  # TODO: Pass the number of cores explicitly.
  # TODO: Not all input parameters are needed by all methods. Needs refactoring.

  # Handling global variable error.
  `%>%` <- magrittr::`%>%`

  Rule <- rule <- tau <- se <- . <- Estimate <- NULL

  if (any(is.na(select_rules_interpretable))) {
    # No Causal Rules Selected: Estimate ATE

    cate_model <- stats::lm(ite_inf ~ 1)
    cate_coeff <- summary(cate_model)$coefficients
    colnames(cate_coeff) <- c("Estimate", "Std_Error", "Z_Value", "P_Value")
    cate_names <- rownames(cate_coeff) %>%
      stringr::str_replace_all("(Intercept)", "BATE")

    cate_summary <- data.frame(Rule = cate_names) %>% cbind(cate_coeff)
    rownames(cate_summary) <- 1:nrow(cate_summary)

  } else {
    # Estimate CATE

    if (cate_method %in% c("poisson")) {

      if (!requireNamespace("gnm", quietly = TRUE)) {
        stop(
          "Package \"gnm\" must be installed to use this function.",
          call. = FALSE
        )
      }

      colnames(rules_matrix_inf) <- select_rules_interpretable
      colnames(X_inf) <- X_names
      if (include_offset) {
        X_offset <- X_inf[,which(X_names == offset_name)]
        X_inf <- X_inf[,-which(X_names == offset_name)]

        # Fit gnm model
        conditional_gnm <- gnm::gnm(y_inf ~ offset(log(X_offset)) + z_inf +
                                      z_inf:rules_matrix_inf + X_inf,
                                    family = stats::poisson(link = "log"))
      } else {
        # Fit gnm model
        conditional_gnm <- gnm::gnm(y_inf ~ z_inf + z_inf:rules_matrix_inf
                                          + X_inf,
                                    family = stats::poisson(link = "log"))
      }
      cate_model <- summary(conditional_gnm)$coefficients
      cate_names <- rownames(cate_model) %>%
        stringr::str_remove_all("X_inf") %>%
        stringr::str_remove_all("rules_matrix_inf") %>%
        stringr::str_replace_all("z_inf", "treatment")

      cate_temp <- data.frame(Predictor = cate_names) %>%
        cbind(cate_model)
      colnames(cate_temp) <- c("Rule", "Estimate", "Std_Error",
                               "Z_Value", "P_Value")
      cate_summary <- subset(cate_temp, cate_temp$P_Value <= t_pvalue |
                                      cate_temp$Rule == "(BATE)")
      rownames(cate_summary) <- 1:nrow(cate_summary)
      cate_model <- conditional_gnm
    } else if (cate_method %in% c("DRLearner")) {
      # split the data evenly
      split <- sample(nrow(X_inf), nrow(X_inf) * 0.5, replace = FALSE)

      # assign names to rules matrix and X matrix
      colnames(rules_matrix_inf) <- select_rules_interpretable
      colnames(X_inf) <- X_names

      # generate new data frames
      y_inf_a <- y_inf[split]
      y_inf_b <- y_inf[-split]
      X_inf_a <- as.data.frame(X_inf[split,])
      X_inf_b <- as.data.frame(X_inf[-split,])
      z_inf_a <- z_inf[split]
      z_inf_b <- z_inf[-split]
      rules_matrix_inf_a <- rules_matrix_inf[split,]
      rules_matrix_inf_b <- rules_matrix_inf[-split,]

      # on set A, train a model to predict Z using X,
      # then make predictions on set B
      sl_z <- SuperLearner::SuperLearner(Y = z_inf_a,
                                         X = X_inf_a,
                                         newX = X_inf_b,
                                         family = binomial(),
                                         SL.library = cate_SL_library,
                                         cvControl = list(V=0))
      phat <- sl_z$SL.predict

      # generate CATE estimates for set A, predict set B
      sl_y <- SuperLearner::SuperLearner(Y = y_inf_a,
                                         X = data.frame(X = X_inf_a,
                                                        Z = z_inf_a),
                                         family = gaussian(),
                                         SL.library = cate_SL_library,
                                         cvControl = list(V=0))
      pred_0s <- stats::predict(sl_y,
                                data.frame(X = X_inf_b,
                                           Z = rep(0, nrow(X_inf_b))),
                                onlySL = TRUE)
      pred_1s <- stats::predict(sl_y,
                                data.frame(X = X_inf_b,
                                           Z = rep(1, nrow(X_inf_b))),
                                onlySL = TRUE)

      cate <- pred_1s$pred - pred_0s$pred

      # generate AIPW estimate
      apo_1 <- pred_1s$pred + (z_inf_b*(y_inf_b - pred_1s$pred)/(phat))
      apo_0 <- pred_0s$pred + ((1 - z_inf_b)*(y_inf_b - pred_0s$pred)/(1-phat))

      delta <- apo_1 - apo_0

      # regress AIPW onto the rules
      DRLearner <- stats::lm(delta ~ rules_matrix_inf_b)
      cate_model <- summary(DRLearner)$coefficients
      colnames(cate_model) <- c("Estimate", "Std_Error", "Z_Value", "P_Value")
      if (length(select_rules_interpretable)==1) {
        cate_names <- rownames(cate_model) %>%
        stringr::str_replace_all("rules_matrix_inf_b",
                                 select_rules_interpretable) %>%
        stringr::str_replace_all("(Intercept)", "BATE")
      } else {
        cate_names <- rownames(cate_model) %>%
        stringr::str_remove_all("rules_matrix_inf_b") %>%
        stringr::str_replace_all("(Intercept)", "BATE")
      }

      cate_temp <- data.frame(Rule = cate_names) %>%
        cbind(cate_model)
      cate_summary <- subset(cate_temp, cate_temp$P_Value <= t_pvalue |
                                      cate_temp$Rule == "(BATE)")
      rownames(cate_summary) <- 1:nrow(cate_summary)
    } else if (cate_method == "bart-baggr") {

      if (!requireNamespace("baggr", quietly = TRUE)) {
        stop(
          "Package \"baggr\" must be installed to use this function.",
          call. = FALSE
        )
      }


      stopifnot(ncol(rules_matrix_inf) == length(select_rules_interpretable))
      df_rules_factor <- as.data.frame(rules_matrix_inf) %>%
        dplyr::transmute_all(as.factor)
      names(df_rules_factor) <- select_rules_interpretable
      joined_ite <- data.frame(group = 1:length(ite_inf),
                               tau = ite_inf, se = sd_ite_inf)

      # Generate CATE data frame with ATE
      # TODO: Pass number of cores to baggr. Do not use available cores.
      baggr_ite <- suppressWarnings(
        baggr::baggr(joined_ite))
      sum_ate <- 0
      sum_sd_ate <- 0
      n_samples <- length(baggr_ite$fit@sim$samples)
      for (i in 1:n_samples) {
        sum_ate <- sum_ate + mean(baggr_ite$fit@sim$samples[[i]]$`mu[1]`)
        sum_sd_ate <- sum_sd_ate + mean(baggr_ite$fit@sim$samples[[i]]$`tau[1]`)
      }

      ate <- sum_ate / n_samples
      sd_ate <- sum_sd_ate / n_samples
      cate_means <- data.frame(Rule = "(BATE)",
                               CATE = ate,
                               CI_lower = ate - (1.96 * sd_ate),
                               CI_upper = ate + (1.96 * sd_ate))

      # Determine CATE manually for each rule
      for (i in 1:length(select_rules_interpretable)) {
        df_temp <- data.frame(tau = ite_inf, se = sd_ite_inf,
                              rule = df_rules_factor[,i]) %>%
          dplyr::filter(rule == 1) %>% dplyr::select(-rule)
        df_temp <- df_temp %>% dplyr::summarize(group = 1:nrow(df_temp),
                                                tau,
                                                se)

          # TODO: Pass number of cores to baggr. Do not use available cores.
          baggr_ite_temp <- suppressWarnings(
          baggr::baggr(df_temp))

        sum_cate_temp <- 0
        sum_sd_cate_temp <- 0
        n_samples_temp <- length(baggr_ite_temp$fit@sim$samples)
        for (j in 1:n_samples_temp) {
          sum_cate_temp <- sum_cate_temp +
            mean(baggr_ite_temp$fit@sim$samples[[j]]$`mu[1]`)
          sum_sd_cate_temp <- sum_sd_cate_temp +
            mean(baggr_ite_temp$fit@sim$samples[[j]]$`tau[1]`)
        }
        cate_temp <- sum_cate_temp / n_samples_temp
        sd_cate_temp <- sum_sd_cate_temp / n_samples_temp

        cate_temp <- data.frame(Rule = select_rules_interpretable[i],
                                CATE = cate_temp,
                                CI_lower = cate_temp - (1.96 * sd_cate_temp),
                                CI_upper = cate_temp + (1.96 * sd_cate_temp))
        cate_means <- rbind(cate_means, cate_temp)
      }
      cate_summary <- cate_means
      # TODO: return CATE model
      cate_model <- NULL
    } else if (cate_method == "cf-means") {
      stopifnot(ncol(rules_matrix_inf) == length(select_rules_interpretable))
      df_rules_factor <- as.data.frame(rules_matrix_inf) %>%
        dplyr::transmute_all(as.factor)
      names(df_rules_factor) <- select_rules_interpretable
      joined_ite_rules <- cbind(ite_inf, sd_ite_inf, df_rules_factor)

      # Generate CATE data frame with ATE
      cate_means <- data.frame(Rule = "()",
                               CATE = mean(ite_inf),
                               CI_lower = mean(ite_inf) -
                                 (1.96 * mean(sd_ite_inf)),
                               CI_upper = mean(ite_inf) +
                                 (1.96 * mean(sd_ite_inf)))

      # Determine CATE manually for each rule
      for (i in 1:length(select_rules_interpretable)) {
        df_temp <- data.frame(ite_inf = joined_ite_rules[,1],
                              sd_ite_inf = joined_ite_rules[,2],
                              rule = joined_ite_rules[,i + 2]) %>%
          dplyr::filter(rule == 1)
        cate_temp <- data.frame(Rule = select_rules_interpretable[i],
                                CATE = mean(df_temp$ite),
                                CI_lower = mean(df_temp$ite_inf) -
                                  (1.96 * mean(df_temp$sd_ite_inf)),
                                CI_upper = mean(df_temp$ite_inf) +
                                  (1.96 * mean(df_temp$sd_ite_inf)))
        cate_means <- rbind(cate_means, cate_temp)
      }
      cate_summary <- cate_means
      # TODO: return CATE model
      cate_model <- NULL
    } else if (cate_method == "linreg") {
      stopifnot(ncol(rules_matrix_inf) == length(select_rules_interpretable))
      df_rules_factor <- as.data.frame(rules_matrix_inf) %>%
        dplyr::transmute_all(as.factor)
      names(df_rules_factor) <- select_rules_interpretable
      joined_ite_rules <- cbind(ite_inf, df_rules_factor)

      # Fit linear regression model with contr.treatment
      old_op <- options()
      on.exit(options(old_op))

      # then extract coefficients and confidence intervals
      options(contrasts = rep("contr.treatment", 2))
      model1_cate <- stats::lm(ite_inf ~ ., data = joined_ite_rules)
      model1_coef <- summary(model1_cate)$coef[,c(1,4)] %>% as.data.frame()
      model1_ci <- stats::confint(model1_cate) %>% as.data.frame()
      model1_ci <- model1_ci[!is.na(model1_ci[1]),]

      # Generate model 1 data frame
      cate_reg_orig <- model1_coef %>% cbind(model1_ci)
      cate_reg_orig_names <- stringr::str_extract(row.names(cate_reg_orig),
                                                  "`.*`") %>%
                             stringr::str_remove_all("`")
      cate_reg_orig_names[1] <- "(BATE)"
      cate_temp <- data.frame(Rule = cate_reg_orig_names,
                              Estimate = cate_reg_orig[,1],
                              CI_lower = cate_reg_orig[,3],
                              CI_upper = cate_reg_orig[,4],
                              P_Value = cate_reg_orig[,2])
      row.names(cate_temp) <- 1:nrow(cate_temp)
      cate_summary <- subset(cate_temp, cate_temp$P_Value <= t_pvalue |
                                      cate_temp$Rule == "(BATE)")
      cate_model <- model1_cate
    } else {
      stop("Error: No CATE Estimation method specified.")
    }
  }

  # Return final results
  cate = list(summary = cate_summary, model = cate_model)
  return(cate)
}
