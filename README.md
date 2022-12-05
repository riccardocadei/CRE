| Resource    |  Github Actions      |  Code Coverage  |
| ----------  | -------------------- | --------------- |
| Platforms   | Windows, macOS, Linux|    codecov      |
| R CMD check | [![R-CMD-check](https://github.com/nsaph-software/CRE/workflows/R-CMD-check/badge.svg)](https://github.com/nsaph-software/CRE/actions) | [![codecov](https://codecov.io/gh/NSAPH-Software/CRE/branch/develop/graph/badge.svg?token=UMSVOYRKGA)](https://app.codecov.io/gh/NSAPH-Software/CRE)|


# CRE

The CRE package Provides an interpretable identification of subgroups with heterogeneous causal effect. The heterogeneous subgroups are discovered through ensemble learning of causal rules. Causal rules are highly interpretable if-then statement that recursively partition the features space into heterogeneous subgroups. A small number of significant causal rules are selected through Stability Selection to control for family-wise error rate in the finite sample setting. It proposes various estimation methods for the conditional causal effects for each discovered causal rule.  It is highly flexible and multiple causal estimands and imputation methods are implemented.

## Summary
Interpretable Subgroups Identification through Ensemble Learning of Causal Rules

## Installation

```r
library("devtools")
install_github("NSAPH-Software/CRE", ref="develop")
library("CRE")
```

## Usage

Input parameters:

**`y`** The observed response (outcome) vector.     
**`z`** The treatment (exposure, policy) vector.    
**`X`** The covariate matrix.    
**`method_params`** Parameters for estimating the individual treatment effect, including:    
__Parameters for Discovery__           
- **`ratio_dis`** The ratio of data delegated to the discovery sub-sample.     
- **`ite_method_dis`** The method to estimate the discovery sub-sample in estimating individual treatment effect (ITE).    
- **`include_ps_dis`**  Whether or not to include propensity score estimate as a covariate in discovery ITE estimation, considered only for BART, or CF.    
- **`ps_method_dis`** The estimation model for the propensity score on the discovery sub-sample.    
- **`or_method_dis`** The estimation model for the outcome regressions estimate_ite_aipw on the discovery sub-sample.      
__Parameters for Inference__     
- **`ite_method_inf`** The method to estimate the inference sample ITE.    
- **`include_ps_inf`** Whether or not to include propensity score estimate as a covariate in inference ITE estimation, considered only for BART, or CF.     
- **`ps_method_inf`** The estimation model for the propensity score on the inference subsample.     
- **`or_method_inf`** The estimation model for the outcome regressions in estimate_ite_aipw on the inference subsample.     
__Other Parameters__
- **`include_offset`** Whether or not to include an offset when estimating the ITE, for Poisson only.     
- **`offset_name`** The name of the offset, if it is to be included.     
- **`cate_method`** The method to estimate the conditional average treatment effect (CATE) values.     
- **`cate_SL_library`** The library used if cate_method is set to DRLearner.    
- **`filter_cate`** Whether or not to filter rules with p-value <= 0.05.   
**`hyper_params`** The list of parameters required to tune the functions, including:    
- **`intervention_vars`** Intervention-able variables used for Rules Generation.     
- **`ntrees_rf`** The number of decision trees for randomForest.     
- **`ntrees_gbm`** The number of decision trees for gradient boosting.     
- **`node_size`** The minimum size of the trees' terminal nodes.      
- **`max_nodes`** The maximum number of terminal nodes trees in the forest can have.    
- **`max_depth`** The number of top levels from each tree considered to extract conditions.    
- **`replace`** Boolean variable for replacement in bootstrapping.     
- **`max_decay`** Decay Threshold for pruning the rules.     
- **`type_decay`** Decay Type for pruning the rules (1: relative error; 2: error).     
- **`t_ext`** The threshold to define too generic or too specific (extreme) rules.     
- **`t_corr`** The threshold to define correlated rules.     
- **`stability_selection`** Whether or not using stability selection for selecting the causal rules.
- **'cutoff'**:  Threshold defining the minimum cutoff value for the stability scores.   
- **`pfer`** Upper bound for the per-family error rate (tolerated amount of falsely selected rules).    

### A note on the parameters

**(1)** Options for the ITE estimation are as follows: 

- Inverse Propensity Weighting (`ipw`)
- Stabilized Inverse Propensity Weighting (`sipw`)
- Augmented Inverse Propensity Weighting (`aipw`)
- Outcome Regression (`oreg`)
- Bayesian Additive Regression Trees (`bart`)
- Bayesian Causal Forests (`bcf`)
- Causal Forests (`cf`)
- Poisson Regression (`poisson`)

**(2)** The `include_ps_dis` and `include_ps_inf` arguments will only be considered if the ITE method selected is `bart`, `cf`.


The CRE package can generate synthetic data that can be used to test different features of the package. At the current implementation, the code can generate data with continuous or binary outcomes. 

```r
  set.seed(9687)
  dataset <- generate_cre_dataset(n = 300, 
                                  rho = 0, 
                                  n_rules = 2, 
                                  p = 10,
                                  effect_size = 2, 
                                  binary_covariates = TRUE,
#'                                binary_outcome = FALSE)
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]

  method_params = list(ratio_dis = 0.25,
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

 hyper_params = list(intervention_vars = c(),
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

cre_results <- cre(y, z, X, method_params, hyper_params)

```

## References

Lee, K., Bargagli-Stoffi, F. J., & Dominici, F. (2020). Causal rule ensemble:
Interpretable inference of heterogeneous treatment effects.  arXiv preprint arXiv:2009.09036.
