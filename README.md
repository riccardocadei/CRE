| Resource    |  GitHub Actions      |  Code Coverage  |
| ----------  | -------------------- | --------------- |
| Platforms   | Windows, macOS, Linux|    codecov      |
| R CMD check | [![R-CMD-check](https://github.com/nsaph-software/CRE/workflows/R-CMD-check/badge.svg)](https://github.com/nsaph-software/CRE/actions) | [![codecov](https://codecov.io/gh/NSAPH-Software/CRE/branch/develop/graph/badge.svg?token=UMSVOYRKGA)](https://app.codecov.io/gh/NSAPH-Software/CRE)|


# CRE
#### Interpretable Discovery and Inference of Heterogeneous Treatment Effects

In health and social sciences, it is critically important to identify subgroups of the study population where a treatment has notable heterogeneity in the causal effects with respect to the average treatment effect (ATE). The bulk of heterogeneous treatment effect (HTE) literature focuses on two major tasks: (i) estimating HTEs by examining the conditional average treatment effect (CATE); (ii) discovering subgroups of a population characterized by HTE. 

Several methodologies have been proposed for both tasks, but providing interpretability in the results is still an open challenge. [Bargagli-Stoffi et al. (2023)](https://arxiv.org/abs/2009.09036) proposed Causal Rule Ensemble, a new method for HTE characterization in terms of decision rules, via an extensive exploration of heterogeneity patterns by an ensemble-of-trees approach, enforcing stability in the discovery. CRE is an R Package providing a flexible implementation of the Causal Rule Ensemble algorithm.


## Installation

Installing from CRAN.

```r
install.packages("CRE")
```

Installing the latest developing version. 

```r
library(devtools)
install_github("NSAPH-Software/CRE", ref="develop")
```

Import.

```r
library("CRE")
```
The full list of required dependencies can be found in project [DESCRIPTION](https://github.com/NSAPH-Software/CRE/blob/main/DESCRIPTION) file.

## Arguments

__Data (required)__   
**`y`** The observed response/outcome vector (binary or continuous).
**`z`** The treatment/exposure/policy vector (binary).  

**`X`** The covariate matrix (binary or continuous).    

__Parameters (not required)__    
**`method_parameters`** The list of parameters to define the models used, including:
- **`ratio_dis`** The ratio of data delegated to the discovery sub-sample (default: 0.5). 
- **`ite_method_dis`** The method to estimate the individual treatment effect (ITE) on the discovery sub-sample (default: 'aipw') [1].        
- **`ps_method_dis`** The estimation model for the propensity score on the discovery sub-sample (default: 'SL.xgboost').     
- **`or_method_dis`** The estimation model for the outcome regressions estimate_ite_aipw on the discovery sub-sample (default: 'SL.xgboost').      
- **`ite_method_inf`** The method to estimate the individual treatment effect (ITE) on the inference sub-sample (default: 'aipw') [1].       
- **`ps_method_inf`** The estimation model for the propensity score on the inference subsample (default: 'SL.xgboost').     
- **`or_method_inf`** The estimation model for the outcome regressions in estimate_ite_aipw on the inference subsample (default: 'SL.xgboost').   

**`hyper_params`** The list of hyper parameters to finetune the method, including:
- **`intervention_vars`** Array with intervention-able covariates names used for Rules Generation. Empty or null array means that all the covariates are considered as intervention-able (default: NULL).  
- **`offset`** Name of the covariate to use as offset (i.e. 'x1') for T-Poisson ITE Estimation. NULL if not used (default: NULL).
- **`ntrees_rf`** A number of decision trees for random forest (default: 20).   
- **`ntrees_gbm`** A number of decision trees for the generalized boosted regression modeling algorithm. (default: 20).     
- **`node_size`** Minimum size of the trees' terminal nodes (default: 20).
- **`max_nodes`** Maximum number of terminal nodes per tree (default: 5).  
- **`max_depth`** Maximum rules length (default: 3).  
- **`replace`** Boolean variable for replacement in bootstrapping (default: TRUE).     
- **`t_decay`** The decay threshold for rules pruning (default: 0.025).          
- **`t_ext`** The threshold to define too generic or too specific (extreme) rules (default: 0.01).     
- **`t_corr`** The threshold to define correlated rules (default: 1). 
- **`t_pvalue`** The threshold to define statistically significant rules (default: 0.05).
- **`stability_selection`** Whether or not using stability selection for selecting the causal rules (default: TRUE).
- **`cutoff`** Threshold defining the minimum cutoff value for the stability scores (default: 0.9).
- **`pfer`** Upper bound for the per-family error rate (tolerated amount of falsely selected rules) (default: 1).
- **`penalty_rl`** Order of penalty for rules length during LASSO for Causal
Rules Discovery (i.e. 0: no penalty, 1: rules_length, 2: rules_length^2) (default: 1).
- **`B`** Number of bootstrap samples for stability selection in rules selection and uncertainty quantification in estimation (default: 20).
- **`subsample`** Bootstrap ratio subsample and stability selection in rules selection, and uncertainty quantification in estimation (default: 0.5).

__Additional Estimates (not required)__    
**`ite`** The estimated ITE vector. If given, both the ITE estimation steps in Discovery and Inference are skipped (default: NULL).


## Notes

**[1]** Options for the ITE estimation are as follows: 
- [S-Learner](https://www.tandfonline.com/doi/pdf/10.1198/jcgs.2010.08162?casa_token=OZlb52s-4bMAAAAA:60Ye7a0sTgZV-K2EtRs_zyE1GD2H3xgbleo47Fz1Mte3ZCiz097oV8QYTMbRvHTUoy57ukcrvEk) (`slearner`)
- [T-Learner](https://journals.sagepub.com/doi/pdf/10.1002/dir.10035?casa_token=Cxhw8gRR8bEAAAAA:Dtjt4dDd_CTFg0G0w7TiBYEFjqT1p2ufeyFAX2R8GrUUNmkyrLUKYOhyK_VCQvXzO8rddPjOK5A) (`tlearner`)
- [T-Poisson](https://journals.sagepub.com/doi/pdf/10.1002/dir.10035?casa_token=Cxhw8gRR8bEAAAAA:Dtjt4dDd_CTFg0G0w7TiBYEFjqT1p2ufeyFAX2R8GrUUNmkyrLUKYOhyK_VCQvXzO8rddPjOK5A) (`tpoisson`)
- [X-Learner](https://www.pnas.org/doi/epdf/10.1073/pnas.1804597116) (`xlearner`)
- [Augmented Inverse Probability Weighting](https://www.jstor.org/stable/pdf/2290910.pdf?casa_token=W6IlQUhMN4cAAAAA:R-BueV-53W70b3EcdDXGZGLtDQgLt-97wTIS_6gIbdvtWwjVOqSDqm5APxY4BOfDXbx9rrO0f4vjKXjKC_kYxiwFIJGvxG0rx2glk3NCxT18x2b0PA) (`aipw`)
- [Causal Forests](https://projecteuclid.org/journals/annals-of-statistics/volume-47/issue-2/Generalized-random-forests/10.1214/18-AOS1709.full) (`cf`)
- [Causal Bayesian Additive Regression Trees](https://www.tandfonline.com/doi/pdf/10.1198/jcgs.2010.08162?casa_token=tlBxbbiESdgAAAAA:3RY4NQrwexcIyo2F-v1SIty7ZSMD8qNIf5uiHqlN8G3BWCisPDEtHOWn5IK23tAdN2zOqzNo-p4) (`bart`)

if other estimates of the ITE are provided in `ite` additional argument, both the ITE estimations in discovery and inference are skipped and those values estimates are used instead.


## Examples

**Example 1** (*default parameters*)
```R
set.seed(2023)
dataset <- generate_cre_dataset(n = 2000, 
                                rho = 0, 
                                n_rules = 2, 
                                p = 10,
                                effect_size = 5, 
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = "no")
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]

cre_results <- cre(y, z, X)
summary(cre_results)
plot(cre_results)
ite_pred <- predict(cre_results, X) 
```

**Example 2** (*personalized ite estimation*)
```R
set.seed(2023)
dataset <- generate_cre_dataset(n = 2000, 
                                rho = 0, 
                                n_rules = 2, 
                                p = 10,
                                effect_size = 5, 
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = "no")
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]

# personalized ITE estimation (S-Learner with Linear Regression)
model <- lm(y ~., data = data.frame(y = y, X = X, z = z) )
ite_pred <- predict(model, newdata = data.frame(X = X, z = z))

cre_results <- cre(y, z, X, ite = ite_pred)
summary(cre_results)
plot(cre_results)
ite_pred <- predict(cre_results, X)
```

**Example 3** (*setting parameters*)
```R
  set.seed(2023)
  dataset <- generate_cre_dataset(n = 2000, 
                                  rho = 0, 
                                  n_rules = 2, 
                                  p = 10,
                                  effect_size = 2, 
                                  binary_covariates = TRUE,
                                  binary_outcome = FALSE,
                                  confounding = "no")
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]

  method_params = list(ratio_dis = 0.5,
                       ite_method ="aipw",
                       learner_ps = "SL.xgboost",
                       learner_y = "SL.xgboost")

 hyper_params = list(intervention_vars = c("x1","x2","x3","x4","x5","x6"),
                     offset = NULL,
                     ntrees = 20,
                     node_size = 20,
                     max_rules = 50,
                     max_depth = 2,
                     t_decay = 0.025,
                     t_ext = 0.025,
                     t_corr = 1,
                     t_pvalue = 0.05,
                     stability_selection = "vanilla",
                     cutoff = 0.8,
                     pfer = 0.1,
                     B = 50,
                     subsample = 0.1)

cre_results <- cre(y, z, X, method_params, hyper_params)
summary(cre_results)
plot(cre_results)
ite_pred <- predict(cre_results, X)
```

More synthetic data sets can be generated using `generate_cre_dataset()`.


## Simulations

Discovery.

```r
CRE/functional_tests/experiments/discovery.R
```

Estimation.

```r
CRE/functional_tests/experiments/estimation.R
```
More exhaustive simulation studies and real world experiment of CRE package can be found at [https://github.com/NSAPH-Projects/cre_applications](https://github.com/NSAPH-Projects/cre_applications).


## Code of Conduct

Please note that the CRE project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct). By contributing to this project, you agree to abide by its terms. More information about the opening issues and contributing (i.e., git branching model) can be found on [CRE website](https://nsaph-software.github.io/CRE/articles/Contribution.html).



## References

Causal Rule Ensemble ([methodological paper](https://arxiv.org/abs/2009.09036))
```bibtex
@article{bargagli2023causal,
  title={{Causal rule ensemble: Interpretable Discovery and Inference of Heterogeneous Treatment Effects}},
  author={Bargagli-Stoffi, Falco J and Cadei, Riccardo and Lee, Kwonsang and Dominici, Francesca},
  journal={arXiv preprint arXiv:2009.09036},
  year={2023}
}
```

CRE (software paper)
```bibtex
@article{cadei2023CRE,
  title = {CRE: an R package for Interpretable Discovery and Estimation of Heterogeneous Treatment Effect},
  author = {Cadei, Riccardo and Khoshnevis, Naeem and Bargagli-Stoffi, Falco J and Lee, Kwonsang and Garcia, Daniela Maria},
  year = {2023},
  journal={Working paper},
  url = {},
}
```

CRE ([CRAN package](https://CRAN.R-project.org/package=CRE))
```bibtex
@Manual{khoshnevis2023CRE,
  title = {CRE: Interpretable Subgroups Identification Through Ensemble Learning of Causal Rules},
  author = {Khoshnevis, Naeem and Garcia, Daniela Maria and Cadei, Riccardo and Lee, Kwonsang  and Bargagli-Stoffi, Falco J},
  year = {2023},
  note = {R package version 0.2.0.9000},
  url = {https://github.com/NSAPH-Software/CRE},
}
```
