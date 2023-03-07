---
title: 'CRE: A R package for Interpretable inference of heterogeneous treatment effects'
tags:
  - R
  - causal inference
  - heterogeneous effect 
  - interpretability
  - machine learning
authors:
  - name: Riccardo Cadei
    orcid: 0000-0003-2416-8943
    equal-contrib: true
    affiliation: "1, 2"
  - name: Naeem Khoshnevis
    orcid: 0000-0003-4315-1426
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: "1"
  - name: Falco J. Bargagli Stoffi
    orcid: 0000-0002-6131-8165
    affiliation: "1"
affiliations:
 - name: Department of Biostatistics, Harvard School of Public Health
   index: 1
 - name: School of Computer and Communication Sciences, EPFL
   index: 2
date: 2 November 2022
bibliography: paper.bib
header-includes: 
 - \usepackage{algorithm}
 - \usepackage{bm}

---

# Summary

The bulk of heterogeneous treatment effect (HTE) literature focuses on two major tasks [@dwivedi2020stable]: (i) estimating HTEs by examining the conditional average treatment effect (CATE); (ii) discovering subgroups of a population characterized by HTE. 
Several methodologies have already been proposed for both the tasks, but providing interpretability in the results is still an open challenge. Interpretability is a non-mathematical concept, yet it is often defined as the degree to which a human can understand the cause of a decision [kim2016examples], [miller2018explanation], [lakkaraju2016interpretable], [wang2022causal]. Honest Causal Tree [athey2016] fits perfectly this definition, but despite its high interpretability, it tends to be highly unstable and to find an oversimplified representation of treatment heterogeneity. To accommodate these shortcomings, we implemented CRE, a new R package to discover heterogeneous subgroups through an ensemble-of-trees approach. CRE provides an interpretable representation of the HTE on observational data via an extensive exploration of complex heterogeneity patterns, while guaranteeing high stability in the discovery. 


# Statement of need



# Algorithm

See [@Lee:2020].

# Example

Generate Synthetic Data.
```R
dataset <- generate_cre_dataset(n = 2000, 
                                rho = 0, 
                                n_rules = 4, 
                                p = 10,
                                effect_size = 2, 
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = "no")
y <- dataset$y
z <- dataset$z
X <- dataset$X
```

Example 1. Run CRE with default parameters.

```R
cre_results <- cre(y, z, X)
```

Example 2. Run CRE with customized ITE estimator.
```R
ite_pred <- ... # personalized ite estimation
cre_results <- cre(y, z, X, ite = ite_pred)
```

Example 3. Run CRE customized parameters.
```R
method_params <- list(ratio_dis = 0.25,
                      ite_method_dis="aipw",
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      ite_method_inf = "aipw",
                      ps_method_inf = "SL.xgboost",
                      oreg_method_inf = "SL.xgboost")

hyper_params <- list(intervention_vars = c("x1","x2","x3","x4"),
                     offset = NULL,
                     ntrees_rf = 20,
                     ntrees_gbm = 20,
                     node_size = 20,
                     max_nodes = 5,
                     max_depth = 3,
                     t_decay = 0.025,
                     t_ext = 0.025,
                     t_corr = 1,
                     t_pvalue = 0.05,
                     replace = FALSE,
                     stability_selection = TRUE,
                     cutoff = 0.8,
                     pfer = 0.1,
                     penalty_rl = 1)

cre_results <- cre(y, z, X, method_params, hyper_params)
```

Summarize results.
```R
summary(cre_results)
```

Visualize results.
```R
plot(cre_results)
```

![Visualization of CRE HTE linear decomposition for Example 1. For each decision rules discovered, the corresponding AATE estimate with 95% confidence interval is reported in a range bar plot. The decision rules are order from the most vulnerable (high AATE) to the least, and the ATE is reported on top of the plot. \label{fig:example}](images/example.pdf)

# Acknowledgements

We acknowledge contributions from ...

# References
