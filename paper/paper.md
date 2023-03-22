---
title: 'CRE: An R package for interpretable discovery and inference of heterogeneous treatment effects'
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
  - name: Kwonsang Lee
    orcid: 0000-0002-5823-4331
    affiliation: "1"
  - name: Daniela Maria Garcia
    orcid: 0000-0003-3226-3561
    affiliation: "1"
  - name: Falco J. Bargagli Stoffi
    orcid: 0000-0002-6131-8165
    affiliation: "1"
affiliations:
 - name: Department of Biostatistics, Harvard School of Public Health
   index: 1
 - name: Department of Computer and Communication Science, EPFL
   index: 2
date: 15 March 2023
bibliography: paper.bib
header-includes: 
 - \usepackage{algorithm}
 - \usepackage{bm}
 - \usepackage{amsmath}

---

# Summary

In health and social sciences, it is critically important to identify interpretable subgroups of the study population where a treatment has notable heterogeneity in the causal effects with respect to the average treatment effect (ATE). Several approaches have already been proposed for heterogenous treatment effect (HTE) discovery, either estimating first the conditional average treatment effect (CATE) and indentifying heterogenous subgroups in a second stage [@foster2011subgroup; @bargagli2020heterogeneous; @hahn2020bayesian; @bargagli2022heterogeneous], either in a direct data-driven procedure [wang2022causal; nagpal2020interpretable]. Many of these methodologies are decision tree-based methodologies. In fact, tree-based approaches are based on efficient and easily implementable recursive mathematical programming (e.g., maximization in the heterogeneous treatment effects), they can be easily tweaked and adapted to different scenarios on the basis of the research question of interest, and they guarantee a high degree of interpretability---i.e., the degree to which a human can understand the cause of a decision [@lakkaraju2016interpretable]. Despite their appealing features, single-tree heterogeneity discovery is characterized by two main limitations: (i) instability in the identification of the subgroups, and (ii) reduced exploration of the potential heterogeneity.  To accommodate these shortcomings, @bargagli2023causal proposed Causal Rule Ensemble, a new method for interpretable HTE characterization in terms of decision rules, via an extensive exploration of heterogeneity patterns by an ensemble-of-trees approach, enforcing high stability in the discovery. `CRE` is an R Package providing a flexible implementation of the Causal Rule Ensemble algorithm [@bargagli2023causal].

# Statement of Need

Several methodologies for HTE estimation have already been proposed (together with the release of the corresponding packages), but interpretable discovery of the subgroups and the key factors driving the HTE is still an open challenge. To the best of our knowledge, `causalTree` (based on Causal Honest Tree [@athey2016]) is the unique R library proposing a methodology for interpretable HTE discovery and estimation via a decision rules using a single tree, but deespite its appealing features, it is also characterized by two main limitations. Firstly, single-tree-based subgroup identification is sensitive to variations in the training sample---e.g., if the data are slightly altered, a completely different set of discovered subgroups might be found (namely, the model variance is high) [@breiman1996heuristics; @hastie2009elements; @kuhn2013applied]. Secondly, it may fail to explore a vast number of potential subgroups (limited subgroup exploration)---e.g., the subgroups discovered are just the ones that can be represented by a single tree [kuhn2013applied; spanbauer2021nonparametric]. To illustrate, consider a scenario in which two distinct factors are independently contributing to the heterogeneity in treatment effects. In such cases, a single tree algorithm may detect only one of these factors, failing to identify the second. In instances where both factors are identified, they are detected sub-optimally as an interaction between the two variables rather than as distinct drivers of the treatment heterogeneity. To account for these shortcomings, we propose a novel `CRE` Causal Rule Ensemble (CRE) method that uses multiple trees rather than a single tree to uncover, in a data-driven way, heterogeneity patterns in the treatment effect via decision rules. `CRE` provides (i) an interpretable representation of the HTE, (ii) via an extensive exploration of complex heterogeneity patterns, while (iii) guaranteeing high stability in the discovery. We also develop a general two-stage estimation approach for the conditional causal effects of the discovered subgroups and provide theoretical guarantees.


# Algorithm

Causal Rule Ensemble relies on the Treatment Effect linear decomposition assumption, characterizing the Conditional Average Treatment Effect (CATE) by $M+1$ distinct contributions:
$$\tau(\boldsymbol{x}) = \mathbb{E}[\tau_i | X_i=\boldsymbol{x}] = \bar{\tau} + \sum_{m=1}^M \alpha_m \cdot r_m(\boldsymbol{x})$$
where $\bar{\tau}$ is the ATE, and for each $m$ in $\{1,..., M\}$, $r_m$ is an interpretable decision rule characterizing a specific subset of the covariate space, and $\alpha_m$ is the corresponding Additive Average Treatment Effect (AATE).
`CRE` procedure is divided into two steps, discovery and estimation, and each observation is used for only one of the two steps (honest splitting).
During the discovery step, `CRE` retrieves the $M$ decision rules characterizing the heterogeneity in the treatment effect. A set of candidate decision rules is extracted by an ensemble of trees trained by a _fit-the-fit_ procedure to model some Individual Treatment Effect (ITE) estimates, and among these, only a simple and robust subset of rules is selected for the linear decomposition by the Stability Selection algorithm via LASSO.
During the estimation step, `CRE` estimates the ATE and AATEs, by the normal equations to model some ITE estimates. Different ITE estimators can be selected for each step.


# Usage
`CRE` is available both on [CRAN](https://cran.r-project.org/web/packages/CRE/index.html) and [GitHub](https://github.com/NSAPH-Software/CRE) and can be installed and loaded into the R session
using:
```R
install.packages("CRE")
library("CRE")
```

`generate_cre_dataset()` is a flexible synthetic dataset generator, which can be used for simulations before applying `CRE` to real-world observational data sets. 
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

We propose here three examples of how to run the Causal Rule Esemble algorithm by the `CRE` package.

**Example 1.** Running Causal Rule Ensemble with default parameters described in @bargagli2023causal.
```R
cre_results <- cre(y, z, X)
```

**Example 2.** Running Causal Rule Ensemble with customized ITE estimator.
```R
ite_pred <- ... # personalized ite estimation
cre_results <- cre(y, z, X, ite = ite_pred)
```

**Example 3.** Running Causal Rule Ensemble with customized parameters.
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

The results are collected in an S3 object containing: the number of decision rules extracted at each step (`M`), the `data.frame` of the CATE decomposition estimates with corresponding uncertainty quantification (`CATE`), the list of selected parameters (`method_params` and `hyper_params`), and the predicted ITEs (`ite_pred`). 

`summarize()` and `print()` display a summary of these results, and `plot()` visualizes the CATE decomposition estimates in a range bar plot. Figure 1 reports an example of the proposed results visualization for Example 1. 

![Visualization of Causal Rule Ensemble HTE linear decomposition for Example 1. For each decision rule discovered, the corresponding AATE estimate with 95% confidence interval is reported in a range bar plot. The decision rules are ordered from the most vulnerable (high AATE) to the least, and the ATE is reported on top of the plot.](images/example.pdf)

Online documentation for the package can be found at [https://nsaph-software.github.io/CRE/](https://nsaph-software.github.io/CRE/).

# Acknowledgements

This work was partially funded by the following grants: NIH: R01ES026217, R01MD012769, R01ES028033, 1R01ES030616, 1R01AG066793, 1R01MD016054-01A1, R01AG066793-02S1 and R01ES028033-03S1; Sloan Foundation: G-2020-13946.

# References
