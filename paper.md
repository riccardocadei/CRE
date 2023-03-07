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
    affiliation: 1
  - name: Naeem Khoshnevis
    orcid: 0000-0003-4315-1426
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 1
  - name: Falco J. Bargagli Stoffi
    orcid: 0000-0002-6131-8165
    affiliation: 1
affiliations:
 - name: Department of Biostatistics, Harvard School of Public Health
   index: 1
date: 2 November 2022
bibliography: paper.bib
header-includes: 
 - \usepackage{algorithm}
 - \usepackage{bm}

---

# Summary

Data-driven discovery of heterogeneous treatment effects (HTE) via decision tree methods has been proposed for this task. Despite its high interpretability, the single-tree discovery of HTE tends to be highly unstable and to find an oversimplified representation of treatment heterogeneity. To accommodate these shortcomings, we propose Causal Rule Ensemble (CRE), a new method to discover heterogeneous subgroups through an ensemble-of-trees approach. CRE provides (i) an interpretable representation of the HTE, (ii) via an extensive exploration of complex heterogeneity patterns, while (iii) guaranteeing high stability in the discovery. The discovered subgroups are defined in terms of interpretable decision rules, and we develop a general two-stage approach for subgroup-specific conditional causal effects estimation, providing theoretical guarantees. 

# Statement of need

See [@Lee:2020].


# Algorithm

CRE Descriprion.

$$
\begin{algorithm}[H]
\caption{Algorithm name}
\end{algorithm}
$$

# Example

Generate Synthetic Data
```R
dataset <- generate_cre_dataset(n = 1000, 
                                rho = 0, 
                                n_rules = 2, 
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
summary(cre_results)
```

Example 2. Run CRE with customized ITE estimator.
```R
ite_pred <- ... # personalized ite estimation
cre_results <- cre(y, z, X, ite = ite_pred)
summary(cre_results)
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
summary(cre_results)
```

Visualize results
```R
plot(cre_results)
```

Add plot results description.

\begin{figure}
  \centering 
  \includegraphics{images/example.pdf}
  \caption{Plot results}
  \label{fig:example} 
\end{figure}

# Acknowledgements

We acknowledge contributions from ...

# References
