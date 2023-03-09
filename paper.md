---
title: 'CRE: A R package for interpretable inference of heterogeneous treatment effects'
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
  - name: Kwonsang Lee
    orcid: 0000-0002-5823-4331
    affiliation: "1"
  - name: Daniela Maria Garcia
    orcid: 0000-0003-3226-3561
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
 - \usepackage{amsmath}

---

# Summary

In health and social sciences, it is critically important to identify subgroups of the study population where a treatment has notable heterogeneity in the causal effects with respect to the average treatment effect. The bulk of heterogeneous treatment effect (HTE) literature focuses on two major tasks [@dwivedi2020stable]: (i) estimating HTEs by examining the conditional average treatment effect (CATE); (ii) discovering subgroups of a population characterized by HTE. 
Several methodologies have already been proposed for both the tasks, but providing interpretability in the results is still an open challenge. Interpretability is a non-mathematical concept, yet it is often defined as the degree to which a human can understand the cause of a decision [@kim2016examples], [@miller2018explanation], [@lakkaraju2016interpretable], [@wang2022causal]. Honest Causal Tree [@athey2016] fits perfectly this definition, but despite its high interpretability, it tends to be highly unstable and to find an oversimplified representation of treatment heterogeneity. To accommodate these shortcomings, [@Lee:2020] proposed Causal Rule Ensemble, a new method for HTE characterization in terms of decision rules, via an extensive exploration of heterogeneity patterns by an ensemble-of-trees approach, enforcing high stability in the discovery. `CRE` is a R Package providing a flexible implementation of Causal Rule Ensemble algorithm.


# Algorithm

Causal Rule Ensemble
```latex
\begin{algorithm}[H]
\footnotesize
\caption{Causal Rule Ensemble (CRE)}
\label{alg:cre}
\vspace{0.15cm}
{\bf Inputs:} covariates matrix $X$, (binary) treatment vector $\bm{z}$, and observed response vector $\bm{y}$.\\
{\bf Outputs:} (i) a set of interpretable decision rules $\mathcal{\hat{R}}=\{\hat{r}_m\}_{m=1}^M$, 

\hspace{1.48cm} (ii) ATE $\hat{\bar{\tau}}$ and AATEs $\hat{\bm{\alpha}}$ estimates and confidence intervals,

\hspace{1.44cm} (iii) a sensitivity analysis on ATE $\hat{\bar{\tau}}$ and AATEs $\hat{\bm{\alpha}}$ estimates.

\vspace{0.05cm}
{\bf Procedure:}
\begin{algorithmic}%[1]
    \State $(X^{d},\bm{z}^{d},\bm{y}^{d}$), ($X^{e},\bm{z}^{e},\bm{y}^{e}) \gets \texttt{HonestSplitting}(X,\bm{z},\bm{y}) $
    
    % HCDR Discovery
    \vspace{0.02cm}
    \noindent
    {\bf i. Discovery}
    \begin{algorithmic}
        \State $\bm{\hat{\tau}}^{d} \gets \texttt{EstimateITE}(X^{d},\bm{z}^{d},\bm{y}^{d})$ \Comment{e.g. AIPW, CF, BCF, BART, S/T/X-Learner}
        \State $ \mathcal{\hat{R}'} \gets \texttt{GenerateRules}(X^{d},\bm{\hat{\tau}}^{d}) $
        \Comment{i.e., tree-ensemble method} 
        \State $ \mathcal{\hat{R}} \gets \texttt{RulesSelection}(\mathcal{\hat{R}'},X^{d},\bm{\hat{\tau}}^{d})$ \Comment{Stability Selection}
    \end{algorithmic}
    
    % CATE Estimation
    \vspace{0.02cm}
    \noindent
    {\bf ii. Estimation}
    \begin{algorithmic}
        \State $\bm{\hat{\tau}}^{e} \gets \texttt{EstimateITE}(X^{e},\bm{z}^{e},\bm{y}^{e})$ \Comment{e.g. AIPW, CF, BCF, BART, S/T/X-Learner}
        \State $\hat{\bm{\alpha}} \gets \texttt{EstimateAATE}(\mathcal{\hat{R}},X^{e}, \bm{\hat{\tau}}^{e})$ \Comment{Linear Decomposition}
        %\State $\text{C.I.}(\hat{\bm{\alpha}}) \gets \texttt{ConfidenceInterval}(\mathcal{\hat{R}},X^{e}, \bm{\hat{\tau}}^{e})$
    \end{algorithmic}
\end{algorithmic}
\end{algorithm}
```

See [@Lee:2020].

$$\tau(\boldsymbol{x}) = \mathbb{E}[\tau_i | X_i=\boldsymbol{x}] = \bar{\tau} + \sum_{m=1}^M \alpha_m \cdot r_m(\boldsymbol{x})$$

where $\alpha_m$ and $r_m$.

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
