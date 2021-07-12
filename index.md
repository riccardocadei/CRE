# CRE <a href="https://fasrc.github.io/CRE/"><img src="man/figures/png/CRE_logo.png" align="right" height="115" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/fasrc/CRE/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CRE/actions)
<!-- badges: end -->

# Overview

The CRE Package in R implements the Causal Rule Ensemble, a method for the discovery of subgroups susceptible to heterogeneous treatment effects. 

The CRE method splits the inputted data into discovery and inference subsamples. On the discovery subsample, it generates and filters causal rules that explain the heterogeneity in treatment effects. It then applies these rules to the inference subsample to estimate the Conditional Average Treatment Effect (CATE) for each subgroup. 

The final outputs are a list of select causal rules and CATE estimates. A sensitivity analysis for unmeasured confounding will also be implemented in a later version.
