# CRE <a href="https://nsaph-software.github.io/CRE/"><img src="man/figures/png/CRE_logo.png" align="right" width="115" /></a>

<!-- badges: start -->
[![](http://www.r-pkg.org/badges/version-last-release/CRE)](https://CRAN.R-project.org/package=CRE)
[![R-CMD-check](https://github.com/fasrc/CRE/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CRE/actions)
[![codecov](https://codecov.io/gh/NSAPH-Software/CRE/branch/develop/graph/badge.svg?token=UMSVOYRKGA)](https://codecov.io/gh/NSAPH-Software/CRE)]
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/CRE)](http://www.r-pkg.org/pkg/cre)
<!-- badges: end -->

# Overview

Identifying subgroups of a study population where a treatment or exposure has a notably larger or smaller effect on an outcome compared to the population average is crucial in social and health sciences. While estimating the conditional average treatment effect (CATE) given a pre-specified set of covariates is a common approach, it only allows for estimating causal effects on subgroups that have been specified a priori by the researchers. This package implements the recently developed Causal Rule Ensemble (CRE) algorithm, a flexible and precise method for denovo subgroup discovery. 


# Package Details

The cre() function executes the entire CRE algorithm by utilizing a data set and input parameters the user provides. The cre() function internally employs several functions to conduct subsample analysis for discovery and inference. To estimate Individual Treatment Effects, users can choose from various methodologies that vary in terms of speed and accuracy. The CRE method divides the study data into discovery and inference subsamples, generating and filtering causal rules on the discovery subsample to explain the heterogeneity in treatment effects. These rules are then applied to the inference subsample to estimate each subgroup's Conditional Average Treatment Effect (CATE). The output of cre() includes a list of selected causal rules identifying subgroups with higher heterogeneity in the effects, as well as a matrix of estimated CATEs for each rule, both of which are easily accessible and interpretable.


# Applications

The CRE package can be used in diverse theoretical and practical applications as long as working with binary exposure or treatment level. For example, it can be used in education to determine which teaching methods are most effective for specific student subgroups. In marketing, it can identify the most effective strategies for different consumer subgroups. In healthcare, it can be used to identify patient subgroups that benefit the most from a particular treatment or intervention. It can also be used in studies for personalized medicine where each subgroup of patients responds differently to the provided medicine. In public health, the CRE method was applied to discover subgroups that are more vulnerable (or resilient) to the causal effects of long-term exposure to air pollution on mortality.
