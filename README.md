| Resource    |  Github Actions      |  Code Coverage  |
| ----------  | -------------------- | --------------- |
| Platforms   | Windows, macOS, Linux|    codecov      |
| R CMD check | [![R-CMD-check](https://github.com/nsaph-software/CRE/workflows/R-CMD-check/badge.svg)](https://github.com/nsaph-software/CRE/actions) | [![codecov](https://codecov.io/gh/NSAPH-Software/CRE/branch/develop/graph/badge.svg?token=UMSVOYRKGA)](https://codecov.io/gh/NSAPH-Software/CRE)|


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

TBD

## References

Lee, K., Bargagli-Stoffi, F. J., & Dominici, F. (2020). Causal rule ensemble:
Interpretable inference of heterogeneous treatment effects.  arXiv preprint arXiv:2009.09036.
