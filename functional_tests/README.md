# Causal Rule Ensemble - Functional Tests

## Tests

Test Causal Rule Example on a syntethic dataset.

### Causal Rule Ensemble
```r
`CRE/functional_tests/example_cre.R`
```

## Simulations

Reproduce simulation experiments in Section 4 in @bargagli2023causal, evaluating Causal Rule Ensemble Discovery and Estimation performances, comparing with different benchmarks. 

### Discovery 
Evaluate performance of Causal Rule Ensemble algorithm (varying the pseudo-outcome estimator) in rules and effect modifier discovery.
```r
`CRE/functional_tests/discovery.R`
```

### Estimation 
Evaluate performance of Causal Rule Ensemble algorithm (varying the pseudo-outcome estimator) in treatment effect estimation and comparing it with the corresponding stand-alone ITE estimators.
```r
`CRE/functional_tests/estimation.R`
```
