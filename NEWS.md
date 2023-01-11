# CRE (Developing)

## Changed
* `estimate_ite_poisson` function -> `estimate_ite_tpoisson`
* `max_dacay` hyper-parameter -> `t_decay`.
* `interpret_select_rules` function -> `interpret_rules`.
* `generate_causal_rules` function -> `discover_rules`.
* `discover_causal_rules` function ->`select_rules`.
* `offset_name` method parameter -> `offset`.
* Hyper and method parameters are no more required arguments for `cre`.
* `cre` object: added parameters and ite estimation.

## Added
* S-Learner (`slearner`) method for ITE estimation.
* T-Learner (`tlearner`) method for ITE estimation.
* X-Learner (`xlearner`) method for ITE estimation.
* Rules Selection description in `summary.cre`.
* `verbose` parameter in `summary.cre`.
* `ite`, additional `cre` input parameter to use personalized ite 
estimations.
* Default values for hyper parameters.
* Default values for method parameters.
* Simulation experiments for estimation (`estimation.R`).
* Simulation experiments for discovery (`discovery.R`).
* `extract_effect_modifiers` function (util for performance evaluation).
* `evaluate` function for discovery evaluation.
* `confounding` parameter in `generate_cre_dataset` to set confounding type.
* `ite_pred` and `model` in CRE results.
* `binary_covariates` parameter in `generate_cre_dataset` to set covariates 
domain.

## Removed
* `include_ps_inf` method-parameter.
* `include_ps_dis` method-parameter.
* `oreg` method for ITE estimation.
* `ipw` method for ITE estimation.
* `sipw` method for ITE estimation.
* ITE standard deviation estimation.
* `type_decay` hyper-parameter.
* Keep only `linreg` for CATE estimation (remove `cate_method` and 
`cate_SL_library` parameters).
* `method_params` and `hyper_params` additional parameters in `summary.cre`.
* ite standardization for Rules Generation.
* `random_state` parameter.
* `include_offset` method parameter.

## Solved
* Rules Generation Issue.



# CRE 0.1.1 (2022-10-18)

## Changed

* `binary` parameter in `generate_cre_dataset` -> `binary_outcome` .
* `filter_cate` hyper-parameter -> `t_pvalue`.
* `t_anom` hyper-parameter -> `t_ext`.
* `effect_modifier` hyper-parameter -> `intervention_vars`.
* `lasso_rules_filter` function -> `discover_causal_rules`.
* `split_data` function -> `honest_splitting`.
* `prune_rules` function -> ``filter_irrelevant_rules`.
* `discard_correlated_rules` function -> `filter_correlated_rules`.
* `discard_anomalous_rules` function -> `filter_extreme_rules`.

## Added
* Weighted LASSO for Causal Rules Discovery (by `penalty_rl` hyper-parameter).


# CRE 0.1.0 (2022-10-17)

## Changed

* Update examples and tests for all functions.
* `q` hyper-parameter -> `cutoff`.
* `pfer_val` hyper-parameter -> `pfer`.
* `select_causal_rules` function -> `lasso_rules_filter`.
* `t` hyper-parameter -> `t_anom`.
* Separate standardization, and remove filtering from `generate_rules_matrix` 
function.
* `summary.cre` function to describe results.
* `min_nodes` hyper-parameter -> `node_size` (`randomForest` convention).
* `cre` returns an S3 object.

## Added
* Examples and tests for all functions.
* `prune_rules` function to discard unpredictive rules.
* `discard_anomalous_rules` function to discard anomalous rules (see `t_corr` 
hyper-parameter.).
* `discard_correlated_rules` function to discard correlated rules (see `t_anom` 
hyper-parameter).
* `effect_modifiers` parameter in `generate_rules` function for covariates 
filtering.
* `generate_causal_rules` function.
* Helper function  with `SuperLearner` package for propensity score estimation 
in `estimate_ite_xyz`.
* Five methods for CATE estimation (`poisson`, `DRLearner`, `bart-baggr`, 
`cf-means`, `linreg`) in `estimate_cate` function.
* (`ps_method_dis`, `ps_method_inf`, `or_method_dis`, `or_method_inf`, 
`cate_SL_library`) method-parameters to complement `SuperLearner` package. 
* `cate_method` method-parameter to select CATE estimation method.
* `filter_cate` method-parameter for estimation filtering.
* `p` parameter (in `generate_cre_dataset` function) to set the number of 
covariates.
* `replace` parameter (in `generate_rules` function) to allow bootstrapping.
* `cre.print` generic function to print `cre` S3 object results.
* `cre.summary` generic functions to summarize `cre` S3 object Results.
* `check_input` function to isolate input checks.
* `estimate_ite_aipw` function for augmented inverse propensity weighting.
* `plot.cre` generic function to plot `cre` S3 object results.
* `test-cre_functional.R` to test the functionality of the package.
* `stability_selection` function for causal rules selection.

## Removed
* `estimate_ite_blp` function.
* `take1()` function.

## Solved
* Undesired 'All' Decision Rule Issue.
* No Causal Rule Selected Issue.


# CRE 0.0.1 (2021-10-20)

## Changed
* `estimate_cate` include two methods for estimating the CATE values.
* `cre` added initial checks for binary outcome and whether to include the 
propensity score in the ITE estimation.
* `estimate_ite_xyz` conduct propensity score estimation using helper function.

## Added
* Example for `generate_cre_dataset`.
* `set_logger` and `get_logger`.
* `check_input_data` function.
* `generate_cre_dataset` function to generate synthetic data for testing the 
package.
* `test-generate_cre_dataset` function test.
* `estimate_ps` function to estimate the propensity score.
* `estimate_ite_xbart` function to generate ITE estimates using accelerated 
BART.
* `estimate_ite_xbcf` function to generate ITE estimates using accelerated BCF.
* `analyze_sensitivity` function to conduct sensitivity analysis for unmeasured 
confounding.
* `cre` function to perform the entire Causal Rule Ensemble method.
* `estimate_cate` function to generate CATE estimates from the ITE 
estimates and select rules.
* `estimate_ite` function to generate ITE estimates using the user-specified 
method (calls the other `estimate_ite_xyz` functions).
* `estimate_ite_bart` function to generate ITE estimates using BART.
* `estimate_ite_bcf` function to generate ITE estimates using Bayesian Causal 
Forests.
* `estimate_ite_cf` function to generate ITE estimates using Causal Forests.
* `estimate_ite_ipw` function to generate ITE estimates using IPW.
* `estimate_ite_or` function to generate ITE estimates using Outcome Regression.
* `estimate_ite_sipw` function to generate ITE estimates using SIPW.
* `extract_rules` function to extract a list of causal rules from randomForest 
and GBM models.
* `generate_rules` function to generate causal rule models using 
randomForest and GBM methods.
* `generate_rules_matrix` function to convert a list of causal rules into a 
matrix.
* `select_causal_rules` function to apply penalized regression to causal rules.
to select only the most important ones.
* `split_data` function to split input data into discovery and inference 
subsamples.
* `take1` function to create a subsample of indices.

## Removed
* `seed` argument in `generate_cre_datase` function.
