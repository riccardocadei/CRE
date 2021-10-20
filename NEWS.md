## CRE 0.0.1 (2021-10-20)

### Changed
- `estimate_cate` include two methods for estimating the CATE values
- `cre` added initial checks for binary outcome and whether to include the propensity score in the ITE estimation
- `estimate_ite_xyz` conduct propensity score estimation using helper function
- Removed `seed` as an input from `generate_cre_dataset` function.

### Added
- `set_logger` and `get_logger`
- `check_input_data` function
- example to `generate_cre_dataset`
- `generate_cre_dataset` function to generate synthetic data for testing the package
- `test-generate_cre_dataset` function test
- `estimate_ps` function to estimate the propensity score
- `estimate_ite_xbart` function to generate ITE estimates using accelerated BART
- `estimate_ite_xbcf` function to generate ITE estimates using accelerated BCF
- `analyze_sensitivity` function to conduct sensitivity analysis for unmeasured confounding
- `cre` function to perform the entire Causal Rule Ensemble method
- `estimate_cate` function to generate CATE estimates from the ITE estimates and select rules
- `estimate_ite` function to generate ITE estimates using the user-specified method (calls the other `estimate_ite_xyz` functions)
- `estimate_ite_bart` function to generate ITE estimates using BART
- `estimate_ite_bcf` function to generate ITE estimates using Bayesian Causal Forests
- `estimate_ite_cf` function to generate ITE estimates using Causal Forests
- `estimate_ite_ipw` function to generate ITE estimates using IPW
- `estimate_ite_or` function to generate ITE estimates using Outcome Regression
- `estimate_ite_sipw` function to generate ITE estimates using SIPW
- `extract_rules` function to extract a list of causal rules from randomForest and GBM models
- `generate_rules` function to generate causal rule models using randomForest and GBM methods
- `generate_rules_matrix` function to convert a list of causal rules into a matrix
- `select_causal_rules` function to apply penalized regression to causal rules to select only the most important ones
- `split_data` function to split input data into discovery and inference subsamples
- `take1` function to create a subsample of indices
