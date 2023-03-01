test_that("Effect Modifiers Extracted Correctly", {

  X_names <- paste("x", 1:10, sep = "")
  causal_decision_rules_int <- c("x1>0.5 & x2<=0.5",
                                 "x5>0.5 & x6<=0.5",
                                 "x4<=0",
                                 "x5<=0.5 & x7>0.5 & x8<=0.5")
  effect_modifiers <- c("x1", "x2", "x4", "x5", "x6", "x7", "x8")

  # Correct outputs
  expect_true(all(effect_modifiers ==
                  extract_effect_modifiers(causal_decision_rules_int,
                                                X_names)))
})
