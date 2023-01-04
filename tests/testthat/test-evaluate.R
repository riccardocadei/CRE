test_that("Evaluate Run Correctly", {

  cdr_pred <- c("x1>0.5 & x2<=0.5",
               "x5>0.5 & x6<=0.5",
               "x4<=0",
               "x5<=0.5 & x8<=0.5")
  cdr <- c("x1>0.5 & x2<=0.5",
           "x5>0.5 & x6<=0.5",
           "x4<=0",
           "x5<=0.5 & x7>0.5 & x8<=0.5")

# Correct outputs
  metrics <- evaluate(cdr, cdr_pred)
  expect_true(metrics$IoU == 0.6)
  expect_true(metrics$precision == 0.75)
  expect_true(metrics$recall == 0.75)
})
