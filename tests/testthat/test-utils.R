test_that("utils work as expected", {

  set_options("my_first_key", 10)

  expect_equal(get_options("my_first_key"),10)

  set_options("my_second_key", "random")

  lop <- list_options()
  expect_true("my_first_key" %in% lop)
  expect_true("my_second_key" %in% lop)
  expect_false("my_ff_key" %in% lop)

})
