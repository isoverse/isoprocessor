context("Standard and non-standard evaluation")

test_that("Getting new column names work correctly", {

  expect_error(get_new_column_names(a = quo(x^2)), "not a valid column name")
  expect_error(get_new_column_names(a = quo(x^2), b=quo(y^2)), "not a valid column name")
  expect_equal(get_new_column_names(a = quo(x)), list(a = "x"))
  expect_equal(get_new_column_names(a = quo(`x^2`)), list(a = "x^2"))
  expect_equal(get_new_column_names(a = quo(default(x))), list(a = "x"))
  expect_equal(get_new_column_names(a = quo("x")), list(a = "x"))
  expect_equal(get_new_column_names(a = quo(x), b = quo(default(y)), c = quo("z"), d = quo(`x^2`)), list(a = "x", b = "y", c = "z", d = "x^2"))

})
