context("Standard and non-standard evaluation")

test_that("Getting new column names work correctly", {

  expect_error(get_new_column_names(a = quo(x^2)), "not .* valid column name")
  expect_error(get_new_column_names(a = quo(x^2), b=quo(y^2)), "not .* valid column name")
  expect_equal(get_new_column_names(a = quo(x)), list(a = "x"))
  expect_equal(get_new_column_names(a = quo(default(x))), list(a = "x"))
  expect_equal(get_new_column_names(a = quo("x")), list(a = "x"))
  expect_equal(get_new_column_names(a = quo(x), b = quo(default(y)), c = quo("z")), list(a = "x", b = "y", c = "z"))

})

test_that("Column name to quosure conversion works correctly", {

  expect_error(cols_to_quos(list(a="test", b=5)), "can only convert character")
  expect_equal(cols_to_quos(letters[1:3]), quos(a, b, c))
  expect_equal(cols_to_quos(letters[1]), quo(a))
  expect_equal(cols_to_quos(letters[1], always_as_list = TRUE), quos(a))
  expect_equal(cols_to_quos(list(x="a", y="b", z="c")), quos(a, b, c))
  expect_equal(cols_to_quos(list(x="a", y="b", z="c"), keep_names = TRUE), quos(x = a, y = b, z = c))
  expect_equal(cols_to_quos(list(x = "a", y = c("b", "c"))), quos(a, b, c))
  expect_equal(cols_to_quos(list(x = "a", y = c("b", "c")), keep_names = TRUE), quos(x = a, y1 = b, y2 = c))
  expect_equal(cols_to_quos(letters[1:3], negate = TRUE), quos(-a, -b, -c))
})

test_that("Column name to symbol list works correctly", {

  expect_error(cols_to_symbol_list(list(a="test", b=5)), "can only convert character")
  expect_equal(cols_to_symbol_list(letters[1:3]), list(as.name("a"), as.name("b"), as.name("c")))
  expect_equal(cols_to_symbol_list(letters[1]), list(as.name("a")))
  expect_equal(cols_to_symbol_list(list(x="a", y="b", z="c")), list(as.name("a"), as.name("b"), as.name("c")))
  expect_equal(cols_to_symbol_list(list(x="a", y="b", z="c"), keep_names = TRUE), list(x = as.name("a"), y = as.name("b"), z = as.name("c")))
})
