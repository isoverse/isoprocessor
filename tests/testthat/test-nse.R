context("Standard and non-standard evaluation")

test_that("Getting column name finding works correctly", {

  df <- as_data_frame(mtcars)

  # basic errors
  expect_error(get_column_names(), "no data frame supplied")
  expect_error(get_column_names(5), "not a data frame")
  expect_error(get_column_names(df, a = quo(x)), "invalid column")

  # single column per identifier
  expect_equal(get_column_names(df, a = quo(mpg), b = quo(wt)), list(a="mpg", b="wt"))
  expect_equal(get_column_names(df, a = quo("mpg"), b = quo("wt")), list(a="mpg", b="wt"))

  # no columns per identifier
  expect_error(get_column_names(df, a = quo(mpg), n_reqs = list(a = "wrong")), "unknown number requirement")
  expect_error(get_column_names(df, a = quo(NULL)), "not .* the correct number of columns")
  expect_error(get_column_names(df, a = quo(NULL), n_reqs = list(a = "+")), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(NULL), n_reqs = list(a = "*")), list(a = character(0)))
  expect_equal(get_column_names(df, a = quo(NULL), n_reqs = list(a = "?")), list(a = character(0)))
  expect_equal(get_column_names(df, a = quo(mpg), n_reqs = list(a = "?")), list(a = "mpg"))
  expect_error(get_column_names(df, a = quo(NULL), b = quo(NULL), n_reqs = list(a = "+")), "not .* the correct number of columns")

  # multiple columns in identifier
  expect_error(get_column_names(df, a = quo(c(mpg, wt))), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), n_reqs = list(a = "*")), list(a = c("mpg", "wt")))
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), n_reqs = list(a = "+")), list(a = c("mpg", "wt")))
  expect_error(get_column_names(df, a = quo(c(mpg, wt)), b = quo(c(mpg, wt)), n_reqs = list(a = "+")), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), b = quo(starts_with("d")), n_reqs = list(a = "+", b="*")),
               list(a = c("mpg", "wt"), b = c("disp", "drat")))

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
