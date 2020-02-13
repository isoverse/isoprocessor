context("Standard and non-standard evaluation")

test_that("Testing expression checks", {

  df <- as_data_frame(mtcars) %>% tibble::rownames_to_column()

  # basic errors
  expect_error(check_expressions(), "no data frame supplied")
  expect_error(check_expressions(5), "not a data frame")

  # error messages
  expect_error(check_expressions(df, quo(x == 5)), "not a valid expression")
  expect_error(check_expressions(df, quo(x == 5), quo(y == 42)), "invalid expressions")

  # test evaluations
  expect_equal(
    check_expressions(df, quo(mpg > 20), quo(ifelse(grepl("Merc", rowname), "test", rowname)), z = NULL),
    df
  )

})


test_that("Getting new column names work correctly", {

  expect_error(get_new_column_names(a = quo(x^2)), "not a valid column name")
  expect_error(get_new_column_names(a = quo(x^2), b=quo(y^2)), "not a valid column name")
  expect_equal(get_new_column_names(a = quo(x)), list(a = "x"))
  expect_equal(get_new_column_names(a = quo(`x^2`)), list(a = "x^2"))
  expect_equal(get_new_column_names(a = quo(default(x))), list(a = "x"))
  expect_equal(get_new_column_names(a = quo("x")), list(a = "x"))
  expect_equal(get_new_column_names(a = quo(x), b = quo(default(y)), c = quo("z"), d = quo(`x^2`)), list(a = "x", b = "y", c = "z", d = "x^2"))

})
