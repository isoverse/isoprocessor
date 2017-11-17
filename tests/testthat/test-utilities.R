context("Utilities and other convenience functions")

test_that("text to numeric conversions work", {

  # extract single matching group
  expect_equal(isoprocessorCUB::str_extract_group(c("abc123def", "ABC456DEF"), "(\\d+)(\\w+)", 1),
            c("123", "456"))
  expect_equal(isoprocessorCUB::str_extract_group(c("abc123def", "ABC456DEF"), "(\\d+)(\\w+)", 2),
               c("def", "DEF"))

  # cleanup numeric
  expect_error(cleanup_numeric(), "x not supplied")
  expect_equal(cleanup_numeric(1:5), 1:5)
  expect_error(cleanup_numeric(data.frame(x=1:5)), "not a character vector")

  # test numbers
  numbers <- c("abc42", "abc42def", "abc4.2def", "abc-4.2def", "42def", "4.2")
  expect_equal(numbers %>% cleanup_numeric(), c(42, 42, 4.2, -4.2, 42, 4.2))
  expect_equal(numbers %>% cleanup_numeric(allow_negative = FALSE), c(42, 42, 4.2, 4.2, 42, 4.2))
  expect_equal(numbers %>% cleanup_numeric(trim_text_prefix = FALSE), c(NA, NA, NA, NA, 42, 4.2))
  expect_equal(numbers %>% cleanup_numeric(trim_text_suffix = FALSE), c(42, NA, NA, NA, NA, 4.2))
  expect_equal(numbers %>% cleanup_numeric(trim_text_prefix = FALSE, trim_text_suffix = FALSE), c(NA, NA, NA, NA, NA, 4.2))

})
