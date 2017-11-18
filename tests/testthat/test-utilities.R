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

test_that("nesting and unnesting functions work properly", {

  # testing the nest data function
  test_df <- dplyr::data_frame(x=1:5, z = "text", y =c("a", "a", "b", "a", "b"))
  expect_error(nest_data(), "no data table supplied")
  expect_error(nest_data(test_df, DNE), "refers to invalid column")
  expect_equal(nest_data(test_df) %>% names(), c("nested_data"))
  expect_equal(nest_data(test_df, y) %>% names(), c("y", "nested_data"))
  expect_equal(nest_data(test_df, y, my_data) %>% names(), c("y", "my_data"))
  expect_equal(nest_data(test_df, c(y,z)) %>% names(), c("z", "y", "nested_data"))
  expect_equal(nest_data(test_df, c("y","z")) %>% names(), c("z", "y", "nested_data"))
  expect_equal(nest_data(test_df, -x) %>% names(), c("z", "y", "nested_data"))
  expect_equal(nest_data(test_df, starts_with("y")) %>% names(), c("y", "nested_data"))

  # testing unnest_select_data
  nested_df <- nest_data(test_df, y)
  expect_error(unnest_select_data(), "no data table supplied")
  expect_error(unnest_select_data(nested_df, nested = my_nested), "refers to invalid column")
  expect_error(unnest_select_data(nested_df, DNE), "refers to invalid column")
  expect_equal(unnest_select_data(nested_df) %>% names(), nested_df %>% names())
  expect_equal(unnest_select_data(nested_df, z) %>% names(), c("y", "z", "nested_data"))
  expect_equal(unnest_select_data(nested_df, "z") %>% names(), c("y", "z", "nested_data"))
  expect_equal(unnest_select_data(nested_df, x) %>% names(), c("y", "x", "nested_data"))
  expect_equal(unnest_select_data(nested_df, everything()) %>% names(), c("y", "x", "z"))

  # introduce more nested column
  mutated_nested_df <- mutate(nested_df, test_data = purrr::map(nested_data, ~dplyr::rename(.x, x2=x, z2=z)))
  expect_equal(unnest_select_data(mutated_nested_df, z) %>% names(), c("y", "z", "nested_data", "test_data"))
  expect_error(unnest_select_data(nested_df, z, nested = test_data), "refers to invalid column")
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data) %>% names(), c("y", "z2", "test_data", "nested_data"))

})
