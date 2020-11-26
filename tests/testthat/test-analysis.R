context("Analysis functions")

# outlier identification ======

test_that("test outlier identification", {

  expect_error(iso_identify_outliers(), "no data frame")
  expect_error(iso_identify_outliers(tibble()), "no y-variable defined")
  expect_error(iso_identify_outliers(tibble(), y = y), "no cutoff provided")
  expect_error(iso_identify_outliers(tibble(), y = y, plus_minus_value = "2"), "provide.*single numeric")
  expect_error(iso_identify_outliers(tibble(), y = y, plus_minus_value = 1:2), "provide.*single numeric")
  expect_error(iso_identify_outliers(tibble(), y = y, plus_minus_sd = "2"), "provide.*single numeric")
  expect_error(iso_identify_outliers(tibble(), y = y, plus_minus_sd = 1:2), "provide.*single numeric")

  # check values
  df <- tibble(x = 1:10, grp = rep(c("x", "y"), each = 5))
  expect_equal(
    df %>% iso_identify_outliers(y = x, plus_minus_sd = 1) %>% dplyr::pull(is_outlier),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    df %>% iso_identify_outliers(y = x, plus_minus_sd = 1, group_by = grp) %>% dplyr::pull(is_outlier),
    c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    df %>% iso_identify_outliers(y = x, plus_minus_value = 1) %>% dplyr::pull(is_outlier),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(
    df %>% iso_identify_outliers(y = x, plus_minus_value = 1, group_by = grp) %>% dplyr::pull(is_outlier),
    c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )


})
