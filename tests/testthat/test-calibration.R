context("Calibration")

test_that("test that standard addition works properly", {

  expect_error(iso_add_standards(), "no data table")
  expect_error(iso_add_standards(tibble()), "no standards table")
  expect_error(iso_add_standards(tibble(), tibble()), "unknown column")

  # FIXME: elaborate on test cases

})

test_that("test that calibration prep works properly", {

  expect_error(iso_prepare_for_calibration(), "no data table")
  expect_error(iso_prepare_for_calibration(tibble(all_data = NA)), "column already exists")
  expect_error(iso_prepare_for_calibration(tibble(), group_by = bla), "unknown column")

  # simple nesting
  expect_message(out <- iso_prepare_for_calibration(tibble()), "nesting the entire dataset")
  expect_equal(names(out), "all_data")
  expect_equal(out %>% unnest(all_data), tibble())

  # grouped nesting
  expect_message(out <- iso_prepare_for_calibration(ggplot2::mpg, group_by = cyl), "grouping.*cyl")
  expect_equal(names(out), c("cyl", "all_data"))
  expect_equal(out %>% unnest(all_data), ggplot2::mpg)

  expect_message(out <- iso_prepare_for_calibration(ggplot2::mpg, group_by = c(cyl, drv)), "grouping.*cyl.*drv")
  expect_equal(names(out), c("cyl", "drv", "all_data"))
  expect_equal(out %>% unnest(all_data), ggplot2::mpg)

})

test_that("test that calibration variables work properly", {
  # get calibration vars function
  expect_error(get_calibration_vars(), "missing")
  expect_equal(get_calibration_vars(""),
               list(calib_name = "", model_name = "calib", model_enough_data = "calib_ok", model_params = "calib_params", residual = "resid", in_range = "in_range"))
  expect_equal(vars <- get_calibration_vars("x"),
               list(calib_name = "'x' ", model_name = "x_calib", model_enough_data = "x_calib_ok", model_params = "x_calib_params", residual = "x_resid", in_range = "x_in_range"))

  # check calibration cols function
  expect_error(check_calibration_cols(42), "not a data frame")
  expect_error(check_calibration_cols(mtcars, vars[c("model_enough_data", "model_params")]),
               "unknown column.*make sure to run.*use the same.*calibration")
  expect_silent(check_calibration_cols(tibble(x_calib_ok = TRUE, x_calib_params = list()),
                                       vars[c("model_enough_data", "model_params")]))
})

test_that("test default behavior of calibrations", {

  # FIXME: implement test casesto test calibration functions

})

test_that("test that problematic calibrations can be removed properly", {

  expect_error(iso_get_problematic_calibrations(), "no data table")
  expect_error(iso_get_problematic_calibrations(42), "not a data frame")
  expect_error(iso_get_problematic_calibrations(tibble()), "unknown column")
  expect_message(iso_get_problematic_calibrations(tibble(calib_ok = TRUE)), "no problematic calibrations")
  expect_message(out <- iso_get_problematic_calibrations(tibble(name = c("x", "y"), calib_ok = c(TRUE, FALSE)), select = name), "fetching problematic.*1 of 2")
  expect_equal(out, tibble(name = "y"))

  expect_message(out <- iso_remove_problematic_calibrations(tibble(name = c("x", "y"), calib_ok = c(TRUE, FALSE))), "removing problematic.*1 of 2")
  expect_equal(out, tibble(name = "x"))

})
