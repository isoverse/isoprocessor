context("Calibration")

test_that("test that standard addition works properly", {

  expect_error(iso_add_standards(), "no data table")
  expect_error(iso_add_standards(data_frame()), "no standards table")
  expect_error(iso_add_standards(data_frame(), data_frame()), "unknown column")

  # FIXME: elaborate on test cases

})

test_that("test that calibration prep works properly", {

  expect_error(iso_prepare_for_calibration(), "no data table")
  expect_error(iso_prepare_for_calibration(data_frame(all_data = NA)), "column already exists")
  expect_error(iso_prepare_for_calibration(data_frame(), group_by = bla), "unknown column")

  # simple nesting
  expect_message(out <- iso_prepare_for_calibration(data_frame()), "nesting the entire dataset")
  expect_equal(names(out), "all_data")
  expect_equal(out %>% unnest(all_data), data_frame())

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
               list(calib_name = "", model_name = "calib", model_enough_data = "calib_ok", model_params = "calib_params", residual = "resid"))
  expect_equal(vars <- get_calibration_vars("x"),
               list(calib_name = "'x' ", model_name = "x_calib", model_enough_data = "x_calib_ok", model_params = "x_calib_params", residual = "x_resid"))

  # check calibration cols function
  expect_error(check_calibration_cols(42), "not a data frame")
  expect_error(check_calibration_cols(mtcars, vars[c("model_enough_data", "model_params")]),
               "unknown column.*make sure to run.*use the same.*calibration")
  expect_silent(check_calibration_cols(data_frame(x_calib_ok = TRUE, x_calib_params = list()),
                                       vars[c("model_enough_data", "model_params")]))
})

test_that("test default behavior of calibrations", {

  expect_error(iso_calibrate_delta(), "no .* supplied")
  expect_error(iso_calibrate_delta(iris), "no .* supplied")

  # FIXME: elaborate on test cases, test all calibration functions

})

test_that("test that problematic calibrations can be removed properly", {

  expect_error(iso_get_problematic_calibrations(), "no data table")
  expect_error(iso_get_problematic_calibrations(42), "not a data frame")
  expect_error(iso_get_problematic_calibrations(data_frame()), "unknown column")
  expect_message(iso_get_problematic_calibrations(data_frame(calib_ok = TRUE)), "no problematic calibrations")
  expect_message(out <- iso_get_problematic_calibrations(data_frame(name = c("x", "y"), calib_ok = c(TRUE, FALSE)), select = name), "fetching problematic.*1 of 2")
  expect_equal(out, data_frame(name = "y"))

  expect_message(out <- iso_remove_problematic_calibrations(data_frame(name = c("x", "y"), calib_ok = c(TRUE, FALSE))), "removing problematic.*1 of 2")
  expect_equal(out, data_frame(name = "x"))

})
