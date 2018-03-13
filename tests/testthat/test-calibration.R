context("Calibration")

test_that("test that standard addition works properly", {

  expect_error(iso_add_standards(), "no data table")
  expect_error(iso_add_standards(data_frame()), "no standards table")
  expect_error(iso_add_standards(data_frame(), data_frame()), "unknown column")

  # FIXME: elaborate on test cases

})

test_that("test default behavior of calibrations", {

  expect_error(iso_calibrate_delta(), "no .* supplied")
  expect_error(iso_calibrate_delta(iris), "no .* supplied")

  # FIXME: elaborate on test cases, test all calibration functions

})
