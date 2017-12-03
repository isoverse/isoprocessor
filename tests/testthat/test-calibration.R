context("Calibration")

test_that("test default behavior of calibrations", {

  expect_error(iso_calibrate_delta(), "no .* supplied")
  expect_error(iso_calibrate_delta(iris), "no .* supplied")

  # FIXME: elaborate on test cases, test all calibration functions

})
