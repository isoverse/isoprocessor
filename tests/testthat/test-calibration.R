context("Calibration")

test_that("test default behavior of calibrations", {

  expect_error(get_standards_calibrations(), "no .* supplied")
  expect_error(get_standards_calibrations(iris), "no .* supplied")

})
