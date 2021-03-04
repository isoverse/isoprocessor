context("Export")

# standard addition =====

test_that("test that calibration export works properly", {

  # error checks
  expect_error(iso_export_calibration_to_excel(), "must be a data frame")
  expect_error(iso_export_calibration_to_excel(42), "must be a data frame")
  expect_error(iso_export_calibration_to_excel(tibble(all_data = 1)), "not a list")
  expect_error(iso_export_calibration_to_excel(tibble(all_data = list())), "no.*calibrations")

  expect_error(iso_export_data_to_excel(), "filepath.*must be set")
  expect_error(iso_export_data_to_excel(filepath = "x"), "no data sets")
  expect_error(iso_export_data_to_excel(42, filepath = "x"), "must be.*data frames")

  # FIXME: continue implementing tests

})
