context("Metadata")

test_that("metadata addition works", {

  expect_error(iso_add_metadata(), "no data table")
  expect_error(iso_add_metadata(data_frame()), "no metadata")

  # FIXME: continue with proper testing of this function !!!

  # get missing metadta
  expect_error(iso_get_missing_metadata(), "no data table")
  expect_error(iso_get_missing_metadata(data_frame()), "has_metadata.*unknown column")
  test_data <- data_frame(id = c("A", "B"), has_metadata = c(TRUE, FALSE))
  expect_error(iso_get_missing_metadata(test_data, select = c()), "at least one")
  expect_equal(iso_get_missing_metadata(test_data, select = c(my_id = id)),
               data_frame(my_id = "B"))
  expect_message(iso_get_missing_metadata(test_data), "fetching.*missing metadata")
  expect_silent(iso_get_missing_metadata(test_data, quiet = TRUE))

  # remove missing metadata
  expect_error(iso_remove_missing_metadata(), "no data table")
  expect_error(iso_remove_missing_metadata(data_frame()), "has_metadata.*unknown column")
  expect_equal(iso_remove_missing_metadata(test_data), data_frame(id = "A"))
  expect_equal(iso_remove_missing_metadata(test_data, remove_has_metadata_column = FALSE), data_frame(id = "A", has_metadata = TRUE))

})
