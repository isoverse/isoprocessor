# file info ====

context("Peak mapping")

test_that("metadata addition works", {

  expect_error(iso_add_metadata(), "no data table")
  expect_error(iso_add_metadata(data_frame()), "no metadata")

  #iso_add_metadata(data_frame(a=1, b="test"), data_frame(a = 1:5, b="bla"), match_by = c(a, b))

})

test_that("testing that peak mapping works", {

  expect_error(iso_map_peaks(), "no data table")
  expect_error(iso_map_peaks(data_frame()), "no peak maps")

  # data table
  my_dt <- data_frame(
    file_id = rep(c("a", "b", "c"), each = 3),
    map_id = rep(c("a_map", "bc_map", "bc_map"), each = 3),
    #              X,   Y,  NA,   X, Y+Z,   Z,  X,   X,    Z
    rt       = c(1.1, 4.0, 5.8, 1.5, 5.0, 5.8, 1.5, 2.0, 5.5),
    rt_start = c(0.9, 3.8, 5.6, 1.0, 4.4, 5.4, 1.1, 1.6, 5.5),
    rt_end   = c(1.3, 4.2, 6.0, 2.0, 5.6, 6.2, 1.9, 2.4, 5.5)
  )

  # maps
  my_peak_maps <- data_frame(
    compound = c("X", "Y", "Z"),
    other_info = c(TRUE, FALSE, FALSE),
    `rt:a_map` = c(1.1, 4.2, NA),
    `rt:bc_map` = c(1.8, 4.4, 5.5),
    `rt:d_map` = c(1, 2, 3)
  )

  # make sure get an error if peak maps are missing
  expect_error(iso_map_peaks(mutate(my_dt, map_id = ifelse(row_number() == 1, "missing_pm", map_id)), my_peak_maps),
               "maps .* do not exist .* 'missing_pm'.* Available .* 'a_map', 'bc_map', 'd_map'")

  # check that it all assembles properly
  expect_equal(iso_map_peaks(my_dt, my_peak_maps),
               data_frame(
                 file_id = c("a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c"),
                 map_id = c("a_map", "a_map", "a_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map"),
                 rt = c(1.1, 4, 5.8, 1.5, 5, 5, 5.8, 1.5, 2, 4.4, 5.5),
                 rt_start = c(0.9, 3.8, 5.6, 1, 4.4, 4.4, 5.4, 1.1, 1.6, NA, 5.5),
                 rt_end = c(1.3, 4.2, 6, 2, 5.6, 5.6, 6.2, 1.9, 2.4, NA, 5.5),
                 compound = c("X", "Y", NA, "X", "Y", "Z", "Z", "X", "X", "Y", "Z"),
                 other_info = c(TRUE, FALSE, NA, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
                 n_matches = c(1, 1, 0, 1, 2, 2, 1, 1, 1, 0, 1),
                 n_overlapping = c(1, 1, 0, 1, 1, 2, 2, 2, 2, 0, 1),
                 is_identified = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                 is_missing = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                 is_ambiguous = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
               ))

  expect_message(iso_map_peaks(my_dt, my_peak_maps), "8 peaks in 3 files .* 1 could not be assigned.*1 were missing")
})
