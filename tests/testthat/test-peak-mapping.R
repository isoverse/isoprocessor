# peak Mapping ====

context("Peak mapping")

test_that("testing that peak mapping works", {

  expect_error(iso_map_peaks(), "no data table")
  expect_error(iso_map_peaks(tibble()), "no peak map*")

  # data table
  my_dt <- tibble(
    file_id = rep(c("a", "b", "c"), each = 3),
    map_id = rep(c("a_map", "bc_map", "bc_map"), each = 3),
    #            X+Y,   Z,  NA,   X, Y+Z,   Z,   X,   X,   Z
    rt       = c(1.1, 4.0, 5.8, 1.5, 5.0, 5.8, 1.5, 2.0, 5.5),
    rt_start = c(0.9, 3.8, 5.6, 1.0, 4.4, 5.4, 1.1, 1.6, 5.5),
    rt_end   = c(1.3, 4.2, 6.0, 2.0, 5.6, 6.2, 1.9, 2.4, 5.5)
  )

  # maps
  my_peak_maps <- tibble(
    compound = c("X", "Y", "Z"),
    other_info = c(TRUE, FALSE, FALSE),
    `rt:a_map` = c(1.1, 1.3, 4.2),
    `rt:bc_map` = c(1.8, 4.4, 5.5),
    `rt:d_map` = c(1, 2, 3)
  )

  # make sure get an error if peak maps are missing
  expect_error(iso_map_peaks(mutate(my_dt, map_id = ifelse(row_number() == 1, NA_character_, map_id)), my_peak_maps),
               "no.*map defined")

  expect_error(iso_map_peaks(mutate(my_dt, map_id = ifelse(row_number() == 1, "missing_pm", map_id)), my_peak_maps),
               "maps .* do not exist .* 'missing_pm'.* Available .* 'a_map', 'bc_map', 'd_map'")

  # check that it all assembles properly
  expect_equal(mapped_dt <- iso_map_peaks(my_dt, my_peak_maps),
               tibble(
                 file_id = c("a", "a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c"),
                 map_id = c("a_map", "a_map", "a_map", "a_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map"),
                 compound = c("X", "Y" , "Z", NA, "X", "Y", "Z", "Z", "X", "X", "Y", "Z"),
                 peak_info = c("X or Y? (1.1)", "X or Y? (1.1)", "Z (4)", "?? (5.8)", "X (1.5)", "Y or Z? (5)", "Y or Z? (5)", "Z? (5.8)", "X? (1.5)", "X? (2)", "Y (4.4?)", "Z (5.5)"),
                 rt = c(1.1, 1.1, 4, 5.8, 1.5, 5, 5, 5.8, 1.5, 2, 4.4, 5.5),
                 rt_start = c(0.9, 0.9, 3.8, 5.6, 1, 4.4, 4.4, 5.4, 1.1, 1.6, NA, 5.5),
                 rt_end = c(1.3, 1.3, 4.2, 6, 2, 5.6, 5.6, 6.2, 1.9, 2.4, NA, 5.5),
                 other_info = c(TRUE, FALSE, FALSE, NA, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
                 n_matches = as.integer(c(2, 2, 1, 0, 1, 2, 2, 1, 1, 1, 0, 1)),
                 n_overlapping = as.integer(c(1, 1, 1, 0, 1, 1, 2, 2, 2, 2, 0, 1)),
                 is_identified = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                 is_missing = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                 is_ambiguous = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
               ))

  expect_message(iso_map_peaks(my_dt, my_peak_maps), "8 of 9 peaks in 3 files.*5.*ambiguous.*1.*not be mapped.*1.*missing")
  expect_message(iso_map_peaks(my_dt, my_peak_maps), "1.*ambiguous.*multiple matching peak")
  expect_message(iso_map_peaks(my_dt, my_peak_maps), "3.*ambiguous.*overlapping")
  expect_message(iso_map_peaks(my_dt, my_peak_maps), "1.*ambiguous.*both")

  # test get problematic peaks
  expect_error(iso_get_problematic_peaks(), "no data table")
  expect_error(iso_get_problematic_peaks(tibble()), "unknown columns")
  expect_error(iso_get_problematic_peaks(mapped_dt, select = c()), "not.*correct number")

  expect_message(out <- mapped_dt %>% iso_get_problematic_peaks(
    select = c(file_id, rt, compound), ambiguous = FALSE, missing = TRUE, unidentified = FALSE),
    "fetching 1.*\\(missing\\)")
  expect_equal(out, tibble(file_id = "c", rt = 4.4, compound = "Y", peak_info = "Y (4.4?)", problem = "missing"))

  expect_message(out <- mapped_dt %>% iso_get_problematic_peaks(
    select = c(file_id, rt, compound), ambiguous = FALSE, missing = FALSE, unidentified = TRUE),
    "fetching 1.*\\(unidentified\\)")
  expect_equal(out, tibble(file_id = "a", rt = 5.8, compound = NA_character_, peak_info = "?? (5.8)", problem = "unidentified"))

  expect_message(out <- mapped_dt %>% iso_get_problematic_peaks(
    select = c(file_id, rt, compound), ambiguous = TRUE, missing = FALSE, unidentified = FALSE),
    "fetching 7.*\\(ambiguous\\)")
  expect_equal(out %>% select(-compound) %>% unique() %>% nrow(), 5L)
  expect_message(mapped_dt %>% iso_get_problematic_peaks(), "fetching 9.*\\(unidentified, missing or ambiguous\\)")

  # get removing problematic peaks
  expect_message(out <- mapped_dt %>% iso_remove_problematic_peaks(), "removing 9 of 12.*\\(unidentified, missing or ambiguous\\)")
  expect_equal(nrow(out), 3L)
  expect_equal(names(out), select(mapped_dt, -starts_with("is_"), -starts_with("n_")) %>% names())

  expect_message(out <- mapped_dt %>% iso_remove_problematic_peaks(remove_ambiguous = FALSE, remove_unidentified = FALSE),
                 "removing 1 of 12.*\\(missing\\)")
  expect_equal(nrow(out), 11L)
  expect_equal(names(out), select(mapped_dt, -is_missing) %>% names())

  expect_message(out <- mapped_dt %>% iso_remove_problematic_peaks(remove_ambiguous = FALSE, remove_missing = FALSE),
                 "removing 1 of 12.*\\(unidentified\\)")
  expect_equal(nrow(out), 11L)
  expect_equal(names(out), select(mapped_dt, -is_identified) %>% names())

  expect_message(out <- mapped_dt %>% iso_remove_problematic_peaks(remove_missing = FALSE, remove_unidentified = FALSE),
                 "removing 7 of 12.*\\(ambiguous\\)")
  expect_equal(nrow(out), 5L)
  expect_equal(names(out), select(mapped_dt, -is_ambiguous, -starts_with("n_")) %>% names())

  # peak summaries
  expect_equal(
    iso_summarize_peak_mappings(mapped_dt),
    tibble(
      file_id = c("a", "b", "c"),
      expected = c(2L, 3L, 4L),
      found = 3L,
      problematic = c(2L, 2L, 3L),
      peak_info = c("X or Y? (1.1), Z (4), ?? (5.8)", "X (1.5), Y or Z? (5), Z? (5.8)", "X? (1.5), X? (2), Y (4.4?), Z (5.5)")
    )
  )

  expect_equal(
    iso_get_problematic_peaks(mapped_dt) %>% iso_summarize_peak_mappings(),
    tibble(
      file_id = c("a", "b", "c"),
      expected = 1:3,
      found = c(2L, 2L, 2L),
      problematic = c(2L, 2L, 3L),
      peak_info = c("X or Y? (1.1), ?? (5.8)", "Y or Z? (5), Z? (5.8)", "X? (1.5), X? (2), Y (4.4?)")
    )
  )

})
