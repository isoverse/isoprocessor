# peak Mapping ====

context("Peak mapping")

test_that("testing that peak mapping works", {

  expect_error(iso_map_peaks(5), "not defined")
  expect_error(iso_map_peaks(tibble()), "no peak map*")
  expect_error(iso_map_peaks(tibble(), tibble()), "no data")
  expect_error(iso_map_peaks(isoreader:::make_di_data_structure("a")), "only.*continuous flow")

  # data table
  my_dt <- tibble(
    file_id = rep(c("a", "b", "c"), each = 3),
    map_id = rep(c("a_map", "bc_map", "bc_map"), each = 3),
    #            X+Y,   Z,  NA,   X, Y+Z,   Z,   X,   X,   Z
    rt       = c(1.1, 4.0, 5.8, 1.5, 5.0, 5.8, 1.5, 2.0, 5.5),
    rt_start = c(0.9, 3.8, 5.6, 1.0, 4.4, 5.4, 1.1, 1.6, 5.5),
    rt_end   = c(1.3, 4.2, 6.0, 2.0, 5.6, 6.2, 1.9, 2.4, 5.5)
  )

  # single map
  my_peak_map <- tibble(
    compound = c("W", "X", "Y", "Z"),
    other_info = c(TRUE, FALSE, FALSE, FALSE),
    rt = c(1.3, 1.8, 4.2, 5.6)
  )

  # maps
  my_peak_maps <- tibble(
    compound = c("X", "Y", "Z"),
    other_info = c(TRUE, FALSE, FALSE),
    `rt:a_map` = c(1.1, 1.3, 4.2),
    `rt:bc_map` = c(1.8, 4.4, 5.5),
    `rt:d_map` = c(1, 2, 3)
  )

  # errors
  expect_error(iso_map_peaks(select(my_dt, -map_id), rename(my_peak_map, rt2=rt)), "map id defined.*but.*map_id.*column does not exist")
  expect_error(iso_map_peaks(select(my_dt, -map_id), my_peak_maps), "more than one map defined.*map_id.*does not exist")
  expect_error(iso_map_peaks(mutate(my_dt, map_id = ifelse(row_number() == 1, NA_character_, map_id)), my_peak_maps),
               "no.*map defined")
  expect_error(iso_map_peaks(mutate(my_dt, map_id = ifelse(row_number() == 1, "missing_pm", map_id)), my_peak_maps),
               "maps .* do not exist .* 'missing_pm'.* Available .* 'a_map', 'bc_map', 'd_map'")
  expect_error(iso_map_peaks(my_dt, my_peak_maps, map_id = c(file_id, map_id)),
               "map id must be stored in a single column")
  expect_error(iso_map_peaks(my_dt, my_peak_maps %>% dplyr::select(-starts_with("rt"))),
               "peak maps do not have any columns that match or start with.*retention time column")

  # check that it all assembles properly for single maps
  expect_equal(mapped_dt <- iso_map_peaks(my_dt, my_peak_map),
               tibble(
                 file_id = c("a", "a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", "c"),
                 compound = c("W", "X", "Y", "Z", "W", "X", "Y", "Z", "Z", "W", "X", "X", "Y", NA, "Z"),
                 peak_info = c("W (1.1)", "X (1.8??)", "Y (4)", "Z (5.8)", "W or X? (1.5)", "W or X? (1.5)", "Y (4.2??)", "Z? (5)", "Z? (5.8)",
                               "W or X? (1.5)", "W or X? (1.5)", "X? (2)", "Y (4.2??)", "?? (5.5)", "Z (5.6??)"),
                 other_info = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, NA, FALSE),
                 map_id = c("a_map", NA, "a_map", "a_map", "bc_map", "bc_map", NA, "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", NA, "bc_map", NA),
                 rt = c(1.1, 1.8, 4, 5.8, 1.5, 1.5, 4.2, 5, 5.8, 1.5, 1.5, 2, 4.2, 5.5, 5.6),
                 rt_start = c(0.9, NA, 3.8, 5.6, 1, 1, NA, 4.4, 5.4, 1.1, 1.1, 1.6, NA, 5.5, NA),
                 rt_end = c(1.3, NA, 4.2, 6, 2, 2, NA, 5.6, 6.2, 1.9, 1.9, 2.4, NA, 5.5, NA),
                 n_overlapping = c(1L, 0L, 1L, 1L, 1L, 1L, 0L, 2L, 2L, 1L, 2L, 2L, 0L, 0L, 0L),
                 n_matches = c(1L, 0L, 1L, 1L, 2L, 2L, 0L, 1L, 1L, 2L, 2L, 1L, 0L, 0L, 0L),
                 is_identified = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
                 is_missing = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
                 is_ambiguous = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
               ))
  expect_message(iso_map_peaks(my_dt, my_peak_map), "8 of 9 peaks in 3 files.*using a single peak map.*5.*ambiguous.*1.*not be mapped.*4.*missing")
  expect_message(iso_map_peaks(my_dt, my_peak_map), "1.*ambiguous.*multiple matching peak")
  expect_message(iso_map_peaks(my_dt, my_peak_map), "3.*ambiguous.*overlapping")
  expect_message(iso_map_peaks(my_dt, my_peak_map), "1.*ambiguous.*both")

  # check that it all assembles properly for multi maps
  expect_equal(mapped_dt <- iso_map_peaks(my_dt, my_peak_maps),
               tibble(
                 file_id = c("a", "a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c"),
                 map_id = c("a_map", "a_map", "a_map", "a_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map", "bc_map"),
                 compound = c("X", "Y" , "Z", NA, "X", "Y", "Z", "Z", "X", "X", "Y", "Z"),
                 peak_info = c("X or Y? (1.1)", "X or Y? (1.1)", "Z (4)", "?? (5.8)", "X (1.5)", "Y or Z? (5)", "Y or Z? (5)", "Z? (5.8)", "X? (1.5)", "X? (2)", "Y (4.4??)", "Z (5.5)"),
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

  expect_message(iso_map_peaks(my_dt, my_peak_maps), "8 of 9 peaks in 3 files.*using 2 peak maps.*5.*ambiguous.*1.*not be mapped.*1.*missing")
  expect_message(iso_map_peaks(my_dt, my_peak_maps), "1.*ambiguous.*multiple matching peak")
  expect_message(iso_map_peaks(my_dt, my_peak_maps), "3.*ambiguous.*overlapping")
  expect_message(iso_map_peaks(my_dt, my_peak_maps), "1.*ambiguous.*both")

  # pre-existing mappings / overwrite
  expect_equal(mapped_dt, iso_map_peaks(mutate(my_dt, compound = "test"), my_peak_maps))
  expect_message(iso_map_peaks(mutate(my_dt, compound = "test"), my_peak_maps), "previous peak mappings.*9.*overwritten")

  # test get problematic peaks
  expect_warning(try(iso_get_problematic_peaks()), "renamed")
  expect_error(iso_get_problematic_peak_mappings(5), "not defined")
  expect_error(iso_get_problematic_peak_mappings(tibble()), "unknown columns")
  expect_error(iso_get_problematic_peak_mappings(mapped_dt, select = c()), "not.*correct number")

  expect_message(out1 <- mapped_dt %>% iso_get_problematic_peak_mappings(
    select = c(file_id, rt, compound), ambiguous = FALSE, missing = TRUE, unidentified = FALSE),
    "fetching 1.*\\(missing\\)")
  expect_equal(out1, tibble(file_id = "c", rt = 4.4, compound = "Y", peak_info = "Y (4.4??)", problem = "missing"))

  expect_message(out2 <- mapped_dt %>% iso_get_problematic_peak_mappings(
    select = c(file_id, rt, compound), ambiguous = FALSE, missing = FALSE, unidentified = TRUE),
    "fetching 1.*\\(unidentified\\)")
  expect_equal(out2, tibble(file_id = "a", rt = 5.8, compound = NA_character_, peak_info = "?? (5.8)", problem = "unidentified"))

  expect_message(out3 <- mapped_dt %>% iso_get_problematic_peak_mappings(
    select = c(file_id, rt, compound), ambiguous = TRUE, missing = FALSE, unidentified = FALSE),
    "fetching 7.*\\(ambiguous\\)")
  expect_equal(out3 %>% select(-compound) %>% unique() %>% nrow(), 5L)
  expect_message(mapped_dt %>% iso_get_problematic_peak_mappings(), "fetching 9.*\\(unidentified, missing or ambiguous\\)")

  # removing problematic peaks
  expect_message(out4 <- mapped_dt %>% iso_remove_problematic_peak_mappings(), "removing 9 of 12.*\\(unidentified, missing or ambiguous\\)")
  expect_equal(nrow(out4), 3L)
  expect_equal(names(out4), select(mapped_dt, -starts_with("is_"), -starts_with("n_")) %>% names())

  expect_message(out5 <- mapped_dt %>% iso_remove_problematic_peak_mappings(remove_ambiguous = FALSE, remove_unidentified = FALSE),
                 "removing 1 of 12.*\\(missing\\)")
  expect_equal(nrow(out5), 11L)
  expect_equal(names(out5), select(mapped_dt, -is_missing) %>% names())

  expect_message(out6 <- mapped_dt %>% iso_remove_problematic_peak_mappings(remove_ambiguous = FALSE, remove_missing = FALSE),
                 "removing 1 of 12.*\\(unidentified\\)")
  expect_equal(nrow(out6), 11L)
  expect_equal(names(out6), select(mapped_dt, -is_identified) %>% names())

  expect_message(out7 <- mapped_dt %>% iso_remove_problematic_peak_mappings(remove_missing = FALSE, remove_unidentified = FALSE),
                 "removing 7 of 12.*\\(ambiguous\\)")
  expect_equal(nrow(out7), 5L)
  expect_equal(names(out7), select(mapped_dt, -is_ambiguous, -starts_with("n_")) %>% names())

  # peak summaries
  expect_error(iso_summarize_peak_mappings(1), "not defined")
  expect_error(iso_summarize_peak_mappings(tibble()), "unknown columns")

  expect_equal(
    iso_summarize_peak_mappings(mapped_dt),
    tibble(
      file_id = c("a", "b", "c"),
      mapped = c("2/3", "3/3", "3/3"),
      ambiguous = c("1/2", "2/3", "2/3"),
      missing = c("0/3", "0/3", "1/3"),
      peak_info = c("X or Y? (1.1), Z (4), ?? (5.8)", "X (1.5), Y or Z? (5), Z? (5.8)", "X? (1.5), X? (2), Y (4.4??), Z (5.5)")
    )
  )

  expect_equal(
    iso_get_problematic_peak_mappings(mapped_dt) %>% iso_summarize_peak_mappings(),
    tibble(
      file_id = c("a", "b", "c"),
      mapped = c("1/2", "2/2", "2/2"),
      ambiguous = c("1/1", "2/2", "2/2"),
      missing = c("0/2", "0/2", "1/2"),
      peak_info = c("X or Y? (1.1), ?? (5.8)", "Y or Z? (5), Z? (5.8)", "X? (1.5), X? (2), Y (4.4??)")
    )
  )

  # test mapping with iso files
  iso_file_a <- isoreader:::make_cf_data_structure("a")
  iso_file_a$read_options$file_info <- TRUE
  iso_file_b <- isoreader:::make_cf_data_structure("b")
  iso_file_b$read_options$file_info <- TRUE
  iso_file_c <- isoreader:::make_cf_data_structure("c")
  iso_file_c$read_options$file_info <- TRUE
  iso_files <- c(iso_file_a, iso_file_b, iso_file_c)
  expect_warning(iso_map_peaks(iso_file_a, my_peak_map), "no.*peak_table")
  expect_warning(iso_map_peaks(iso_files, my_peak_map), "no.*peak_table")
  expect_message(iso_files <- iso_add_file_info(iso_files, select(my_dt, file_id, map_id) %>% unique(), by = "file_id"))
  expect_message(iso_files <- iso_set_peak_table(iso_files, select(my_dt, -map_id)), "setting peak table for 3/3")
  expect_equal(iso_files %>% iso_get_peak_table(include_file_info = map_id), my_dt)
  expect_message(mapped_iso <- iso_map_peaks(iso_files, my_peak_maps), "8 of 9 peaks in 3 files.*using 2 peak maps.*5.*ambiguous.*1.*not be mapped.*1.*missing")
  expect_equal(iso_get_peak_table(mapped_iso, include_file_info = map_id), mapped_dt)

  # test problematic peaks with iso_files
  expect_equal(
    mapped_iso %>% iso_get_problematic_peak_mappings(
      select = c(file_id, rt, compound), ambiguous = FALSE, missing = TRUE, unidentified = FALSE),
    out1)
  expect_equal(
    mapped_iso %>% iso_get_problematic_peak_mappings(
      select = c(file_id, rt, compound), ambiguous = FALSE, missing = FALSE, unidentified = TRUE),
    out2)
  expect_equal(
    mapped_iso %>% iso_get_problematic_peak_mappings(
      select = c(file_id, rt, compound), ambiguous = TRUE, missing = FALSE, unidentified = FALSE),
    out3)

  expect_equal(out4, mapped_iso %>% iso_remove_problematic_peak_mappings() %>% iso_get_peak_table(include_file_info = map_id))
  expect_equal(out5, mapped_iso %>% iso_remove_problematic_peak_mappings(remove_ambiguous = FALSE, remove_unidentified = FALSE) %>%
                 iso_get_peak_table(include_file_info = map_id))
  expect_equal(out6, mapped_iso %>% iso_remove_problematic_peak_mappings(remove_ambiguous = FALSE, remove_missing = FALSE) %>%
                 iso_get_peak_table(include_file_info = map_id))
  expect_equal(out7, mapped_iso %>% iso_remove_problematic_peak_mappings(remove_missing = FALSE, remove_unidentified = FALSE) %>%
                 iso_get_peak_table(include_file_info = map_id))
  expect_equal(iso_summarize_peak_mappings(mapped_dt), iso_summarize_peak_mappings(mapped_iso))
  expect_equal(iso_summarize_peak_mappings(mapped_dt, file_id = c(file_id, map_id)),
               iso_summarize_peak_mappings(mapped_iso, include_file_info = map_id))

})
