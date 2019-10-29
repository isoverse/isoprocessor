context("Peak table data structure")

# test setting peak table  =======

test_that("test that setting peak table works", {

  # test data
  iso_file_a <- isoreader:::make_cf_data_structure("a")
  iso_file_b <- isoreader:::make_cf_data_structure("b")
  iso_files <- c(iso_file_a, iso_file_b)

  # errors
  expect_error(iso_set_peak_table(1), "only.*for continuous flow")
  expect_error(iso_set_peak_table(isoreader:::make_di_data_structure("NA")), "only.*continuous flow")
  expect_error(iso_set_peak_table(iso_file_a, peak_table = tibble()), "no.*file_id")

  # test data
  data <- tibble(file_id = rep(c("a", "b"), each = 5), x = 1:10, y = iso_double_with_units(1:10, "V"))

  # all set
  expect_silent(out <- iso_set_peak_table(iso_files, data, quiet = TRUE))
  expect_message(out <- iso_set_peak_table(iso_files, data), "setting peak table for 2/2")
  expect_equal(out$a$peak_table, dplyr::filter(data, file_id == "a") %>% dplyr::select(-file_id))
  expect_equal(out$b$peak_table, dplyr::filter(data, file_id == "b") %>% dplyr::select(-file_id))

  # resets
  expect_message(out2 <- iso_set_peak_table(out, dplyr::filter(data, file_id=="a")), "setting peak table for 1/2.*resetting.*for 1")
  expect_equal(out2$a$peak_table, dplyr::filter(data, file_id == "a") %>% dplyr::select(-file_id))
  expect_equal(out2$b$peak_table, tibble())
  expect_message(out3 <- iso_set_peak_table(out2, dplyr::filter(data, file_id=="b"), reset_missing = FALSE), "setting peak table for 1/2.*keeping.*unchanged.*for 1")
  expect_equal(out3$a$peak_table, dplyr::filter(data, file_id == "a") %>% dplyr::select(-file_id))
  expect_equal(out3$b$peak_table, dplyr::filter(data, file_id == "b") %>% dplyr::select(-file_id))

  # unmatched
  expect_message(out4 <- iso_set_peak_table(iso_files, dplyr::mutate(data, file_id = "c")), "setting peak table for 0/2.*resetting.*for 2.*ignoring.*for 1")
  expect_equal(out4$a$peak_table, tibble())
  expect_equal(out4$b$peak_table, tibble())

})

test_that("test that setting peak table from vendor data table works", {

  # test data
  iso_file_a <- isoreader:::make_cf_data_structure("a")
  iso_file_a$vendor_data_table <- tibble(a = 1, b1 = 1, b2 = 1)
  iso_file_b <- isoreader:::make_cf_data_structure("b")
  iso_files <- c(iso_file_a, iso_file_b)

  # errors
  expect_error(iso_set_peak_table_from_vendor_data_table(1), "only.*for continuous flow")
  expect_error(iso_set_peak_table_from_vendor_data_table(isoreader:::make_di_data_structure("NA")), "only.*continuous flow")
  expect_error(iso_set_peak_table_from_vendor_data_table(iso_file_a), "must provide either")
  expect_error(iso_set_peak_table_from_vendor_data_table(iso_file_a, direct_rename = "NA"), "must be.*named")
  expect_error(iso_set_peak_table_from_vendor_data_table(iso_file_a, regex_rename = "NA"), "must be.*named")

  # testing
  expect_message(
    out <- iso_set_peak_table_from_vendor_data_table(iso_files, direct_rename = c(a2 = "a"), regex_rename = c("c\\1" = "b(\\d)")),
    "setting.*from vendor data table.*for 1 file.*'a'->'a2'.*'b1'->'c1'.*'b2'->'c2'"
  )
  expect_equal(out$a$peak_table, rename(iso_file_a$vendor_data_table, a2 = a, c1 = b1, c2 = b2))
  expect_null(out$b$peak_table)

  # testing with all the files
  iso_file_b$vendor_data_table <- tibble(b2 = 1, b3 = 1)
  iso_file_c <- iso_file_b
  iso_file_c$file_info$file_id <- "c"
  iso_files <- c(iso_file_a, iso_file_b, iso_file_c)
  expect_message(
    out <- iso_set_peak_table_from_vendor_data_table(iso_files, direct_rename = c(a2 = "a"), regex_rename = c("c\\1" = "b(\\d)")),
    "setting.*from vendor data table.*for 2 file.*'b2'->'c2'.*'b3'->'c3'.*1 file.*'a'->'a2'.*'b1'->'c1'.*'b2'->'c2'"
  )
  expect_equal(out$a$peak_table, rename(iso_file_a$vendor_data_table, a2 = a, c1 = b1, c2 = b2))
  expect_equal(out$b$peak_table, rename(iso_file_b$vendor_data_table, c2 = b2, c3 = b3))
  expect_equal(out$c$peak_table, rename(iso_file_b$vendor_data_table, c2 = b2, c3 = b3))

  # isodat vendor data table
  iso_file_a$vendor_data_table <- tibble(
    `Nr.` = 1, `Is Ref.?` = 1, Start = 1, Rt = 1, End = 1,
    `Ampl 2` = 1, `Ampl 3` = 1, `BGD 2` = 1, `BGD 3` = 1,
    `rIntensity 2` = 1, `rIntensity 3` = 1,
    `rR 3H2/2H2` = 1, `rd 3H2/2H2` = 1, `d 3H2/2H2` = 1,
    `d 2H/1H` = 1, `AT% 2H/1H` = 1
  )
  expect_message(out <- iso_set_peak_table_from_isodat_vendor_data_table(iso_file_a), "setting peak table")
  expect_equal(
    out$peak_table,
    tibble(peak_nr = 1, is_ref = 1, rt_start = 1, rt = 1, rt_end = 1,
           amp2 = 1, amp3 = 1, bgrd2_start = 1, bgrd3_start = 1, bgrd2_end = 1, bgrd3_end = 1,
           area2 = 1, area3 = 1, `r3/2` = 1, `rd3/2` = 1, `d3/2` = 1, `d2H` = 1, `at2H` = 1)
  )

})

# peak table mutate =====

test_that("test that peak table mutate works", {

  # test data
  iso_file_a <- isoreader:::make_cf_data_structure("a")
  iso_file_a$peak_table <- tibble(a = 1, b1 = 1, b2 = 1)
  iso_file_b <- isoreader:::make_cf_data_structure("b")
  iso_file_b$peak_table <- tibble(b2 = 1, b3 = 1)
  iso_files <- c(iso_file_a, iso_file_b)

  # mutate test
  expect_error(iso_mutate_peak_table(1L), "not defined")
  expect_silent(iso_mutate_peak_table(iso_file_a, quiet = TRUE))
  expect_message(out <- iso_mutate_peak_table(iso_file_a), "mutating.*1")
  expect_equal(out$peak_table, iso_file_a$peak_table)
  expect_message(out <- iso_mutate_peak_table(iso_files, x = 5 * b1), "mutating.*2")
  expect_equal(out$a$peak_table, mutate(iso_file_a$peak_table, x = 5 * b1))
  expect_equal(select(out$b$peak_table, -x), iso_file_b$peak_table)
  expect_equal(out$b$peak_table$x, NA_real_)

})

# test aggregation =====

test_that("test that peak table aggregation works", {

  # errors
  expect_error(iso_get_peak_table(5), "incompatible data type")
  expect_error(iso_get_peak_table(isoreader:::make_di_data_structure("NA")), "only available in continuous flow")
  expect_warning(out <- iso_get_peak_table(isoreader:::make_cf_data_structure("NA")), "none.*has a peak_table")
  expect_equal(out, tibble(file_id = character(0)))

  # test data
  iso_file_a <- isoreader:::make_cf_data_structure("a")
  iso_file_a$read_options$file_info <- TRUE
  iso_file_a$peak_table <- tibble(x = 1:5, y = iso_double_with_units(1:5, "V"))
  iso_file_b <- iso_file_a
  iso_file_b$file_info$file_id <- "b"
  iso_files <- c(iso_file_a, iso_file_b)

  # with / without units
  expect_silent(iso_get_peak_table(iso_file_a, quiet = TRUE))
  expect_message(out <- iso_get_peak_table(iso_file_a), "aggregating peak table.*1")
  expect_message(out_units <- iso_get_peak_table(iso_file_a, with_explicit_units = TRUE), "aggregating peak table with explicit units.*1")
  expect_equal(out, tibble(file_id = "a", x = 1:5, y = iso_double_with_units(1:5, "V")))
  expect_equal(out_units, tibble(file_id = "a", x = 1:5, `y [V]` = as.numeric(1:5)))
  expect_message(out <- iso_get_peak_table(iso_files), "aggregating peak table.*2")
  expect_message(out_units <- iso_get_peak_table(iso_files, with_explicit_units = TRUE), "aggregating peak table with explicit units.*2")
  expect_equal(
    out,
    vctrs::vec_rbind(tibble(file_id = "a", x = 1:5, y = iso_double_with_units(1:5, "V")), tibble(file_id = "b", x = 1:5, y = iso_double_with_units(1:5, "V")))
  )
  expect_equal(
    out_units,
    vctrs::vec_rbind(tibble(file_id = "a", x = 1:5, `y [V]` = as.numeric(1:5)), tibble(file_id = "b", x = 1:5, `y [V]` = as.numeric(1:5)))
  )

  # selecting/renaming specific columns
  expect_warning(agg <- iso_get_peak_table(iso_files, select = c(bla, y)), "unknown column")
  expect_equal(names(agg), c("file_id", "y"))
  expect_equal(names(iso_get_peak_table(iso_files, select = c(file_id, y))), c("file_id", "y"))
  expect_equal(names(iso_get_peak_table(iso_files, select = c(x = file_id, y2 = y))), c("x", "y2"))
  expect_equal(names(iso_get_peak_table(iso_files, select = c(z = starts_with("file")))), c("z"))
  expect_equal(names(iso_get_peak_table(iso_files, select = c())), c("file_id"))

  # include file info
  iso_file_a <- modifyList(iso_file_a, list(file_info = list(test_info = "x")))
  iso_file_b <- modifyList(iso_file_b, list(file_info = list(test_info = "y")))
  iso_files <- c(iso_file_a, iso_file_b)
  expect_true("test_info" %in% names(agg <- iso_get_peak_table(iso_files, include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  expect_equal(names(iso_get_peak_table(iso_files, select = y, include_file_info = c(a = test_info))), c("file_id", "a", "y"))

  # make sure that files that have no data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_peak_table(
      c(isoreader:::make_cf_data_structure("NA"), iso_file_a, iso_file_b),
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )

})
