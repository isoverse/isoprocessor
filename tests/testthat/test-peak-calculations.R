context("peak calculations")

test_that("test raw data and peak table combination", {

  # safety checks
  expect_error(iso_combine_raw_data_with_peak_table(), "must supply raw_data and peak_table")
  expect_error(iso_combine_raw_data_with_peak_table(tibble()), "must supply raw_data and peak_table")
  expect_error(iso_combine_raw_data_with_peak_table(peak_table = tibble()), "must supply raw_data and peak_table")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(), tibble()), "no peaks data")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(), tibble(x = 1)), "'file_id'.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(x = 1)), "file_id.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1)), "rt.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1), rt = my_rt), "my_rt.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1, rt = 1)), "rt_start.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1, rt = 1), rt_start = my_rt_start), "my_rt_start.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1, rt = 1, rt_start = 1)), "rt_end.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1, rt = 1, rt_start = 1), rt_end = my_rt_end), "my_rt_end.*unknown")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1), tibble(file_id = 1, rt = 1, rt_start = 1, rt_end = 1)), "unclear which column is the time column")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = 1, rt_start = 1, rt_end = "1")), "must be numeric")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = 1, rt_start = "1", rt_end = 1)), "must be numeric")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = "1", rt_start = 1, rt_end = 1)), "must be numeric")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = 1, rt_start = iso_double_with_units(1, "min"), rt_end = iso_double_with_units(1, "s"))), "conflicting units")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = 1, rt_start = 2, rt_end = 1)), "impossible peak definition")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = 1, rt_start = 2, rt_end = 3)), "impossible peak definition")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = 4, rt_start = 2, rt_end = 3)), "impossible peak definition")
  expect_error(iso_combine_raw_data_with_peak_table(tibble(file_id = 1, time.s=1), tibble(file_id = 1, rt = NA_real_, rt_start = NA_real_, rt_end = NA_real_)), "impossible peak definition")

  # no peaks that fit
  raw_data <- tibble(time.s = 1:10, value = 1:10, file_id=1)
  peak_table <- tibble(rt = c(20.), rt_start = c(NA_real_), rt_end = c(NA_real_), file_id = 1)
  expect_warning(
    result <- iso_combine_raw_data_with_peak_table(raw_data, peak_table),
    "no peaks that fit into the time window"
  )
  expect_equal(
    result,
    tidyr::crossing(
      raw_data,
      tibble(peak_marker = FALSE, peak_point = 0, peak_start = FALSE, peak_end = FALSE, rt = NA_real_, rt_start = NA_real_, rt_end = NA_real_)
    )
  )

  # peaks without start and end
  peak_table <- tibble(rt = c(3.1, 8.1), rt_start = c(NA_real_), rt_end = c(NA_real_), file_id = 1)
  expect_equal(
    iso_combine_raw_data_with_peak_table(raw_data, peak_table),
    dplyr::bind_cols(
      raw_data,
      result <- tibble(peak_marker = c(F,F,T,F,F,F,F,T,F,F), peak_point = c(0, 0, 1, 0, 0, 0, 0, 2, 0, 0),
                       peak_start =  c(F,T,T,F,F,F,T,T,F,F), peak_end = c(F,F,T,T,F,F,F,T,T,F),
                       rt = c(NA,NA,3.1,NA,NA,NA,NA,8.1,NA,NA), rt_start = NA_real_, rt_end = NA_real_)
    )
  )

  # peaks with same start and end
  expect_equal(
    iso_combine_raw_data_with_peak_table(raw_data, dplyr::mutate(peak_table, rt_start=rt, rt_end=rt)),
    dplyr::bind_cols(raw_data, dplyr::mutate(result,rt_start=rt,rt_end=rt))
  )

  # peaks with only start and end
  peak_table <- tibble(rt = c(NA_real_), rt_start = c(2.5), rt_end = c(6.4), file_id = 1)
  expect_equal(
    iso_combine_raw_data_with_peak_table(raw_data, peak_table),
    dplyr::bind_cols(
      raw_data,
      tibble(peak_marker = F, peak_point = c(0, 0, 1, 1, 1, 1, 0, 0, 0, 0),
             peak_start =  c(F,T,T,F,F,F,F,F,F,F), peak_end = c(F,F,F,F,F,T,T,F,F,F),
             rt = NA_real_, rt_start = c(NA,NA,2.5,2.5,2.5,2.5,NA,NA,NA,NA),
             rt_end = c(NA,NA,6.4,6.4,6.4,6.4,NA,NA,NA,NA))
    )
  )

  # peak data from multiple files
  raw_data <- tibble(time.s = rep(1:5, times = 2), value = time.s, file_id=rep(1:2, each = 5))
  peak_table <- tibble(rt = c(3.1), rt_start = c(1.8), rt_end = c(4.8), file_id = 1:2)
  expect_equal(
    iso_combine_raw_data_with_peak_table(raw_data, peak_table),
    dplyr::bind_cols(
      raw_data,
      tibble(peak_marker = c(F,F,T,F,F,F,F,T,F,F), peak_point = c(0, 1, 1, 1, 0, 0, 2, 2, 2, 0),
             peak_start =  c(T,T,F,F,F,T,T,F,F,F), peak_end = c(F,F,F,T,T,F,F,F,T,T),
             rt = c(NA,3.1, 3.1, 3.1,NA,NA,3.1,3.1,3.1,NA),
             rt_start = c(NA,1.8,1.8,1.8,NA,NA,1.8,1.8,1.8,NA),
             rt_end = c(NA,4.8,4.8,4.8,NA,NA,4.8,4.8,4.8,NA))
    )
  )

})
