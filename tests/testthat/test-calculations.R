# ratios =======

context("Ratio calculations")

test_that("test that ratios can be calculated", {

  # confirm expected errors
  expect_warning(tryCatch(iso_calculate_ratios(isoreader:::make_cf_data_structure("NA")), error = function(e) {}), "read without extracting the raw data")
  expect_error(iso_calculate_ratios(), "missing")
  expect_error(iso_calculate_ratios(42), "not defined")
  expect_error(iso_calculate_ratios(tibble()), "no data")
  expect_error(iso_calculate_ratios(tibble(x=1)), "no ratios provided")
  expect_error(iso_calculate_ratios(tibble(x=1), "42"), "invalid ratio")
  expect_error(iso_calculate_ratios(tibble(x=1), "42/41"), "missing intensity column")
  expect_error(iso_calculate_ratios(tibble(x=1), "46/44", "47/44"), "make sure to pass ratios as a vector")
  expect_error(iso_calculate_ratios(tibble(v44.mV=1, v46.mV=1), "46/44"), "missing key column")

  # example calculation
  raw_data <- dplyr::tibble(file_id = "a", tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10))
  expect_message(iso_calculate_ratios(raw_data, ratios = c("46/44"), quiet = FALSE), "calculating ratio.*1 data file.*r46/44")
  expect_silent(raw_data_w_ratio <- iso_calculate_ratios(raw_data, ratios = c("46/44"), quiet = TRUE))
  expect_equal(raw_data_w_ratio$`r46/44`, with(raw_data, v46.mV/v44.mV))

  # example calculation in isofiles version
  iso_file <- isoreader:::make_cf_data_structure("a")
  iso_file$read_options$raw_data <- TRUE
  iso_file$raw_data <- dplyr::select(raw_data, -file_id)
  expect_true(iso_is_file(iso_file_w_ratio <- iso_calculate_ratios(iso_file, ratios = c("46/44"))))
  expect_equal(iso_file_w_ratio$raw_data$`r46/44`, with(iso_file$raw_data, v46.mV/v44.mV))
  expect_equal(iso_file_w_ratio %>% iso_get_raw_data(), raw_data_w_ratio)

})

# deltas =======

context("Deltas calculations")

test_that("test that deltas can be calculated", {

  # confirm expected errors
  expect_error(iso_calculate_deltas(), "missing")
  expect_error(iso_calculate_deltas(42), "not defined")
  expect_error(iso_calculate_deltas(tibble()), "no data")
  expect_error(iso_calculate_deltas(tibble(x=1)), "no deltas provided")
  expect_error(iso_calculate_deltas(tibble(x=1), "d42"), "invalid delta")
  expect_error(iso_calculate_deltas(tibble(x=1), "d42/41"), "missing ratio column")
  expect_error(iso_calculate_deltas(tibble(x=1), "d46/44", "d47/46"), "make sure to pass deltas as a vector")
  expect_error(iso_calculate_deltas(tibble(`r46/44`=1), "d46/44"), "missing key column")
  expect_error(iso_calculate_deltas(tibble(`r46/44`=1, file_id="A", type="standard", cycle="a"), "d46/44"))
  expect_error(iso_calculate_deltas(tibble(`r46/44`=1, file_id="A", type="standard", cycle=1, block="a"), "d46/44"))
  expect_error(
    iso_calculate_deltas(tibble(`r46/44`=1, file_id="A", type=c("x", "standard"), cycle=1), "d46/44"),
    "type column must have.*Missing.*sample.*Unexpected.*x"
  )

  # example calculation
  df <-
    bind_rows(
      tibble(
        file_id = "A",
        `r46/44` = c(1.5, 1.5, 1),
        `r45/44` = c(0.5, 0.75, 0.75),
        type = c("standard", "sample", "standard"),
        cycle = c(0, 1, 1),
        extra = "5"
      ),
      tibble(
        file_id = "B",
        `r46/44` = c(2, 1),
        `r45/44` = c(1, 2),
        type = c("standard", "sample"),
        cycle = c(1, 1),
        extra = "7"
      )
    )

  expect_equal(
    iso_calculate_deltas(df, c("d46/44", "d45/44")) %>% names(),
    c("file_id", "r46/44", "r45/44", "type", "cycle", "extra", "d46/44.permil", "d45/44.permil"))
  expect_equal(
    iso_calculate_deltas(df, c("d46/44", "d45/44"))$`d46/44.permil`,
    c(NA_real_, 200., NA_real_, NA_real_, NA_real_))
  expect_equal(
    iso_calculate_deltas(df, c("d46/44", "d45/44"), in_permil = FALSE)$`d46/44`,
    c(NA_real_, 0.200, NA_real_, NA_real_, NA_real_))
  expect_equal(
    iso_calculate_deltas(df, c("d46/44", "d45/44"))$`d45/44.permil`,
    c(NA_real_, 200., NA_real_, NA_real_, NA_real_))
  expect_equal(
    iso_calculate_deltas(df, c("d46/44", "d45/44"), bracket = FALSE)$`d46/44.permil`,
    c(NA_real_, 500., NA_real_, NA_real_, -500))
  expect_equal(
    iso_calculate_deltas(df, c("d46/44", "d45/44"), bracket = FALSE)$`d45/44.permil`,
    c(NA_real_, 0., NA_real_, NA_real_, 1000))


  # dual inlet iso files
  expect_error(iso_calculate_deltas(isoreader:::make_cf_data_structure("NA")), "only.*dual inlet files")
  expect_warning(tryCatch(iso_calculate_deltas(isoreader:::make_di_data_structure("NA")), error = function(e) {}), "read without extracting the raw data")

})
