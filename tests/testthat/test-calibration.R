context("Calibration")

# standard addition =====

test_that("test that standard addition works properly", {

  expect_error(iso_add_standards(), "no data table")
  expect_error(iso_add_standards(tibble()), "no standards table")
  expect_error(iso_add_standards(tibble(), tibble()), "unknown column")

  # FIXME: elaborate on test cases

})

# calibration prep =====

test_that("test that calibration prep works properly", {

  expect_error(iso_prepare_for_calibration(), "no data table")
  expect_error(iso_prepare_for_calibration(tibble(all_data = NA)), "column already exists")
  expect_error(iso_prepare_for_calibration(tibble(), group_by = bla), "unknown column")

  # simple nesting
  expect_message(out <- iso_prepare_for_calibration(tibble()), "nesting the entire dataset")
  expect_equal(names(out), "all_data")
  expect_equal(out %>% unnest(all_data), tibble())

  # grouped nesting
  expect_message(out <- iso_prepare_for_calibration(ggplot2::mpg, group_by = cyl), "grouping.*cyl")
  expect_equal(names(out), c("cyl", "all_data"))
  expect_equal(out %>% unnest(all_data), ggplot2::mpg)

  expect_message(out <- iso_prepare_for_calibration(ggplot2::mpg, group_by = c(cyl, drv)), "grouping.*cyl.*drv")
  expect_equal(names(out), c("cyl", "drv", "all_data"))
  expect_equal(out %>% unnest(all_data), ggplot2::mpg)

})

# calibrations vars =====

test_that("test that calibration variables work properly", {
  # get calibration vars function
  expect_error(get_calibration_vars(), "missing")
  expect_equal(get_calibration_vars(""),
               list(calib_name = "", model_name = "calib", model_enough_data = "calib_ok", model_data_points = "calib_points", model_params = "calib_params", residual = "resid", in_reg = "in_calib", in_range = "in_range"))
  expect_equal(vars <- get_calibration_vars("x"),
               list(calib_name = "'x' ", model_name = "x_calib", model_enough_data = "x_calib_ok", model_data_points = "x_calib_points", model_params = "x_calib_params", residual = "x_resid", in_reg = "x_in_calib", in_range = "x_in_range"))

  # check calibration cols function
  expect_error(check_calibration_cols(42), "not a data frame")
  expect_error(check_calibration_cols(mtcars, vars[c("model_enough_data", "model_params")]),
               "unknown column.*make sure to run.*use the same.*calibration")
  expect_silent(check_calibration_cols(tibble(x_calib_ok = TRUE, x_calib_params = list()),
                                       vars[c("model_enough_data", "model_params")]))
})

# default behavior of calibs =====

test_that("test default behavior of calibrations", {

  # test calibration
  expect_message(df <- iso_prepare_for_calibration(ggplot2::mpg, group_by = cyl),
                 "preparing.*calibration")
  expect_equal(df %>% all_calibrations(), character(0))
  expect_equal(df %>% last_calibration(check = FALSE), character(0))
  expect_error(df %>% last_calibration(), "not find.*calibration")
  expect_warning(
    df %>% iso_generate_calibration(model = lm(hwy ~ cty), is_std_peak = TRUE),
    "parameter.*was renamed to.*use_in_calib"
  )
  expect_warning(
    df %>% iso_generate_calibration(model = lm(hwy ~ cty), is_standard = TRUE),
    "parameter.*was renamed to.*use_in_calib"
  )
  expect_message(
    df_calib <- df %>% iso_generate_calibration(model = lm(hwy ~ cty), use_in_calib = TRUE),
    "generating calibration.*1 model.*4 data group.*filter \'TRUE\'.*new column \'resid\'.*new column \'in_calib\'"
  )
  expect_equal(df_calib %>% all_calibrations(), "")
  expect_equal(df_calib %>% last_calibration(), "")
  expect_message(
    df_calib2 <- df_calib %>% iso_generate_calibration(model = lm(cty ~ hwy), calibration = "x", use_in_calib = TRUE),
    "generating \'x\' calibration.*1 model.*4 data group.*filter \'TRUE\'.*new column \'x_resid\'.*new column \'x_in_calib\'"
  )
  expect_equal(df_calib2 %>% all_calibrations(), c("", "x"))
  expect_equal(df_calib2 %>% last_calibration(), "x")

  # FIXME: continue testin these!

})

# problematic calibrations ====

test_that("test that problematic calibrations can be removed properly", {

  expect_error(iso_get_problematic_calibrations(), "no data frame")
  expect_error(iso_get_problematic_calibrations(42), "no data frame")
  expect_error(iso_get_problematic_calibrations(tibble()), "could not find.*calibration")
  expect_message(iso_get_problematic_calibrations(tibble(calib_ok = TRUE, calib_params = list(tibble()))), "no problematic calibrations")
  expect_message(out <- iso_get_problematic_calibrations(tibble(name = c("x", "y"), calib_ok = c(TRUE, FALSE), calib_params = list(tibble())), select = name), "fetching problematic.*1 of 2")
  expect_equal(out, tibble(name = "y"))

  expect_message(out <- iso_remove_problematic_calibrations(tibble(name = c("x", "y"), calib_ok = c(TRUE, FALSE), calib_params = list(tibble()))), "removing problematic.*1 of 2")
  expect_equal(select(out, name), tibble(name = "x"))

})
