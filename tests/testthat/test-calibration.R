context("Calibration")

# standard addition =====

test_that("test that standard addition works properly", {

  expect_error(iso_add_standards(), "missing parameters")
  expect_error(iso_add_standards(42), "not defined")
  expect_error(iso_add_standards(tibble()), "no standards table")
  expect_error(iso_add_standards(tibble(), tibble()), "unknown column")

  # FIXME: elaborate on test cases
  # especially for standards added inside iso files

})

# calibration prep =====

test_that("test that calibration prep works properly", {

  expect_error(iso_prepare_for_calibration(), "no data table")
  expect_error(iso_prepare_for_calibration(tibble(all_data = NA)), "column already exists")
  expect_error(iso_prepare_for_calibration(tibble(), group_by = bla), "unknown column")

  # simple nesting
  expect_message(out <- iso_prepare_for_calibration(ggplot2::mpg), "nesting the entire dataset")
  expect_equal(names(out), "all_data")
  expect_equal(out %>% unnest(all_data), ggplot2::mpg)

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
  expect_message(df <- iso_prepare_for_calibration(
    mutate(ggplot2::mpg, datetime = Sys.time(), `Date & Time` = Sys.time()), group_by = year),
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
    "generating calibration.*1 model.*2 data group.*filter \'TRUE\'.*new column \'resid\'.*new column \'in_calib\'"
  )
  expect_equal(df_calib %>% all_calibrations(), "")
  expect_equal(df_calib %>% last_calibration(), "")
  expect_true(has_regression_fit(df_calib))
  expect_false(has_regression_fit(strip_regression_fits(df_calib)))
  expect_error(df_calib %>% iso_generate_calibration(model = lm(cty ~ hwy)), "already has an unnamed calibration")
  expect_message(
    df_calib2 <- df_calib %>% strip_regression_fits() %>% iso_generate_calibration(model = lm(cty ~ hwy), calibration = "x", use_in_calib = TRUE),
    "generating \'x\' calibration.*1 model.*2 data group.*filter \'TRUE\'.*new column \'x_resid\'.*new column \'x_in_calib\'"
  )
  expect_equal(df_calib2 %>% all_calibrations(), c("", "x"))
  expect_equal(df_calib2 %>% last_calibration(), "x")
  expect_true(has_regression_fit(df_calib2))
  expect_false(has_regression_fit(df_calib2, calibration = ""))
  expect_true(has_regression_fit(df_calib2, calibration = "x"))
  expect_false(has_regression_fit(strip_regression_fits(df_calib2), calibration = "x"))
  expect_error(df_calib2 %>% iso_generate_calibration(model = lm(cty ~ hwy), calibration = "x"), "already has a calibration name.*x")

  # loess
  expect_equal(correct_loess_date_time(df, quos(lm(y ~ datetime))), quos(lm(y ~ datetime)))
  expect_equal(correct_loess_date_time(df, quos(loess(y ~ datetime))), quos(`loess(y ~ datetime)` = loess(y ~ as.numeric(datetime))))
  expect_equal(correct_loess_date_time(df, quos(x = loess(y ~ `Date & Time`))), quos(x = loess(y ~ as.numeric(`Date & Time`))))
  expect_equal(
    correct_loess_date_time(df, quos(lm(y ~ datetime), loess(y ~ cty), loess(y ~ `Date & Time`), y = loess(y ~ datetime + datetime * `Date & Time`^2))),
    quos(lm(y ~ datetime), loess(y ~ cty), `loess(y ~ \`Date & Time\`)` = loess(y ~ as.numeric(`Date & Time`)), y = loess(y ~ as.numeric(datetime) + as.numeric(datetime) * as.numeric(`Date & Time`)^2))
  )
  expect_warning(df %>% iso_generate_calibration(model = loess(cty ~ hwy), use_in_calib = TRUE, quiet = TRUE), "discouraged")
  expect_silent(df %>% iso_generate_calibration(model = loess(cty ~ hwy), calibration = "drift", use_in_calib = TRUE, quiet = TRUE))

  # applying model
  expect_error(iso_apply_calibration(df_calib), "no variable.*specified")
  expect_message(iso_apply_calibration(df_calib, hwy), "applying calibration to infer.*hwy.*2 data group.*resulting value.*hwy_pred")
  expect_message(iso_apply_calibration(df_calib, hwy, calculate_error = TRUE), "applying calibration to infer.*hwy.*2 data group.*resulting value.*hwy_pred.*estimated error.*hwy_pred_se")
  expect_message(iso_apply_calibration(df_calib, cty), "applying calibration to infer.*cty.*2 data group.*resulting value.*cty_pred")
  expect_message(calib_applied <- iso_apply_calibration(df_calib2, cty, calculate_error = TRUE), "applying.*x.*calibration to infer.*cty.*2 data group.*resulting value.*cty_pred.*estimated error.*cty_pred_se")
  expect_error(df_calib %>% strip_regression_fits() %>% iso_apply_calibration(hwy), "regression fits.*no longer available")

  # range
  expect_error(calib_applied %>% iso_evaluate_calibration_range(DNE), "not.*valid")
  expect_message(calib_range <- calib_applied %>% iso_evaluate_calibration_range(cty, hwy, cty*hwy),
                 "evaluating range for terms.*cty.*hwy.*cty \\* hwy.*x.*calibration for 2 data group.*new column.*in_range")

  # get data
  expect_warning(calib_range %>% iso_get_calibration_data(keep_remaining_nested_data = TRUE), "renamed")
  expect_warning(calib_range %>% iso_get_calibration_data(keep_other_list_data = TRUE), "renamed")
  expect_message(out <- calib_range %>% iso_get_calibration_data(), "retrieving all data")
  calib_cols <- c("calib", "calib_ok", "calib_points", "x_calib", "x_calib_ok", "x_calib_points", "calib_params", "x_calib_params")
  new_cols <- c("in_calib", "resid", "x_in_calib", "x_resid", "cty_pred", "cty_pred_se", "x_in_range")
  expect_equal(names(out), an <- c("year", names(ggplot2::mpg)[names(ggplot2::mpg)!="year"], "datetime", "Date & Time", new_cols, calib_cols))
  expect_equal(iso_remove_problematic_calibrations(out) %>% names(), an[an!="x_calib_ok"])
  expect_equal(iso_remove_problematic_calibrations(out, calibration="") %>% names(), an[an!="calib_ok"])
  expect_message(out <- calib_range %>% iso_get_calibration_data(select = c(model, z = displ)), "retrieving data colum.*model.*z = displ.*keeping remaining data")
  expect_equal(names(out), c("year", "model", "z", "all_data", calib_cols))
  expect_message(out <- calib_range %>% iso_get_calibration_data(select = c(model, displ), keep_remaining_data = FALSE),
                 "retrieving data colum.*model, displ")
  expect_equal(
    calib_range %>% iso_get_calibration_data(keep_remaining_data = FALSE, keep_calibration_parameters = FALSE) %>% names(),
    an[an != "calib_params" & an != "x_calib_params"]
  )
  expect_equal(names(out), c("year", "model", "displ", calib_cols))
  expect_silent(calib_range %>% iso_get_calibration_data(quiet = TRUE))
  expect_equal(
    select(calib_range, x_calib_params) %>% tidyr::unnest(x_calib_params) %>% names(),
    c("model_fit", "model_coefs", "model_summary", "model_range")
  )
  expect_equal(
    calib_range %>% iso_get_calibration_data() %>%
      select(x_calib_params) %>% tidyr::unnest(x_calib_params) %>% names(),
    c("model_coefs", "model_summary", "model_range")
  )
  expect_equal(
    calib_range %>% iso_get_calibration_data(keep_calibration_regressions = TRUE) %>%
      select(x_calib_params) %>% tidyr::unnest(x_calib_params) %>% names(),
    c("model_fit", "model_coefs", "model_summary", "model_range")
  )

  # get coefficients
  expect_warning(calib_range %>% iso_get_calibration_coefficients(keep_remaining_nested_data = TRUE), "removed")
  expect_warning(calib_range %>% iso_get_calibration_coefficients(keep_other_list_data = TRUE), "renamed")
  expect_warning(calib_range %>% iso_get_calibration_coefficients(keep_calibration_regressions = TRUE), "cannot.*without.*parameters")
  expect_message(
    out <- calib_range %>% iso_get_calibration_coefficients(),
    "retrieving all coefficient.*x.*calibration"
  )
  expect_equal(names(out), c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "estimate", "std.error", "statistic", "p.value", "signif"))
  expect_message(
    out <- calib_range %>% iso_get_calibration_coefficients(select = c(term, z = estimate)),
    "retrieving.*coefficient column.*term.*z = estimate.*x.*calibration"
  )
  expect_equal(names(out), c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "z"))
  expect_equal(
    calib_range %>% iso_get_calibration_coefficients(select = c(term, estimate), keep_calibration_parameters = FALSE) %>% names(),
    c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "estimate")
  )
  expect_equal(
    calib_range %>% iso_get_calibration_coefficients(select = c(term, estimate), keep_other_calibrations = TRUE) %>% names(),
    c("year", "calib", "calib_ok", "calib_points", "x_calib", "x_calib_ok", "x_calib_points", "term", "estimate")
  )

  # get summary
  expect_warning(calib_range %>% iso_get_calibration_summary(keep_remaining_nested_data = TRUE), "removed")
  expect_warning(calib_range %>% iso_get_calibration_summary(keep_other_list_data = TRUE), "renamed")
  expect_warning(calib_range %>% iso_get_calibration_summary(keep_calibration_regressions = TRUE), "cannot.*without.*parameters")
  expect_message(
    out <- calib_range %>% iso_get_calibration_summary(),
    "retrieving all summary.*x.*calibration"
  )
  expect_equal(
    names(out),
    c("year", "x_calib", "x_calib_ok", "x_calib_points", "r.squared", "adj.r.squared", "sigma", "statistic", "p.value", "df", "logLik", "AIC", "BIC", "deviance", "df.residual"))
  expect_message(
    out <- calib_range %>% iso_get_calibration_summary(select = c(r.squared, RSD = sigma)),
    "retrieving.*summary column.*r.squared.*RSD = sigma.*x.*calibration"
  )
  expect_equal(names(out), c("year", "x_calib", "x_calib_ok", "x_calib_points", "r.squared", "RSD"))
  expect_equal(
    calib_range %>% iso_get_calibration_summary(select = c(r.squared, sigma), keep_calibration_parameters = FALSE) %>% names(),
    c("year", "x_calib", "x_calib_ok", "x_calib_points", "r.squared", "sigma")
  )
  expect_equal(
    calib_range %>% iso_get_calibration_summary(select = c(r.squared, sigma), keep_other_calibrations = TRUE) %>% names(),
    c("year", "calib", "calib_ok", "calib_points", "x_calib", "x_calib_ok", "x_calib_points", "r.squared", "sigma")
  )

  # get range
  expect_true(calib_range %>% has_model_range())
  expect_false(calib_range %>% has_model_range(calibration = ""))
  expect_warning(calib_range %>% iso_get_calibration_range(keep_remaining_nested_data = TRUE), "removed")
  expect_warning(calib_range %>% iso_get_calibration_range(keep_other_list_data = TRUE), "renamed")
  expect_warning(calib_range %>% iso_get_calibration_range(keep_calibration_regressions = TRUE), "cannot.*without.*parameters")
  expect_error(calib_range %>% iso_get_calibration_range(calibration = ""), "not yet.*evaluated")
  expect_message(
    out <- calib_range %>% iso_get_calibration_range(),
    "retrieving all calibration range.*x.*calibration"
  )
  expect_equal(
    names(out),
    c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "units", "min", "max"))
  expect_message(
    out <- calib_range %>% iso_get_calibration_range(select = c(term, u = units)),
    "retrieving.*range column.*term.*u = units.*x.*calibration"
  )
  expect_equal(names(out), c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "u"))
  expect_equal(
    calib_range %>% iso_get_calibration_range(select = c(term, units), keep_calibration_parameters = FALSE) %>% names(),
    c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "units")
  )
  expect_equal(
    calib_range %>% iso_get_calibration_range(select = c(term, units), keep_other_calibrations = TRUE) %>% names(),
    c("year", "calib", "calib_ok", "calib_points", "x_calib", "x_calib_ok", "x_calib_points", "term", "units")
  )

  # get parameters
  expect_warning(calib_range %>% iso_unnest_calibration_parameters(), "conflicting name")
  expect_equal(
    suppressWarnings(calib_range %>% iso_unnest_calibration_parameters(select_from_coefs = c(term, estimate), select_from_summary = c(sigma))) %>% names(),
    c("year", "x_calib", "x_calib_ok", "x_calib_points", "term", "estimate", "sigma")
  )


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
