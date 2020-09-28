context("Utilities and other convenience functions")

# nesting and unnesting ======

test_that("nesting and unnesting functions work properly", {

  # testing the nest data function
  test_df <- dplyr::tibble(x=1:5, z = "text", y =c("a", "a", "b", "a", "b"))
  expect_error(nest_data(), "no data table supplied")
  expect_error(nest_data(test_df, DNE), "refers to unknown column")
  expect_equal(nest_data(test_df) %>% names(), c("nested_data"))
  expect_equal(nest_data(test_df, y) %>% names(), c("y", "nested_data"))
  expect_equal(nest_data(test_df, y, nested_data = my_data) %>% names(), c("y", "my_data"))
  expect_equal(nest_data(test_df, c(y,z)) %>% names(), c("z", "y", "nested_data"))
  expect_equal(nest_data(test_df, c("y","z")) %>% names(), c("z", "y", "nested_data"))
  expect_equal(nest_data(test_df, -x) %>% names(), c("z", "y", "nested_data"))
  expect_equal(nest_data(test_df, starts_with("y")) %>% names(), c("y", "nested_data"))

  # testing unnest_select_data
  nested_df <- nest_data(test_df, y)
  expect_error(unnest_select_data(), "no data table supplied")
  expect_error(unnest_select_data(nested_df, nested_data = my_nested), "refers to unknown column")
  expect_error(unnest_select_data(nested_df, nested_data = y), "not .* correct column type")
  expect_error(unnest_select_data(nested_df, select = DNE), "refers to unknown column")
  expect_equal(unnest_select_data(nested_df, select = c()) %>% names(), nested_df %>% names())
  expect_equal(unnest_select_data(nested_df, z) %>% names(), c("y", "z", "nested_data"))
  expect_equal(unnest_select_data(nested_df, "z") %>% names(), c("y", "z", "nested_data"))
  expect_equal(unnest_select_data(nested_df, z) %>% nrow(), nrow(nested_df))
  expect_equal(unnest_select_data(nested_df, x) %>% names(), c("y", "x", "nested_data"))
  expect_equal(unnest_select_data(nested_df, c(x2 = x)) %>% names(), c("y", "x2", "nested_data"))
  expect_equal(unnest_select_data(nested_df, x) %>% nrow(), nrow(test_df))
  expect_equal(unnest_select_data(nested_df, select = everything()) %>% names(), c("y", "x", "z"))
  expect_equal(unnest_select_data(nested_df) %>% nrow(), nrow(test_df))

  # making sure unnest can deal with <NULL> columns
  null_nested_df <- nested_df %>%
    mutate(nested_data = map2(nested_data, row_number(), ~if(.y == 2) {NULL} else {.x}))
  expect_equal(unnest_select_data(null_nested_df, select = c()) %>% names(), nested_df %>% names())
  expect_equal(unnest_select_data(null_nested_df) %>% nrow(), nrow(filter(test_df, y == "a")))

  # introduce more nested column
  mutated_nested_df <- mutate(nested_df, test_data = purrr::map(nested_data, ~dplyr::rename(.x, x2=x, z2=z)))
  expect_equal(unnest_select_data(mutated_nested_df, z) %>% names(), c("y", "z", "nested_data", "test_data"))
  expect_error(unnest_select_data(nested_df, z, nested = test_data), "refers to unknown column")
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data) %>% names(), c("y", "nested_data", "z2", "test_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_other_list_data = FALSE) %>% names(), c("y", "z2", "test_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_remaining_nested_data = FALSE) %>% names(), c("y", "nested_data", "z2"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_other_list_data = FALSE, keep_remaining_nested_data = FALSE) %>% names(), c("y", "z2"))

})

# regression variables ===

test_that("test that model variables work", {

  expect_error(get_formula_variables(1), "must be a quosure")
  expect_error(get_formula_variables(quo(1)), "not a valid formula")
  expect_error(get_formula_variables(quo(x)), "not a valid formula")
  expect_error(get_formula_variables(quo(x+y)), "not a valid formula")
  expect_equal(get_formula_variables(quo(y~.)), c("y", "."))
  expect_equal(get_formula_variables(quo(.~x)), c(".", "x"))
  expect_equal(get_formula_variables(quo(y~x)), c("y", "x"))
  expect_equal(get_formula_variables(quo(y1+y2~x1+x2)), c("y1", "y2", "x1", "x2"))
  expect_equal(get_formula_variables(quo(y1+y2~x1+x2^2+x2*x3+sqrt(x4)^I(x5))), c("y1", "y2", "x1", "x2", "x3", "x4", "x5"))
  expect_equal(get_formula_variables(quo(y1+y2~x1+x2^2+x2*x3+sqrt(x4)^I(x5)), get_x = FALSE), c("y1", "y2"))
  expect_equal(get_formula_variables(quo(y1+y2~x1+x2^2+x2*x3+sqrt(x4)^I(x5)), get_y = FALSE), c("x1", "x2", "x3", "x4", "x5"))
  expect_null(get_formula_variables(quo(y~x), get_x = FALSE, get_y = FALSE))

})

# regression functions =====

test_that("regression functions work properly", {

  set.seed(42)
  test_df <- dplyr::tibble(name = rep(c("a", "b"), 10), x = runif(20), y = runif(20), z = iso_double_with_units(runif(20), "z"))
  nested_test_df <- nest_data(test_df, name, nested_data = model_data)
  expect_error(run_regression(), "no data table supplied")
  expect_error(run_regression(test_df), "model_data.* unknown column")
  expect_error(run_regression(nested_test_df), "no .* model")
  expect_error(run_regression(nested_test_df, model = list()), "no .* model")
  expect_error(run_regression(nested_test_df, model = x), "not .* supported model")
  expect_error(run_regression(nested_test_df, model = nls(y ~ x)), "not .* supported model")
  expect_error(run_regression(nested_test_df, model = lm(y ~ x), model_data = name), "not .* correct column type")
  expect_error(run_regression(nested_test_df, model = lm(y ~ DNE)), "not all variables exist")
  expect_error(run_regression(nested_test_df, model = lm(y + x ~ x)), "multiple dependent.*variables")

  # single model
  expect_s3_class(df_w_models <- nested_test_df %>% run_regression(model = lm(y ~ x)), "tbl")
  expect_equal(nrow(df_w_models), 2L)
  expect_equal(df_w_models$model_fit[[1]]$residuals %>% length(), filter(test_df, name == "a") %>% nrow())
  expect_equal(names(df_w_models), c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "model_fit", "model_coefs", "model_summary"))
  expect_equal(names(df_w_coefs <- unnest(df_w_models, model_coefs)),
               c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "model_fit", "term", "estimate", "std.error", "statistic", "p.value", "signif", "model_summary"))
  expect_equal(nrow(df_w_coefs), 2*2)
  expect_true(all(df_w_coefs$term %in% c("(Intercept)", "x")))
  expect_equal( # columns
    df_w_models %>% select(name, model_data) %>% unnest(model_data) %>% names(),
    c("name", "x", "y", "z", "in_reg", "residual")
  )
  expect_equal( # units
    df_w_models %>% select(name, model_data) %>% unnest(model_data) %>% iso_get_units(),
    c(name = NA, x = NA, y = NA, z = "z", in_reg = NA, residual = NA)
  )
  expect_equal( # units in residuals
    nested_test_df %>% run_regression(model = lm(z ~ x)) %>% select(name, model_data) %>% unnest(model_data) %>% iso_get_units(),
    c(name = NA, x = NA, y = NA, z = "z", in_reg = NA, residual = "z")
  )
  expect_equal( # data
    df_w_models %>% select(name, model_data) %>% unnest(model_data) %>% select(-in_reg, -residual) %>% arrange(name, x),
    test_df %>% arrange(name, x)
  )

  # single model nested outcome
  expect_s3_class(df_w_nested_models <- nested_test_df %>% run_regression(model = lm(y ~ x), nest_model = TRUE), "tbl")
  expect_equal(names(df_w_nested_models), c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "model_params"))
  expect_equal(names(unnest(df_w_nested_models, model_params)),
               c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "model_fit", "model_coefs", "model_summary"))
  expect_equal(df_w_nested_models$model_enough_data, c(TRUE, TRUE))
  expect_equal(df_w_nested_models$model_data_points, c(10, 10))

  # single model with filter
  expect_error(nested_test_df %>% run_regression(model = lm(y ~ x), model_filter_condition = DNE < 0.5), class = "dplyr:::mutate_error")
  expect_s3_class(df_w_models <- nested_test_df %>% run_regression(model = lm(y ~ x), model_filter_condition = y < 0.5) , "tbl")
  expect_equal(df_w_models$model_enough_data, c(TRUE, TRUE))
  expect_equal(df_w_models$model_data_points, c(6, 2))
  expect_equal(nrow(df_w_models), 2L)
  expect_equal(df_w_models$model_fit[[1]]$residuals %>% length(), filter(test_df, name == "a", y < 0.5) %>% nrow())
  expect_equal(df_w_models$model_name, c("lm(y ~ x)", "lm(y ~ x)"))

  # single model not enough data
  expect_warning(out <- nested_test_df %>% run_regression(model = lm(y ~ x), model_filter_condition = y < 0), "insufficient degrees of freedom")
  expect_equal(out$model_enough_data, c(FALSE, FALSE))
  expect_equal(out$model_data_points, c(0, 0))

  # multi model
  expect_error(nested_test_df %>% run_regression(list(m1 = lm(y ~ x), m1 = lm(y ~ x*I(x^2)))), "encountered duplicates")
  expect_error(nested_test_df %>% run_regression(list(lm(y ~ x), lm(y ~ x))), "encountered duplicates")
  expect_s3_class(
    df_w_models2 <- nested_test_df %>% run_regression(list(m1 = lm(y ~ x), m2 = lm(y ~ x*I(x^2)), m3 = loess(y ~ x))),
  "tbl")
  expect_equal(nrow(df_w_models2), 6L)
  expect_equal(df_w_models2$name, c("a", "a", "a", "b", "b", "b"))
  expect_equal(df_w_models2$model_name, c("m1", "m2", "m3", "m1", "m2", "m3"))
  expect_equal(names(df_w_coefs2 <- unnest_select_data(df_w_models2, select = term, nested_data = model_coefs)),
               c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "model_fit", "term", "model_coefs", "model_summary"))
  expect_equal(nrow(df_w_coefs2), 2*2 + 2*4)
  expect_equal(filter(df_w_coefs2, model_name == "m1")$term %>% unique(), c("(Intercept)", "x"))
  expect_equal(filter(df_w_coefs2, model_name == "m2")$term %>% unique(), c("(Intercept)", "x", "I(x^2)", "x:I(x^2)"))
  expect_equal(filter(df_w_coefs2, model_name == "m3")$term, character(0))

  # unnest coefficients (unnested model)
  expect_error(unnest_model_column(df_w_models, select = c(term, p.value)), "specify which .* to unnest")
  expect_equal(unnest_model_column(df_w_models, select = c(term, p.value), model_column = model_coefs) %>% names(),
               c("name", "model_name", "model_enough_data", "model_data_points", "term", "p.value"))
  expect_equal(unnest_model_column(df_w_models2, select = c(term, p.value), model_column = model_coefs) %>% names(),
               c("name", "model_name", "model_enough_data", "model_data_points", "term", "p.value"))
  expect_error(unnest_model_column(df_w_models, model_column = DNE), "unknown column")
  expect_equal(df_w_models %>% mutate(my_test = model_coefs) %>%
                 unnest_model_column(select = c(term, p.value, signif), model_column = my_test) %>% names(),
               c("name", "model_name", "model_enough_data", "model_data_points", "term", "p.value", "signif"))

  # unnest coefficients (nested model)
  expect_error(unnest_model_column(df_w_nested_models, model_column = model_coefs, nested_model = TRUE, model_params = DNE),
               "unknown column")
  expect_equal(unnest_model_column(df_w_nested_models, model_column = model_coefs, select = c(term, p.value), nested_model = TRUE) %>% names(),
               c("name", "model_name", "model_enough_data", "model_data_points", "term", "p.value"))
  expect_equal(unnest_model_column(df_w_nested_models, model_column = model_coefs, select = c(term, p.value), nested_model = TRUE, keep_other_list_data = TRUE) %>% names(),
               c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "term", "p.value", "model_params"))

  # unnest summary
  expect_equal(unnest_model_column(df_w_models, select = c(r.squared, p.value, sigma), model_column = model_summary) %>% names(),
               c("name", "model_name", "model_enough_data", "model_data_points", "r.squared", "p.value", "sigma"))
  expect_equal(unnest_model_column(df_w_nested_models, select = c(r.squared, p.value, sigma), model_column = model_summary, nested_model = TRUE) %>% names(),
               c("name", "model_name", "model_enough_data", "model_data_points", "r.squared", "p.value", "sigma"))
  expect_equal(unnest_model_column(df_w_nested_models, select = c(r.squared, p.value, "sigma"), model_column = model_summary, nested_model = TRUE, keep_other_list_data = TRUE) %>% names(),
               c("name", "model_data", "model_name", "model_enough_data", "model_data_points", "r.squared", "p.value", "sigma", "model_params"))

  # unnest summary for multiple models
  expect_equal(nrow(m_sum <- unnest_model_column(df_w_models2, select = c(r.squared, p.value, sigma), model_column = model_summary)), 6L)
  expect_equal(round(m_sum$r.squared, 3), c(0.131, 0.29, NA, 0.013, 0.132, NA))
  expect_equal(round(m_sum$p.value, 3), c(0.304, 0.529, NA, 0.752, 0.821, NA))
  expect_equal(round(m_sum$sigma, 3), c(0.378, 0.394, 0.408, 0.299, 0.324, 0.347))

  # grouped regressions
  expect_s3_class(df_w_models3 <- test_df %>%
                    run_grouped_regression(group_by = name,
                                           model = list(m1 = lm(y~x)),
                                           model_data = test1,
                                           model_name = test2,
                                           model_enough_data = test3,
                                           model_data_points = test4,
                                           model_fit = test5,
                                           model_coefs = test6,
                                           model_summary = test7) , "tbl")
  expect_equal(names(df_w_models3), c("name", "test1", "test2", "test3", "test4", "test5", "test6", "test7"))
})

# applying regressions (both for dependent and independent variables) =====

test_that("applying regressions work properly", {

  # parameter errors
  expect_error(apply_regression(), "no data table supplied")
  expect_error(apply_regression(tibble()), "unknown column")
  expect_error(apply_regression(tibble(model_name = "test", model_data = TRUE, model_fit = TRUE)),
               "not.*correct column type")
  expect_error(apply_regression(tibble(), nested_model = TRUE), "unknown column")
  expect_error(apply_regression(tibble(model_name = "test", model_data = TRUE, model_enough_data = TRUE, model_params = TRUE), nested_model = TRUE),
               "not.*correct column type")
  expect_error(apply_regression(tibble(model_name = "test", model_data = list(), model_params = TRUE), nested_model = TRUE),
               "not.*correct column type")
  expect_error(apply_regression(tibble(model_name = "test", model_data = list(), model_params = list()), nested_model = TRUE),
               "unknown column")
  expect_error(apply_regression(
    tibble(model_name = "test", model_data = list(42),
           model_params = list(tibble(model_fit = TRUE))), nested_model = TRUE),
    "not.*correct column type")

  # sample data set
  set.seed(42)
  test_df <- tidyr::crossing(
    x1 = seq(1,10, length.out = 5),
    x2 = iso_double_with_units(c(0.1, 0.5, 1), "x"),
    x3 = seq(0, 0.1, length.out = 4)
  ) %>%
    mutate(
      name = c("a", "a", "b", "a", sample(c("a", "b"), replace = TRUE, size = dplyr::n() - 4)),
      use_for_calib = c(rep(FALSE, 4), sample(c(TRUE, FALSE), replace = TRUE, size = dplyr::n() - 4)),
      y = -1 + 5 * x1 + x1*as.numeric(x2)*20 + 50 * sqrt(as.numeric(x2)) + x3 + rnorm(length(x1), sd = 15),
      # special case: missing data
      x2 = { x2[row_number() <= 3] <- NA; x2 },
      y = ifelse(row_number() %in% c(4), -1e10, y) %>% iso_double_with_units("y")
    )
  nested_test_df <- nest_data(test_df, name, nested_data = model_data)
  models <- quo(list(m1 = lm(y ~ x1), m2 = lm(y ~ x1 + I(sqrt(x2)) + x1:x2 + x3), m3 = loess(y ~ x1, span = 0.75)))
  expect_warning(
    df_w_models <- nested_test_df %>% run_regression(!!models, model_filter_condition = use_for_calib),
    "singularities" # because of the loess for case a with the big outlier at y[1]
  )
  expect_warning(
    df_w_nested_models <- nested_test_df %>% run_regression(!!models, nest_model = TRUE, model_filter_condition = use_for_calib),
    "singularities" # because of the loess for case a with the big outlier at y[1]
  )

  # expected errors from regression application
  expect_error(apply_regression(df_w_models, predict = DNE), "not a variable.*m1, m2")
  expect_error(apply_regression(df_w_models, predict = x2), "not a variable.*m1")
  expect_error(apply_regression(df_w_models, predict = x1), "cannot be inverted")
  expect_error(nested_test_df %>% run_regression(lm(y + x1 ~ x2), model_filter_condition = use_for_calib) %>% apply_regression(x2),
               "multiple dependent.*not supported")

  # out of range troubles for the different data sets and models
  expect_warning(
    out_x2 <- df_w_models %>% filter(model_name == "m2") %>% apply_regression(x2, predict_range = c(-10, -9.999)),
    "potential fit is too far outside the calibration range"
  )

  # direct and nested regressions
  expect_silent(out_direct_y1 <- df_w_models %>% apply_regression(y))
  expect_silent(out_direct_y1_w_error <- df_w_models %>% filter(model_name != "m3") %>% apply_regression(y, calculate_error = TRUE))
  expect_silent(out_nested_y1 <- df_w_nested_models %>% apply_regression(y, nested_model = TRUE))
  expect_silent(out_direct_x1 <- df_w_models %>% filter(name == "b", model_name == "m1") %>% apply_regression(x1))
  expect_silent(out_nested_x1 <- df_w_nested_models %>% filter(name == "b", model_name == "m1") %>%
                   apply_regression(x1, nested_model = TRUE))

  # check return columns
  expect_equal(names(out_direct_x1),
               c("name", "model_data", "model_name", "model_enough_data",
                "model_data_points", "model_fit", "model_coefs", "model_summary"))
  expect_equal(names(out_direct_x1), names(out_direct_y1))
  expect_equal(names(out_nested_x1),
               c("name", "model_data", "model_name", "model_enough_data",
                 "model_data_points", "model_params"))
  expect_equal(names(out_nested_x1), names(out_nested_y1))

  # check nested and unnested equivalency
  expect_equal(out_direct_x1$model_data[[1]], out_nested_x1$model_data[[1]])

  # check for new columns in the data frame
  base_cols <- c("x1", "x2", "x3", "use_for_calib", "y", "in_reg", "residual")
  expect_equal(names(out_direct_y1$model_data[[1]]), c(base_cols, "pred", "pred_se"))
  expect_equal(names(out_direct_x1$model_data[[1]]), c(base_cols, "pred", "pred_se"))
  expect_equal(names(out_x2$model_data[[1]]), c(base_cols, "pred", "pred_se"))
  expect_equal(
    tidyr::unnest(select(out_direct_y1, model_data), model_data) %>% iso_get_units(),
    c(x1 = NA, x2 = "x", x3 = NA, use_for_calib = NA, y = "y", in_reg = NA, residual = "y", pred = "y", pred_se = "y")
  )
  expect_equal(
    tidyr::unnest(select(out_direct_x1, model_data), model_data) %>% iso_get_units(),
    c(x1 = NA, x2 = "x", x3 = NA, use_for_calib = NA, y = "y", in_reg = NA, residual = "y", pred = NA, pred_se = NA)
  )
  expect_equal(
    tidyr::unnest(select(out_x2, model_data), model_data) %>% iso_get_units(),
    c(x1 = NA, x2 = "x", x3 = NA, use_for_calib = NA, y = "y", in_reg = NA, residual = "y", pred = "x", pred_se = "x")
  )
  expect_equal(
    df_w_models %>% filter(name == "b", model_name == "m1") %>%
      apply_regression(x1, calculate_error = TRUE) %>%
      { .$model_data[[1]] } %>% names(),
    c(base_cols, "pred", "pred_se")
  )
  expect_false(
    df_w_models %>% filter(name == "b", model_name == "m1") %>%
      apply_regression(x1, calculate_error = TRUE) %>%
      { .$model_data[[1]]$pred_se } %>% is.na() %>% all())

  # custom names
  expect_equal(
    df_w_models %>% filter(name == "b", model_name == "m1") %>%
      apply_regression(x1, calculate_error = TRUE, predict_value = value, predict_error = error) %>%
      { .$model_data[[1]] } %>% names(),
    c(base_cols, "value", "error")
  )

  # check for which values are NA
  expect_equal(out_direct_y1 %>% filter(model_name == "m1") %>% unnest(model_data) %>% filter(is.na(pred)) %>% nrow(), 0L)
  expect_equal(
    out_direct_y1 %>% filter(model_name == "m2") %>% unnest(model_data) %>% filter(is.na(pred)) %>% dplyr::pull(x2),
    iso_double_with_units(c(NA, NA, NA), "x")
  )

  # check for expected predicted values
  expect_true(
    all(with(filter(unnest(out_direct_y1, model_data), in_reg), abs(as.numeric(y) - as.numeric(residual) - as.numeric(pred))) < 1e10)
  )

})

# evaluating ranges ======

test_that("test that range evaluation works", {

  # parameter errors
  expect_error(evaluate_range(), "no data table supplied")
  expect_error(evaluate_range(tibble()), "no terms")
  expect_error(evaluate_range(tibble(), x), "unknown column")

  # testing ranges
  set.seed(42)
  test_df <- dplyr::tibble(name = rep(c("a", "b"), 10), x = runif(20), y = iso_double_with_units(runif(20), "y"))
  nested_test_df <- nest_data(test_df, name, nested_data = model_data)
  df_w_models <- nested_test_df %>% run_regression(model = lm(y ~ x))
  df_w_nested_models <- nested_test_df %>% run_regression(model = lm(y ~ x), nest_model = TRUE)

  # term errors
  expect_error(evaluate_range(df_w_models, DNE), "not all.*terms are valid", class = "dplyr:::mutate_error")

  # range evaluation columns
  expect_is(df_w_models_ranges <- df_w_models %>% evaluate_range(x, y), "tbl")
  expect_equal(names(df_w_models_ranges), c(names(df_w_models), "model_range"))
  expect_is(df_w_nested_models_ranges <- df_w_nested_models %>% evaluate_range(x, y, nested_model = TRUE), "tbl")
  expect_equal(names(df_w_nested_models_ranges), names(df_w_nested_models))
  expect_equal(names(unnest(df_w_nested_models_ranges, model_params)),
               c(names(unnest(df_w_nested_models, model_params)), "model_range"))
  expect_true(setequal(
    names(unnest(df_w_models_ranges, model_data)),
    c(names(unnest(df_w_models, model_data)), "in_range", "model_range"))
  )
  expect_true(setequal(
    names(unnest(df_w_nested_models_ranges, model_data)),
    c(names(unnest(df_w_nested_models, model_data)), "in_range"))
  )
  expect_equal(unnest(df_w_models_ranges, model_data)$in_range %>% unique(), "in range")
  expect_equal(unnest(df_w_nested_models_ranges, model_data)$in_range %>% unique(), "in range")
  expect_equal(
    unnest(df_w_models_ranges, model_range) %>% select(name, term, units, min, max) %>% arrange(name, term),
    bind_rows(
      test_df %>% group_by(name) %>% summarize(term = "x", units = NA, min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)),
      test_df %>% group_by(name) %>% summarize(term = "y", units = "y", min = min(as.numeric(y), na.rm = TRUE), max = max(as.numeric(y), na.rm = TRUE))
    ) %>% arrange(name, term)
  )

  # different range scenario
  expect_warning(
    df_w_models2 <- nested_test_df %>% run_regression(model = lm(y ~ x), model_filter_condition = y < 0.5, min_n_datapoints = 3),
    "insufficient degrees of freedom"
  )
  expect_is(df_w_models_ranges2 <- df_w_models2 %>%
              evaluate_range(x, y, x*y, model_range = my_range, in_range = my_in_range), "tbl")
  expect_equal(names(df_w_models_ranges2), c(names(df_w_models2), "my_range"))
  expect_true(setequal(
    names(unnest(df_w_models_ranges2, model_data)),
    c(names(unnest(df_w_models2, model_data)), "my_in_range", "my_range"))
  )
  expect_equal(
    unnest(df_w_models_ranges2, model_data)$my_in_range %>% unique(),
    # NOTE: this test could fail if the set.seed does not behave the same way on the server
    c(">'y' range, >'x * y' range", "<'x' range, >'y' range", "in range",
      "'x' range NA, 'y' range NA, 'x * y' range NA")
  )
  expect_equal(
    unnest(df_w_models_ranges2, my_range) %>% select(name, term, units, min, max) %>% arrange(name, term),
    bind_rows(
      test_df %>% filter(name == "a", y < 0.5) %>%
        {
          bind_rows(
            summarize(., term = "x", units = NA, min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)),
            summarize(., term = "y", units = "y", min = min(as.numeric(y), na.rm = TRUE), max = max(as.numeric(y), na.rm = TRUE)),
            summarize(., term = "x * y", units = "y", min = min(x*as.numeric(y), na.rm = TRUE), max = max(x*as.numeric(y), na.rm = TRUE))
          )
        } %>%
        mutate(name = "a"),
      tibble(name = "b", term = c("x", "y", "x * y"))
    ) %>% select(name, everything()) %>% arrange(name, term)
  )

})

# example files =====

test_that("example files are accessible", {

  expect_equal(
    iso_get_processor_examples()$filename,
    c("ea_irms_example_carbon.cf.rds","gc_irms_example_carbon.cf.rds")
  )

  expect_error(iso_get_processor_example(), "missing")
  expect_error(iso_get_processor_example("DNE"), "file.*does not exist")


})
