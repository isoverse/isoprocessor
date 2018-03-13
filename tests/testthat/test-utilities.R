context("Utilities and other convenience functions")

test_that("nesting and unnesting functions work properly", {

  # testing the nest data function
  test_df <- dplyr::data_frame(x=1:5, z = "text", y =c("a", "a", "b", "a", "b"))
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
  expect_equal(unnest_select_data(nested_df, x) %>% nrow(), nrow(test_df))
  expect_equal(unnest_select_data(nested_df, select = everything()) %>% names(), c("y", "x", "z"))
  expect_equal(unnest_select_data(nested_df) %>% nrow(), nrow(test_df))

  # making sure unnest can deal with <NULL> columns
  null_nested_df <- nested_df %>% mutate(nested_data = map2(nested_data, row_number(), ~if(.y == 2) {NULL} else {.x}))
  expect_equal(unnest_select_data(null_nested_df, select = c()) %>% names(), nested_df %>% names())
  expect_equal(unnest_select_data(null_nested_df) %>% nrow(), nrow(filter(test_df, y == "a")))

  # introduce more nested column
  mutated_nested_df <- mutate(nested_df, test_data = purrr::map(nested_data, ~dplyr::rename(.x, x2=x, z2=z)))
  expect_equal(unnest_select_data(mutated_nested_df, z) %>% names(), c("y", "z", "nested_data", "test_data"))
  expect_error(unnest_select_data(nested_df, z, nested = test_data), "refers to unknown column")
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data) %>% names(), c("y", "z2", "test_data", "nested_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_other_list_data = FALSE) %>% names(), c("y", "z2", "test_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_remaining_nested_data = FALSE) %>% names(), c("y", "z2", "nested_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_other_list_data = FALSE, keep_remaining_nested_data = FALSE) %>% names(), c("y", "z2"))

})

test_that("regression functions work properly", {

  set.seed(42)
  test_df <- dplyr::data_frame(name = rep(c("a", "b"), 10), x = runif(20), y = runif(20))
  nested_test_df <- nest_data(test_df, name, nested_data = model_data)
  expect_error(run_regression(), "no data table supplied")
  expect_error(run_regression(test_df), "model_data.* unknown column")
  expect_error(run_regression(nested_test_df), "no .* model")
  expect_error(run_regression(nested_test_df, model = list()), "no .* model")
  expect_error(run_regression(nested_test_df, model = x), "not .* valid model")
  expect_error(run_regression(nested_test_df, model = lm(y ~ x), model_data = name), "not .* correct column type")

  # single model
  expect_s3_class(df_w_models <- nested_test_df %>% run_regression(model = lm(y ~ x)) , "tbl")
  expect_equal(nrow(df_w_models), 2L)
  expect_equal(df_w_models$model_fit[[1]]$residuals %>% length(), filter(test_df, name == "a") %>% nrow())
  expect_equal(names(df_w_models), c("name", "model_data", "model_name", "model_fit", "model_coefs", "model_summary"))
  expect_equal(names(df_w_coefs <- unnest(df_w_models, model_coefs)),
               c("name", "model_name", "term", "estimate", "std.error", "statistic", "p.value", "signif"))
  expect_equal(nrow(df_w_coefs), 2*2)
  expect_true(all(df_w_coefs$term %in% c("(Intercept)", "x")))

  # single model with filter
  expect_s3_class(df_w_models <- nested_test_df %>% run_regression(model = lm(y ~ x), model_filter_condition = y < 0.5) , "tbl")
  expect_equal(nrow(df_w_models), 2L)
  expect_equal(df_w_models$model_fit[[1]]$residuals %>% length(), filter(test_df, name == "a", y < 0.5) %>% nrow())
  expect_equal(df_w_models$model_name, c("lm(y ~ x)", "lm(y ~ x)"))

  # multi model
  expect_error(nested_test_df %>% run_regression(list(m1 = lm(y ~ x), m1 = lm(y ~ x*I(x^2)))), "encountered duplicates")
  expect_error(nested_test_df %>% run_regression(list(lm(y ~ x), lm(y ~ x))), "encountered duplicates")
  expect_s3_class(df_w_models2 <- nested_test_df %>% run_regression(list(m1 = lm(y ~ x), m2 = lm(y ~ x*I(x^2)))) , "tbl")
  expect_equal(nrow(df_w_models2), 4L)
  expect_equal(df_w_models2$name, c("a", "b", "a", "b"))
  expect_equal(df_w_models2$model_name, c("m1", "m1", "m2", "m2"))
  expect_equal(names(df_w_coefs2 <- unnest_select_data(df_w_models2, select = term, nested_data = model_coefs)),
               c("name", "model_name", "term", "model_coefs", "model_data", "model_fit", "model_summary"))
  expect_equal(nrow(df_w_coefs2), 2*2 + 2*4)
  expect_equal(filter(df_w_coefs2, model_name == "m1")$term %>% unique(), c("(Intercept)", "x"))
  expect_equal(filter(df_w_coefs2, model_name == "m2")$term %>% unique(), c("(Intercept)", "x", "I(x^2)", "x:I(x^2)"))

  # unnest coefficients
  expect_error(unnest_model_results(df_w_models, select = c(term, p.value)), "specify which .* to unnest")
  expect_equal(unnest_model_results(df_w_models, select = c(term, p.value), model_results = model_coefs) %>% names(),
               c("name", "model_name", "term", "p.value"))
  expect_equal(unnest_model_results(df_w_models2, select = c(term, p.value), model_results = model_coefs) %>% names(),
               c("name", "model_name", "term", "p.value"))
  expect_error(unnest_model_results(df_w_models, model_results = DNE), "unknown column")
  expect_equal(df_w_models %>% mutate(my_test = model_coefs) %>%
                 unnest_model_results(select = c(term, p.value, signif), model_results = my_test) %>% names(),
               c("name", "model_name", "term", "p.value", "signif"))

  # unnest summary
  expect_equal(unnest_model_results(df_w_models, select = c(r.squared, p.value), model_results = model_summary) %>% names(),
               c("name", "model_name", "r.squared", "p.value"))

  # grouped regressions
  expect_s3_class(df_w_models3 <- test_df %>%
                    run_grouped_regression(group_by = name,
                                           model = list(m1 = lm(y~x)),
                                           model_data = test1,
                                           model_name = test2,
                                           model_fit = test3,
                                           model_coefs = test4,
                                           model_summary = test5) , "tbl")
  expect_equal(names(df_w_models3), c("name", "test1", "test2", "test3", "test4", "test5"))
})
