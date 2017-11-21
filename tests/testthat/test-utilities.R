context("Utilities and other convenience functions")

test_that("text to numeric conversions work", {

  # extract single matching group
  expect_equal(isoprocessorCUB::str_extract_group(c("abc123def", "ABC456DEF"), "(\\d+)(\\w+)", 1),
            c("123", "456"))
  expect_equal(isoprocessorCUB::str_extract_group(c("abc123def", "ABC456DEF"), "(\\d+)(\\w+)", 2),
               c("def", "DEF"))

  # cleanup numeric
  expect_error(cleanup_numeric(), "x not supplied")
  expect_equal(cleanup_numeric(1:5), 1:5)
  expect_error(cleanup_numeric(data.frame(x=1:5)), "not a character vector")

  # test numbers
  numbers <- c("abc42", "abc42def", "abc4.2def", "abc-4.2def", "42def", "4.2")
  expect_equal(numbers %>% cleanup_numeric(), c(42, 42, 4.2, -4.2, 42, 4.2))
  expect_equal(numbers %>% cleanup_numeric(allow_negative = FALSE), c(42, 42, 4.2, 4.2, 42, 4.2))
  expect_equal(numbers %>% cleanup_numeric(trim_text_prefix = FALSE), c(NA, NA, NA, NA, 42, 4.2))
  expect_equal(numbers %>% cleanup_numeric(trim_text_suffix = FALSE), c(42, NA, NA, NA, NA, 4.2))
  expect_equal(numbers %>% cleanup_numeric(trim_text_prefix = FALSE, trim_text_suffix = FALSE), c(NA, NA, NA, NA, NA, 4.2))

})

test_that("nesting and unnesting functions work properly", {

  # testing the nest data function
  test_df <- dplyr::data_frame(x=1:5, z = "text", y =c("a", "a", "b", "a", "b"))
  expect_error(nest_data(), "no data table supplied")
  expect_error(nest_data(test_df, DNE), "refers to invalid column")
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
  expect_error(unnest_select_data(nested_df, nested_data = my_nested), "refers to invalid column")
  expect_error(unnest_select_data(nested_df, nested_data = y), "not .* correct column type")
  expect_error(unnest_select_data(nested_df, select = DNE), "refers to invalid column")
  expect_equal(unnest_select_data(nested_df, select = c()) %>% names(), nested_df %>% names())
  expect_equal(unnest_select_data(nested_df, z) %>% names(), c("y", "z", "nested_data"))
  expect_equal(unnest_select_data(nested_df, "z") %>% names(), c("y", "z", "nested_data"))
  expect_equal(unnest_select_data(nested_df, x) %>% names(), c("y", "x", "nested_data"))
  expect_equal(unnest_select_data(nested_df, select = everything()) %>% names(), c("y", "x", "z"))

  # introduce more nested column
  mutated_nested_df <- mutate(nested_df, test_data = purrr::map(nested_data, ~dplyr::rename(.x, x2=x, z2=z)))
  expect_equal(unnest_select_data(mutated_nested_df, z) %>% names(), c("y", "z", "nested_data", "test_data"))
  expect_error(unnest_select_data(nested_df, z, nested = test_data), "refers to invalid column")
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data) %>% names(), c("y", "z2", "test_data", "nested_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_other_list_data = FALSE) %>% names(), c("y", "z2", "test_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_remaining_nested_data = FALSE) %>% names(), c("y", "z2", "nested_data"))
  expect_equal(unnest_select_data(mutated_nested_df, z2, nested = test_data, keep_other_list_data = FALSE, keep_remaining_nested_data = FALSE) %>% names(), c("y", "z2"))

})

test_that("regression functions work properly", {

  test_df <- dplyr::data_frame(name = rep(c("a", "b"), 10), x = runif(20), y = runif(20))
  nested_test_df <- nest_data(test_df, name, nested_data = model_data)
  expect_error(run_regression(), "no data table supplied")
  expect_error(run_regression(test_df), "model_data .* invalid column")
  expect_error(run_regression(nested_test_df), "no .* model")
  expect_error(run_regression(nested_test_df, model = list()), "no .* model")
  expect_error(run_regression(nested_test_df, model = x), "not .* valid model")
  expect_error(run_regression(nested_test_df, model = lm(y ~ x), model_data = name), "not .* correct column type")

  # single model
  expect_s3_class(df_w_models <- nested_test_df %>% run_regression(model = lm(y ~ x)) , "tbl")
  expect_equal(nrow(df_w_models), 2L)
  expect_equal(names(df_w_models), c("name", "model_data", "model_fit", "model_coefs", "model_summary"))
  expect_equal(names(df_w_coefs <- unnest(df_w_models, model_coefs)),
               c("name", "term", "estimate", "std.error", "statistic", "p.value", "signif"))
  expect_equal(nrow(df_w_coefs), 2*2)
  expect_true(all(df_w_coefs$term %in% c("(Intercept)", "x")))

  # multi model
  expect_error(nested_test_df %>% run_regression(list(m1 = lm(y ~ x), m1 = lm(y ~ x*I(x^2)))), "encountered duplicate name")
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
  expect_equal(unnest_model_coefficients(df_w_models, select = c(term, p.value)) %>% names(),
               c("name", "term", "p.value"))
  expect_equal(unnest_model_coefficients(df_w_models2, select = c(term, p.value)) %>% names(),
               c("name", "model_name", "term", "p.value"))
  expect_error(unnest_model_coefficients(df_w_models, model_coefs = DNE), "invalid column")
  expect_equal(df_w_models %>% mutate(my_test = model_coefs) %>%
                 unnest_model_coefficients(select = c(term, p.value, signif), model_coefs = my_test) %>% names(),
               c("name", "term", "p.value", "signif"))

  # unnest summary
  expect_equal(unnest_model_summary(df_w_models, select = c(r.squared, p.value)) %>% names(),
               c("name", "r.squared", "p.value"))

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
