context("Settings and default values")

test_that("info messages can be turned on and off", {
  # functionality exported from isoreader
  expect_message(iso_turn_info_messages_on(), "messages turned on")
  expect_equal(isoprocessorCUB:::default(), quo())
  expect_false(isoprocessorCUB:::default("quiet"))
  expect_false(isoprocessorCUB:::default(quiet))
  expect_silent(iso_turn_info_messages_off())
  expect_true(isoprocessorCUB:::default("quiet"))
  expect_true(isoprocessorCUB:::default(quiet))
})


test_that("default process parameters work properly", {
  # set and reset default parameters
  isoreader:::initialize_options()
  expect_equal(iso_set_default_process_parameters(42, a = x, b = 5, c = NULL), 42)
  expect_equal(default(a), quo(x))
  expect_equal(default(b), quo(5))
  expect_equal(eval_tidy(default(b)), 5)
  expect_equal(eval_tidy(default(c)), NULL)
  expect_equal(iso_get_default_processor_parameters(),
               data_frame(parameter = c("quiet", "print_func", "a", "b", "c"),
                          value = c("FALSE", "identity", "x", "5", "NULL")))

  # reset and default of default
  expect_equal(iso_reset_default_process_parameters(42), 42)
  expect_equal(default(a), quo(a))
  expect_equal(iso_get_default_processor_parameters(), data_frame(parameter = c("quiet", "print_func"), value = c("FALSE", "identity")))

})
