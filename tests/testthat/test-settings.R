context("Settings and default values")

test_that("default values can be set and retrieved", {
  expect_error(isoprocessorCUB:::setting("don't know"), "setting .* does not exist")
  expect_error(isoprocessorCUB:::setting(x^2), "not .* valid setting name")
  expect_error(isoprocessorCUB:::setting(DNE), "setting .* does not exist")
})

test_that("info messages can be turned on and off", {
  expect_message(turn_info_messages_on(), "messages turned on")
  expect_false(isoprocessorCUB:::setting("quiet"))
  expect_false(isoprocessorCUB:::setting(quiet))
  expect_silent(turn_info_messages_off())
  expect_true(isoprocessorCUB:::setting("quiet"))
  expect_true(isoprocessorCUB:::setting(quiet))
})


test_that("default parameters work properly", {

  # set and reset default parameters
  expect_equal(set_default_parameters(42, a = x, b = 5, c = NULL), 42)
  expect_equal(default(a), quo(x))
  expect_equal(default(b), quo(5))
  expect_equal(eval_tidy(default(b)), 5)
  expect_equal(eval_tidy(default(c)), NULL)

  # resolve defaults in a list of quos
  expect_equal(resolve_defaults(default(a)), quo(x))
  expect_equal(resolve_defaults(list(default(a), quo(y))), list(quo(x), quo(y)))

  # reset and default of default
  expect_equal(reset_default_parameters(42), 42)
  expect_equal(default(a), quo(a))

})
