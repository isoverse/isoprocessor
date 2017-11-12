context("Settings and default values")

test_that("default values can be set and retrieved", {
  expect_error(isoprocessorCUB:::setting("don't know"), "setting .* does not exist")
})

test_that("info messages can be turned on and off", {
  expect_message(turn_info_messages_on(), "messages turned on")
  expect_false(isoprocessorCUB:::setting("quiet"))
  expect_silent(turn_info_messages_off())
  expect_true(isoprocessorCUB:::setting("quiet"))
})
