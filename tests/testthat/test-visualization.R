context("Visualization")

# ref peaks =========

test_that("test that referencd peak visualization works", {

  expect_error(iso_plot_ref_peaks(), "no data table")
  expect_error(iso_plot_ref_peaks(tibble()), "no condition.*reference peak")
  expect_error(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE), "missing parameter.*ratio.*group_id.*")
  expect_error(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE, ratio = displ, group_id = DNE), "group_id.*unknown column")
  expect_error(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = cyl > 100, ratio = displ, group_id = model), "no data")

  # simple generation tests
  expect_true((p <- iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE, ratio = displ, group_id = model)) %>% ggplot2::is.ggplot())
  expect_true("ref_peak_nr" %in% as.character(p$mapping$fill))
  expect_true("model" %in% as.character(p$mapping$x))
  expect_true("total_delta_deviation" %in% as.character(p$mapping$y))
  expect_true(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE, ratio = c(displ, hwy), group_id = model) %>% ggplot2::is.ggplot())
  expect_true(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = cyl > 6, ratio = c(displ, hwy), group_id = model) %>% ggplot2::is.ggplot())

  # FIXME: test more (evaluate the resulting plots in more detail?)

})

test_that("visualization works", {

  # @FIXME implement visualization function test

})
