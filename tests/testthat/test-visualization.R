context("Visualization")

# ref peaks =========

test_that("test that referencd peak visualization works", {

  expect_error(iso_plot_ref_peaks(), "no data table")
  expect_error(iso_plot_ref_peaks(data_frame()), "no condition.*reference peak")
  expect_error(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE), "ratio.*group_id.*unknown column")
  expect_error(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE, ratio = displ), "group_id.*unknown column")
  expect_error(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = cyl > 100, ratio = displ, group_id = model), "no data")

  # simple generation tests
  expect_true((p <- iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE, ratio = displ, group_id = model)) %>% ggplot2::is.ggplot())
  expect_equal(p$mapping, list(fill = quo(ref_peak_nr), x = quo(model), y = quo(total_delta_deviation)) %>% { class(.) <- "uneval"; . })
  expect_true(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = TRUE, ratio = c(displ, hwy), group_id = model) %>% ggplot2::is.ggplot())
  expect_true(iso_plot_ref_peaks(ggplot2::mpg, is_ref_condition = cyl > 6, ratio = c(displ, hwy), group_id = model) %>% ggplot2::is.ggplot())

  # FIXME: test more (evaluate the resulting plots in more detail?)

})

test_that("visualization works", {

  # @FIXME implement visualization function test

})
