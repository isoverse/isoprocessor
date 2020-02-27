context("Visualization Utils")

# raw data ========

context("Plotting functions")

test_that("test that raw data plot throws appropriate errors", {
  expect_error(iso_plot_raw_data(42), "can only plot iso files")
})

test_that("test that cf plot data prep works properly", {

  expect_error(iso_prepare_continuous_flow_plot_data())
  expect_error(iso_prepare_continuous_flow_plot_data(c(isoreader:::make_di_data_structure("NA"))), "can only prepare continuous flow")

  # FIXME: include more elaborate tests here

})

# continuous flow plot =====

test_that("test that plot continuous flow works properly", {

  expect_error(iso_plot_continuous_flow_data(42), "not defined")
  expect_is(cf <- isoreader:::make_cf_data_structure("NA"), "continuous_flow")
  cf$read_options$file_info <- TRUE
  cf$read_options$raw_data <- TRUE
  expect_error(iso_plot_continuous_flow_data(cf), "no raw data in supplied iso_files")
  expect_error(iso_plot_continuous_flow_data(c(cf)), "no raw data in supplied iso_files")

  # make test raw data
  cf$raw_data <- tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10))

  # test for errors
  expect_error(iso_plot_raw_data(cf, c("42")), "not available in the provided iso_files")
  expect_error(cf %>% {.$raw_data$time.min = 1:10; .} %>% iso_plot_raw_data(.), "unclear which column is the time column")
  expect_error(iso_plot_raw_data(cf, time_interval = 55), "time interval needs to be a vector with two numeric entries")
  expect_error(iso_plot_raw_data(cf, panel = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(cf, color = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(cf, linetype = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(cf, label = DNE), "not.*valid")

  # generate plot
  cf <- iso_calculate_ratios(cf, "46/44")
  expect_message(p <- iso_plot_raw_data(cf, c("46/44", "44"), quiet = FALSE), "plotting data")
  expect_silent(iso_plot_raw_data(cf, "44", quiet = TRUE))
  expect_silent(p <- iso_plot_continuous_flow_data(cf, c("46/44", "44")))
  expect_true(is.ggplot(p))

  expect_true(all(p$data$data %in% c("44 [mV]", "46/44"))) # only these datas selected
  expect_true(identical(p$data$data %>% levels(), c("46/44", "44 [mV]")))

  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "label")))
  expect_true("file_id" %in% rlang::as_label(p$mapping$colour))
  expect_true("time.s" %in% rlang::as_label(p$mapping$x))
  expect_true("value" %in% rlang::as_label(p$mapping$y))
  expect_true("file_id" %in% rlang::as_label(p$mapping$label))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "..panel") # always ..panel because expressions not possible
  expect_equal(names(p$facet$params$cols) %>% length(), 0)

  # then custom specifications
  expect_true(is.ggplot(p <- iso_plot_raw_data(cf, panel = NULL, color = data, linetype = file_id)))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]", "46/44"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype", "label")))
  expect_true("data" %in% rlang::as_label(p$mapping$colour))
  expect_true("file_id" %in% rlang::as_label(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- iso_plot_raw_data(cf, "44", panel = file_id, color = NULL, linetype = data)))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "linetype", "label")))
  expect_true("data" %in% rlang::as_label(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "..panel") # always ..panel because expressions not possible
  expect_equal(names(p$facet$params$cols) %>% length(), 0)

  # FIXME: implemment proper tests for visualization with peak_table

})

# dual inlet plot ====

test_that("test that plot dual inlet works properly", {

  expect_error(iso_plot_dual_inlet_data(42), "not defined")
  expect_is(di <- isoreader:::make_di_data_structure("NA"), "dual_inlet")
  di$read_options$file_info <- TRUE
  di$read_options$raw_data <- TRUE
  expect_error(iso_plot_raw_data(di), "no raw data in supplied iso_files")

  # make test raw data
  di$raw_data <- data_frame(type = rep(c("standard", "sample"), each = 5), cycle = rep(1:5, times = 2), v44.mV = runif(10), v46.mV = runif(10))

  # test for errors
  expect_error(iso_plot_dual_inlet_data(di, data = 45), "data not available")
  expect_error(iso_plot_dual_inlet_data(di, filter = FALSE), "no data left with filter")

  # generate plot
  di <- iso_calculate_ratios(di, "46/44")
  expect_silent(p <- iso_plot_dual_inlet_data(di, c("46/44", "44")))
  expect_true(is.ggplot(p))
  expect_true(all(p$data$data %in% c("44 [mV]", "46/44"))) # only these datas selected
  expect_true(identical(p$data$data %>% levels(), c("46/44", "44 [mV]")))

  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "shape", "label")))
  expect_true("file_id" %in% rlang::as_label(p$mapping$colour))
  expect_true("cycle" %in% rlang::as_label(p$mapping$x))
  expect_true("value" %in% rlang::as_label(p$mapping$y))
  expect_true("type" %in% rlang::as_label(p$mapping$shape))
  expect_true("file_id" %in% rlang::as_label(p$mapping$label))
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "data")

  # then custom specifications
  expect_true(is.ggplot(p <- iso_plot_raw_data(di, panel = NULL, color = data, linetype = file_id, shape = NULL)))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]", "46/44"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype", "label")))
  expect_true("data" %in% rlang::as_label(p$mapping$colour))
  expect_true("file_id" %in% rlang::as_label(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- iso_plot_raw_data(di, "44", panel = file_id, color = type, linetype = data, shape = file_id)))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "colour", "linetype", "shape", "label")))
  expect_true("type" %in% rlang::as_label(p$mapping$colour))
  expect_true("data" %in% rlang::as_label(p$mapping$linetype))
  expect_true("file_id" %in% rlang::as_label(p$mapping$shape))
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "file_id")

  expect_true(is.ggplot(p <- iso_plot_raw_data(di, "44", panel = file_id ~ data)))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "file_id")
  expect_equal(names(p$facet$params$cols), "data")

})


# scan plot ====

test_that("test that plot scan works properly", {

  expect_error(iso_plot_scan_data(42), "not defined")
  expect_is(scan <- isoreader:::make_scan_data_structure("NA"), "scan")
  scan$read_options$file_info <- TRUE
  scan$read_options$raw_data <- TRUE
  expect_error(iso_plot_scan_data(scan), "no raw data in supplied iso_files")

  # make test data
  scan$file_info$type <- "x"
  scan$raw_data <- tibble(step = 1:10, x = step*10, x_units = "unit", v44.mV = runif(10), v46.mV = runif(10))
  scan2 <- scan
  scan2$file_info$file_id <- "NA2"
  scan2$file_info$type <- "y"

  # test for errors
  expect_error(iso_prepare_scan_plot_data(scan, include_file_info = c()), "type.*must be included")
  expect_error(iso_plot_scan_data(scan, type = "y"), "no data for type.*y.*Available.*x")
  expect_error(iso_plot_scan_data(c(scan, scan2)), "found more than 1 type")
  expect_error(iso_plot_scan_data(scan, data = 45), "data not available")
  expect_error(iso_plot_scan_data(scan, x_interval = 5), "x interval needs to be a vector")
  expect_error(iso_plot_scan_data(scan, x_interval = c("a", "b")), "x interval needs to be a vector")
  expect_error(iso_plot_scan_data(scan, y_interval = 5), "y interval needs to be a vector")
  expect_error(iso_plot_scan_data(scan, y_interval = c("a", "b")), "y interval needs to be a vector")
  expect_error(iso_plot_scan_data(scan, filter = FALSE), "no data left with filter")

  # generate plot
  expect_true(is.ggplot(p <- iso_plot_scan_data(scan, data = c("44"))))
  expect_true(all(p$data$data %in% c("44 [mV]")))
  expect_true(identical(p$data$data %>% levels(), "44 [mV]"))

  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "label")))
  expect_true("data" %in% rlang::as_label(p$mapping$colour))
  expect_true("x" %in% rlang::as_label(p$mapping$x))
  expect_true("value" %in% rlang::as_label(p$mapping$y))
  expect_true("data" %in% rlang::as_label(p$mapping$label))
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "file_id")

  # then custom specifications
  expect_true(is.ggplot(p <- iso_plot_scan_data(scan, panel = NULL, color = data, linetype = file_id)))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype", "label")))
  expect_true("data" %in% rlang::as_label(p$mapping$colour))
  expect_true("file_id" %in% rlang::as_label(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- iso_plot_scan_data(scan, data = "44", panel = file_id, color = type, linetype = data, shape = file_id)))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "colour", "linetype", "shape", "label")))
  expect_true("type" %in% rlang::as_label(p$mapping$colour))
  expect_true("data" %in% rlang::as_label(p$mapping$linetype))
  expect_true("file_id" %in% rlang::as_label(p$mapping$shape))
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "file_id")

  expect_true(is.ggplot(p <- iso_plot_raw_data(scan, data = "44", panel = file_id ~ data)))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "file_id")
  expect_equal(names(p$facet$params$cols), "data")

})

# ref peaks =========

test_that("test that referencd peak visualization works", {

  expect_error(iso_plot_ref_peaks(), "no data table")
  expect_error(iso_plot_ref_peaks(tibble()), "missing parameter.*x")
  expect_error(iso_plot_ref_peaks(tibble(x=1:5), x), "missing parameter.*ratio")
  expect_error(iso_plot_ref_peaks(tibble(x=1:5, y=1:5), x, ratio = y), "file_id.*unknown column")
  expect_error(iso_plot_ref_peaks(tibble(x=1:5, y=1:5, file_id="a"), x, ratio = y, is_ref_condition = FALSE), "no data")

  # simple generation tests
  expect_true((p <- iso_plot_ref_peaks(tibble(x=1:5, y=1:5, file_id="a"), x, ratio = c(z = y))) %>% ggplot2::is.ggplot())
  expect_true("x" %in% rlang::as_label(p$mapping$x))
  expect_true("z" %in% rlang::as_label(p$mapping$y))
  expect_true((p <- iso_plot_ref_peaks(tibble(x=1:5, y=1:5, z=1:5, file_id="a"), x, ratio = c(y, z))) %>% ggplot2::is.ggplot())
  expect_true("y_value" %in% rlang::as_label(p$mapping$y))

})

# iso_plot_data works ======

test_that("test that iso_plot_data visualization works", {

  expect_error(iso_plot_data(), "no data")
  expect_error(iso_plot_data(tibble()), "no data")
  expect_error(iso_plot_data(ggplot2::mpg), "provide.*x")
  expect_error(iso_plot_data(ggplot2::mpg, cyl), "provide.*y")
  expect_warning(iso_plot_data(ggplot2::mpg, cyl, hwy), "plot will be blank")
  expect_warning(iso_plot_data(ggplot2::mpg, cyl, hwy, 42), "ignoring unrecognized parameter")
  expect_warning(iso_plot_data(ggplot2::mpg, cyl, hwy, DNE = 42), "ignoring unrecognized parameter")
  expect_silent(iso_plot_data(ggplot2::mpg, cyl, hwy, geom_point()))
  expect_silent(iso_plot_data(ggplot2::mpg, cyl, hwy, add = geom_point()))


  # @FIXME implement full suite of visualization function test

})

test_that("calibration ranges work", {

  expect_warning(iso_plot_calibration_range(), "deprecated")

})

# x range ======

test_that("marking x range works", {

  p <- ggplot2::ggplot(ggplot2::mpg) + ggplot2::aes(hwy, cty) + ggplot2::geom_point()
  expect_error(iso_mark_x_range(), "no base plot")
  expect_error(iso_mark_x_range(p), "condition.*required")

  # @FIXME: implement more

})

# value range ====

test_that("marking value range works", {

  p <- ggplot2::ggplot(ggplot2::mpg) + ggplot2::aes(hwy, cty) + ggplot2::geom_point()
  expect_error(iso_mark_value_range(), "no base plot")
  expect_warning(iso_mark_value_range(p, sd = 1), "renamed")

})

# outliers =====

test_that("marking outliers works", {

  p <- ggplot2::ggplot(ggplot2::mpg) + ggplot2::aes(hwy, cty) + ggplot2::geom_point()
  expect_error(iso_mark_outliers(), "no base plot")
  expect_error(iso_mark_outliers(p), "no cutoff provided")
  expect_error(iso_mark_outliers(p, plus_minus_value = 5, plus_minus_sd = 5), "more than one")
  expect_error(iso_mark_outliers(p, condition = TRUE, plus_minus_sd = 5), "more than one")
  expect_error(iso_mark_outliers(p, plus_minus_value = 5, condition = TRUE), "more than one")
  expect_true(ggplot2::is.ggplot(p <- iso_mark_outliers(p, condition = TRUE)))

  # @FIXME: implement more
})


