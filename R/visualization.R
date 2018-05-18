
#' Plot reference peaks
#'
#' Visualize how consistent the reference peaks are across a serious of samples.
#'
#' @param ratio which ratio column(s) to compare for the reference peaks (can be multiple)
#' @param group_id group identifier column(s) to clarify which peaks belongs to a single analysis - first column in group id is used for x axis labels
#' @param is_ref_condition condition to identify which of the peaks are reference peaks. Must be a column or expression that evaluates to a logical (TRUE/FALSE).
#' @param is_ref_used [optional] parameter to identify which of the reference peaks (\code{is_ref_condition}) are actually used for ratio/ratio calculations. Must be a column or expresion that evaluates to a logical (TRUE/FALSE). If this parameter is provided, the used reference peaks are higlight in solid while the other peaks are slightly opaque.
#' @param within_group whether to visualize the deviation within the specified group or across all groups
#' @export
iso_plot_ref_peaks <- function(dt, ratio, group_id, is_ref_condition, is_ref_used = NULL, within_group = FALSE) {

  # safety checks
  param_quos <-
    list(dt = enquo(dt), ratio = enquo(ratio), group_id = enquo(group_id),
         is_ref_condition = enquo(is_ref_condition), is_ref_used = enquo(is_ref_used))

  check_params <-
    c(
      dt = "no data table supplied",
      ratio = "no ratio column to compare reference peaks provided",
      group_id = "no grouping column(s) provided to identify individual analyses",
      is_ref_condition = "no condition as to what constitutes a reference peak provided"
    )
  missing <- param_quos[names(check_params)] %>% map_lgl(quo_is_missing)

  if (any(missing)) {
    glue("missing parameter(s) '{collapse(names(check_params)[missing], sep = \"', '\")}':\n",
         " - {collapse(check_params[missing], sep = '\n - ')}") %>%
      stop(call. = FALSE)
  }

  # columns
  dt_cols <- get_column_names(!!enquo(dt), ratio = param_quos$ratio, group_id = param_quos$group_id,
                              n_reqs = list(group_id = "+", ratio = "+"))

  refs <- dt %>%
    filter(!!param_quos$is_ref_condition)

  if (nrow(refs) == 0)
    stop("no data to visualize, check your data table and is_ref_condition filter", call. = FALSE)

  refs <- refs %>%
    # ref is used
    {
      if (!quo_is_null(param_quos$is_ref_used))
        refs %>%
          mutate(is_ref_used = !!param_quos$is_ref_used %>%
                   as.logical() %>%
                   ifelse("yes", "no") %>%
                   factor(levels = c("yes", "no")))
      else
        .
    } %>%
    # gather
    gather(ratio, value, !!!dt_cols$ratio) %>%
    # calculate deviation across all runs
    group_by(ratio) %>%
    mutate(total_delta_deviation = (value/mean(value) - 1) * 1000) %>%
    ungroup() %>%
    # calculate deviation within each analysis
    group_by(ratio, !!!map(dt_cols$group_id, sym)) %>%
    mutate(
      delta_deviation = (value/mean(value) - 1) * 1000,
      ref_peak_nr = 1:n()) %>%
    ungroup() %>%
    mutate(ref_peak_nr = factor(ref_peak_nr))

  # plot reference peaks relative to total average
  p <- refs %>%
    ggplot() +
    aes_string(x = dt_cols$group_id[1], y = "total_delta_deviation", fill = "ref_peak_nr") +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0) +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Analysis", fill = "Ref. peak #", y = "Deviation from total average [permil]") +
    facet_wrap(~ratio, ncol = 1, scales = "free_y")

  # ref is used
  if (!quo_is_null(param_quos$is_ref_used)) {
    p <- p %+% aes(alpha = is_ref_used) +
      scale_alpha_discrete(range = c(1, 0.5)) +
      labs(alpha = "Ref. peak used")
  }

  # within group
  if (within_group) {
    # vs. average within group
    p <- p %+%
      aes(y = delta_deviation) +
      labs(y = "Deviation within group [permil]")
  }
  return(p)
}

#' Visualize calibration parameters
#'
#' Visualize calibration coefficients and summary.
#'
#' @param x the x axis aesthetic for the calibration parameters, can be datetime, text or numeric
#' @param select_from_summary which parameters from the fit summary to include, by default includes the adjusted R2 (renamed just \code{R2}) and the root mean square deviation (\code{RMSD}), which R often calls \link[stats]{sigma} or residual standard deviation (often also called residual standard error and root mean square error instead of deviation, or standard error of the regression).
#' @param color variable to use for color aesthetic for the plot
#' @param shape variable to use for shape aesthetic for the plot
#' @param size variable to use for size aesthetic for the plot
#' @param date_breaks what breaks to use for the x axis if it is a datetime
#' @param date_labels datetime label pattern for x axis if it is a datetime
#' @export
iso_plot_calibration_parameters <- function(dt, calibration = "", x,
                                       select_from_summary = c(R2 = adj.r.squared, RMSD = sigma),
                                       color = NULL, shape = NULL, size = NULL,
                                       date_breaks = "2 hours", date_labels = "%d %b %H:%M") {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(x)) stop("no x axis aesthetic specified", call. = FALSE)

  # check for model parameters
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  # pull out all coefficients (all for now, should always show all?)
  calib_coefs <- dt %>%
    iso_unnest_calibration_coefs(
      calibration = calibration,
      keep_other_list_data = FALSE
    )

  # pull out requested summary
  select_quo <- enquo(select_from_summary)
  calib_summary <- dt %>%
    iso_unnest_calibration_summary(
      calibration = calibration,
      select = everything(),
      keep_other_list_data = FALSE
    )
  cs_cols <- get_column_names(calib_summary, select = select_quo, n_reqs = list(select = "*"))

  # check if any summary should be included
  if (length(cs_cols$select) > 0) {
    calib_summary <- calib_summary %>%
      rename(!!!map(cs_cols$select, sym)) %>%
      gather(term, estimate, !!!map(names(cs_cols$select), sym))
    visualization_data <-
      bind_rows(calib_coefs, calib_summary)

    # make signif available for use
    if ("signif" %in% names(visualization_data))
      visualization_data <- visualization_data %>%
      mutate(signif = ifelse(is.na(signif), "NA", signif))

  } else {
    visualization_data <- calib_coefs
  }

  # column quos
  vis_quos <-
    list(x = enquo(x), color = enquo(color), shape = enquo(shape), size = enquo(size))

  # make sure we only plot unique data sets
  visualization_data <- unique(visualization_data)

  # generate plot
  p <- visualization_data %>%
    # make sure term factor is in order (i.e. paneling)
    mutate(term = as_factor(term)) %>%
    filter(!is.na(estimate)) %>%
    ggplot() +
    aes_q(x = vis_quos$x, y = sym("estimate")) +
    { if(!quo_is_null(vis_quos$color)) aes_q(color = vis_quos$color) } +
    { if(!quo_is_null(vis_quos$shape)) aes_q(shape = vis_quos$shape) } +
    { if(!quo_is_null(vis_quos$size)) aes_q(size = vis_quos$size) } +
    geom_errorbar(data = function(df) filter(df,!is.na(std.error)),
                  map = aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0, size = 1) +
    { if (quo_is_null(vis_quos$size)) geom_point(size = 4) else geom_point() } +
    facet_wrap(~term, ncol = 1, scales = "free_y") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(plot.margin = margin(10, 5, 15, 20)) +
    labs(y="")

  # type of x axis
  x <- mutate(visualization_data, x = !!vis_quos$x)$x
  if (is(x, "POSIXct")) {
    p <- p +
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels) +
      labs(x = "")
  }

  return(p)
}

#' Visualize the calibration range
#'
#' This function visualizes the calibration range for each regression model (separate panels) as it is constrained in the chosen x and y variables (most commonly measures of delta value and signal strength) by showing analysis points within the calibration range in solid and all others slighly opaque, as well as by drawing a rectangle around the calibration range. Note that if one of the chosen dimensions (x or y) is not part of the calibration for a model but the other dimension is, it simply shows two lines bracketing the samples in the dimension that is part of the calibration. Also note that the rectangular interpretation of calibration range in this plot is a visual simplification, technically the range is polygonal and maybe be more than 2 dimensional for more complex regression models.
#' @inheritParams iso_plot_calibration_parameters
#' @param x the x-axis column for visualizing the calibration range, typically a measure of signal strength (amplitude or area) that is part of the/some calibration regression models
#' @param y the y-axis column for visualizing the calibration range, typically a measure of the isotopic value (ratio or delta) that is part of the calibration regression models
#' @export
iso_plot_calibration_range <- function(dt, calibration = "", x, y, color = NULL, shape = NULL, size = NULL) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(x)) stop("have to provide an x to plot", call. = FALSE)
  if (missing(y)) stop("have to provide at least one column to plot", call. = FALSE)

  # quos
  vis_quos <- list(dt = enquo(dt), x = enquo(x), y = enquo(y),
                   color = enquo(color), size = enquo(size), shape = enquo(shape))

  # check for model parameters
  calib_vars <- isoprocessor:::get_calibration_vars(calibration)
  isoprocessor:::check_calibration_cols(!!vis_quos$dt, calib_vars$model_params)

  # data
  dt <- dt %>% mutate(..rowid.. = row_number())
  data <- dt %>% iso_unnest_data()

  # check existence of x and y
  dt_cols <- isoprocessor:::get_column_names(data, x = vis_quos$x, y = vis_quos$y)

  # ranges
  ranges <- dt %>% iso_unnest_calibration_range(calibration = calibration)
  xrange <- ranges %>% filter(var == dt_cols$x) %>% select(..rowid.., xmin = min, xmax = max)
  yrange <- ranges %>% filter(var == dt_cols$y) %>% select(..rowid.., ymin = min, ymax = max)
  data <- data %>%
    # facet
    mutate(panel = !!sym(calib_vars$model_name)) %>%
    # ranges
    left_join(xrange, by = "..rowid..") %>%
    left_join(yrange, by = "..rowid..") %>%
    mutate(
      xmin = ifelse(is.na(xmin), -Inf, xmin),
      xmax = ifelse(is.na(xmax), Inf, xmax),
      ymin = ifelse(is.na(ymin), -Inf, ymin),
      ymax = ifelse(is.na(ymax), Inf, ymax),
      x_in_range = !!sym(dt_cols$x) >= xmin & !!sym(dt_cols$x) <= xmax,
      y_in_range = !!sym(dt_cols$y) >= ymin & !!sym(dt_cols$y) <= ymax,
      in_range = x_in_range & y_in_range)
  # note on ranges: ideally with alpha = 0.5 but then have to make sure each rectangle
  # is only drawn once which is tricky if unclear which columns are grouping columns
  # would have to make unique based on all columns that have only 1 entry for each ..rowid..

  # color
  if (!quo_is_null(vis_quos$color))
    data <- data %>% mutate(color = !!vis_quos$color)


  # shape
  if (!quo_is_null(vis_quos$shape))
    data <- data %>% mutate(shape = !!vis_quos$shape)

  # size
  if (!quo_is_null(vis_quos$size))
    data <- data %>% mutate(size = !!vis_quos$size)

  # plot
  data %>%
    ggplot() +
    aes_q(x = vis_quos$x, y = vis_quos$y, alpha = sym("in_range")) +
    { if(!quo_is_null(vis_quos$color)) aes(color = color) } +
    { if(!quo_is_null(vis_quos$shape)) aes(shape = shape) } +
    { if(!quo_is_null(vis_quos$size)) aes(size = size) } +
    # in range area
    geom_rect(
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    color = NULL, fill = NULL),
      color = "black", fill = NA) +
    scale_alpha_discrete(
      "calibration range",
      breaks = c(FALSE, TRUE),
      labels = function(x) ifelse(x, "in range", "out of range"),
      range = c(0.4, 1)) +
    guides(
      color = guide_legend(override.aes = list(fill = "white", linetype = 0)),
      alpha = guide_legend(override.aes = list(fill = "white", linetype = 0))) +
    # points
      { if (quo_is_null(vis_quos$size)) geom_point(size = 4) else geom_point() } +
    theme_bw() +
    facet_wrap(~panel)
}



#' Visualize the data
#'
#' General purpose convenience visualization function. Simply add other ggplot components after calling this function to customize more (e.g. with \link[ggplot2]{facet_wrap} or \link[ggplot2]{theme} calls).
#'
#' @inheritParams iso_map_peaks
#' @inheritParams iso_plot_calibration_parameters
#' @param y which columns to visualize, combine with c()
#' @param group what to group by, multiple columns allowed (combine with paste(...)), usually not necessary if groupings are fully defined through other aesthetics
#' @param linetype variable to use for linetype aesthetic for the plot
#' @param y_error an error column for drawing y error bars - if multiple \code{y} are provided, error needs to point to the same number of columns
#' @param lines whether to plot lines (TRUE by default)
#' @param points whether to plot points (FALSE by default)
#' @export
#' @note should probably make sure that the default columns for gather 'panel' and 'value' do not exist...
#' @note it would be great to allow renaming of the columns via this (especially the y column)
#' @export
iso_plot_data <- function(dt, x, y, y_error = NULL, group = NULL, color = NULL, shape = NULL, size = NULL, linetype = NULL, lines = TRUE, points = FALSE) {
  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(x)) stop("have to provide an x to plot", call. = FALSE)
  if (missing(y)) stop("have to provide at least one column to plot", call. = FALSE)

  # quoss
  vis_quos <-
    list(dt = enquo(dt),
         x = enquo(x), y = enquo(y), y_error = enquo(y_error),
         group = enquo(group),
         color = enquo(color), shape = enquo(shape),
         linetype = enquo(linetype), size = enquo(size))

  # check existence of ys and errors
  dt_cols <- get_column_names(!!vis_quos$dt, y = vis_quos$y, y_error = vis_quos$y_error,
                              n_reqs = list(y = "+", y_error = "*"))

  # double check same number of ys and errors
  if (length(dt_cols$y_error) > 0 && length(dt_cols$y) != length(dt_cols$y_error))
    glue("not the same number of y ({collapse(dt_cols$y, sep = ', ')}) and ",
         "y_error columns ({collapse(dt_cols$y_error, sep = ', ')}) provided") %>%
    stop(call. = FALSE)

  # gather data
  dt$..row_id.. <- 1:nrow(dt)
  visualization_data <- gather(dt, panel, y_value, !!!map(dt_cols$y, sym))

  # y_errors
  if (length(dt_cols$y_error) > 0) {
    y_error_data <- gather(dt, panel_y_error, y_error, !!!map(dt_cols$y_error, sym))
    visualization_data <-
      visualization_data %>%
      left_join(
        data_frame(panel = dt_cols$y, panel_y_error = dt_cols$y_error),
        by = "panel"
      ) %>%
      left_join(
        select(y_error_data, ..row_id.., panel_y_error, y_error),
        by = c("..row_id..", "panel_y_error")
      ) %>%
      select(-panel_y_error)
  }

  # generate plot
  p <- visualization_data %>%
    # make sure panel factor is in order
    mutate(panel = as_factor(panel)) %>%
    filter(!is.na(y_value)) %>%
    ggplot() +
    # aesthetics
    aes_q(x = vis_quos$x, y = sym("y_value")) +
    { if(!quo_is_null(vis_quos$group)) aes_q(group = vis_quos$group) } +
    { if(!quo_is_null(vis_quos$color)) aes_q(color = vis_quos$color) } +
    { if(!quo_is_null(vis_quos$shape)) aes_q(shape = vis_quos$shape) } +
    { if(!quo_is_null(vis_quos$linetype)) aes_q(linetype = vis_quos$linetype) } +
    { if(!quo_is_null(vis_quos$size)) aes_q(size = vis_quos$size) } +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(plot.margin = margin(10, 5, 15, 20)) +
    labs(y="")

  # y error bars
  if (length(dt_cols$y_error) > 0) {
    p <- p + geom_errorbar(
      data = function(df) filter(df, !is.na(y_error)),
      mapping = aes(ymin = y_value - y_error, ymax = y_value + y_error),
      width = 0)
  }

  # lines and points
  if (lines) {
    p <- p + geom_line()
  }
  if (points) {
    p <- p +  { if (quo_is_null(vis_quos$size)) geom_point(size = 4) else geom_point() }
  }

  return(p)
}
