
#' Plot reference peaks
#'
#' Visualize how consistent the reference peaks across a serious of samples.
#'
#' @param is_ref_condition condition to identify which of the peaks are reference peaks
#' @param ratio which ratio column(s) to compare across the reference peaks
#' @param group_id group identifier column(s) to clarify which peaks belongs together - first column in group id is used for x axis labels
#' @param within_group whether to visualize the deviation within the specified group or across all groups
#' @export
iso_plot_ref_peaks <- function(dt, is_ref_condition, ratio = default(ratio), group_id = default(group_id), within_group = FALSE) {

  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(is_ref_condition)) stop("no condition as to what constitutes a reference peak provided", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), ratio = enquo(ratio), group_id = enquo(group_id),
                              n_reqs = list(group_id = "+", ratio = "+"))
  ref_quo <- enquo(is_ref_condition)

  refs <- dt %>%
    filter(!!ref_quo)

  if (nrow(refs) == 0)
    stop("no data to visualize, check your data table and is_ref_condition filter", call. = FALSE)

  refs <- refs %>%
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
#' @param include_from_summary which parameters from the fit summary to include
#' @param color variable to use for color aesthetic for the plot
#' @param shape variable to use for shape aesthetic for the plot
#' @param size variable to use for size aesthetic for the plot
#' @param date_breaks what breaks to use for the x axis if it is a datetime
#' @param date_labels datetime label pattern for x axis if it is a datetime
#' @export
iso_plot_calibration_parameters <- function(dt, calibration = "", x,
                                       include_from_summary = c(R2 = adj.r.squared, RMS = deviance),
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
      select = c(term, estimate, std.error),
      keep_other_list_data = FALSE
    )

  # pull out requested summary
  select_quo <- enquo(include_from_summary)
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
    visualization_data <- bind_rows(calib_coefs, calib_summary)
  } else {
    visualization_data <- calib_coefs
  }

  # check column availability
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
    aes(x = !!vis_quos$x, y = estimate,
        color = !!if(!quo_is_null(vis_quos$color)) vis_quos$color else quo(),
        shape = !!if(!quo_is_null(vis_quos$shape)) vis_quos$shape else quo(),
        size = !!if(!quo_is_null(vis_quos$size)) vis_quos$size else quo()
    ) +
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

#@ implement me
iso_plot_calibration_range <- function() {

}

#' Visualize the data
#'
#' General purpose visualization function.
#'
#' @inheritParams iso_map_peaks
#' @inheritParams iso_visualize_calibration_parameters
#' @param y which columns to visualize, combine with c()
#' @param group what to group by, multiple columns allowed (combine with c(...)), usually not necessary if color or other grouping is defined
#' @param lines whether to plot lines
#' @param points whether to plot points
#' @export
#' @note should probably make sure that the default columns for gather 'panel' and 'value' do not exist...
#' @note it would be great to allow renaming of the columns via this (especially the y column)
#' @export
iso_visualize_data <- function(dt, x, y, group = NULL, color = NULL, shape = NULL, size = NULL, linetype = NULL, lines = TRUE, points = FALSE) {
  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(x)) stop("have to provide an x to plot", call. = FALSE)
  if (missing(y)) stop("have to provide at least one column to plot", call. = FALSE)
  #Fixme, continue here
  vis_cols <- get_column_names(!!enquo(dt), x = enquo(x), y = enquo(y), group = enquo(group),
                              color = enquo(color), shape = enquo(shape), linetype = enquo(linetype), size = enquo(size),
                              n_reqs = list(y = "+", group = "*", color = "?", shape = "?", linetype = "?", size = "?"))

  # gather data
  visualization_data <- gather(dt, panel, value, !!!cols_to_quos(vis_cols$y))

  # group quos (cols_to_quos does not work in this instance since it )
  group_quos <- quo(str_c(!!!cols_to_symbol_list(vis_cols$group)))

  # generate plot
  p <- visualization_data %>%
    # make sure panel factor is in order
    mutate(panel = as_factor(panel)) %>%
    filter(!is.na(value)) %>%
    ggplot() +
    aes_q(x = sym(vis_cols$x), y = sym("value"),
          group = if(!is_empty(vis_cols$group)) group_quos else NULL,
          color = if(!is_empty(vis_cols$color)) sym(vis_cols$color) else NULL,
          shape = if(!is_empty(vis_cols$shape)) sym(vis_cols$shape) else NULL,
          linetype = if(!is_empty(vis_cols$linetype)) sym(vis_cols$linetype) else NULL,
          size = if(!is_empty(vis_cols$size)) sym(vis_cols$size) else NULL
    ) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(plot.margin = margin(10, 5, 15, 20)) +
    labs(y="")

  # lines and points
  if (lines) {
    p <- p + geom_line()
  }
  if (points) {
    p <- p + { if (is_empty(vis_cols$size)) geom_point(size = 4) else geom_point() }
  }

  return(p)
}
