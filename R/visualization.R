# raw data plots =====

#' Plot raw data from isoreader files
#'
#' Convenience function for making quick standard plots for raw isoreader data.
#' Calls \code{\link{iso_plot_continuous_flow_data}} and \code{\link{iso_plot_dual_inlet_data}} for data specific plotting (see those functions for parameter details).
#' For customizing plotting calls, it is easier to use \code{\link{iso_plot_continuous_flow_data}} and \code{\link{iso_plot_dual_inlet_data}} directly.
#'
#' @param iso_files collection of iso_file objects
#' @param ... parameters for the data specific plotting functions
#' @inheritParams iso_show_default_processor_parameters
#' @family plot functions
#' @export
iso_plot_raw_data <- function(iso_files, ..., quiet = default(quiet)) {
  if(!iso_is_object(iso_files)) stop("can only plot iso files or lists of iso files", call. = FALSE)

  iso_files <- iso_as_file_list(iso_files)

  if (!quiet) sprintf("Info: plotting data from %d data file(s)", length(iso_files)) %>% message()

  if (iso_is_continuous_flow(iso_files))
    iso_plot_continuous_flow_data (iso_files, ...)
  else if (iso_is_dual_inlet(iso_files))
    iso_plot_dual_inlet_data (iso_files, ...)
  else
    stop("plotting of this type of iso_files not yet supported", call. = FALSE)
}


# NOTE: should the color and linetype aesthetics allow any expression?
#' Plot chromatogram from continuous flow data
#'
#' @inheritParams iso_plot_raw_data
#' @param data which masses and ratios to plot (e.g. c("44", "45", "45/44")), if omitted, all available masses and ratios are plotted. Note that ratios should be calculated using \code{\link{iso_calculate_ratios}} prior to plotting.
#' @param time_interval which time interval to plot
#' @param time_interval_units which units the time interval is in, default is "seconds"
#' @param filter any filter condition to apply to the data beyond the masses/ratio selection (param \code{data}) and time interval (param \code{time_interval}). For details on the available data columns see \link{iso_get_raw_data} with parameters \code{gather = TRUE} and \code{include_file_info = everything()} (i.e. all file info is available for plotting aesthetics).
#' @param normalize whether to normalize all data (default is FALSE, i.e. no normalization). If TRUE, normalizes each trace across all files. Normalizing always scales such that each trace fills the entire height of the plot area. Note that zooming (if \code{zoom} is set) is applied after normalizing.
#' @param zoom if not set, automatically scales to the maximum range in the selected time_interval in each panel. If set, scales by the indicated factor, i.e. values > 1 are zoom in, values < 1 are zoom out, baseline always remains the bottom anchor point, zooming is always relative to the max in the plot (even if that is outside visible frame). Note that for overlay plots (\code{panel_bu = "none"}) zooming is relative to the max in each panel (potentially across different data traces). Also note that zooming only affects masses, ratios are not zoomed.
#' @param panel whether to panel data by anything - any data column is possible (see notes in the \code{filter} parameter) but the most commonly used options are \code{panel = NULL} (overlay all), \code{panel = data} (by mass/ratio data), \code{panel = file_id} (panel by files, alternatively use any appropriate file_info column). The default is panelling by \code{data}.
#' @param color whether to color data by anything, options are the same as for \code{panel} but the default is \code{file_id} and complex expressions (not just columns) are supported.
#' @param linetype whether to differentiate data by linetype, options are the same as for \code{panel} but the default is \code{NULL} (i.e. no linetype aesthetic) and complex expressions (not just columns) are supported. Note that a limited number of linetypes (6) is defined by default and the plot will fail if a higher number is required unless specified using \code{\link[ggplot2]{scale_linetype}}.
#' @param label this is primarily of use for turning these into interactive plots via ggplotly as it present as an additional mousover label. Any unique file identifier is a useful choice, the default is \code{file_id}.
#' @param ... deprecated parameters
#' @family plot functions
#' @export
iso_plot_continuous_flow_data <- function(
  iso_files, data = c(),
  time_interval = c(), time_interval_units = "seconds",
  filter = NULL,
  normalize = FALSE, zoom = NULL,
  panel = data, color = file_id, linetype = NULL, label = file_id,
  ...) {

  # safety checks
  if(!iso_is_continuous_flow(iso_files))
    stop("iso_plot_continuous_flow_data can only plot continuous flow iso_files", call. = FALSE)

  # check for deprecated parameters
  dots <- list(...)
  old <- c("panel_by", "color_by", "linetype_by", "shape_by")
  if (any(old %in% names(dots))) {
    glue("deprecated parameter(s): ",
         "'{collapse(old[old %in% names(dots)], sep=\"', '\")}' ",
         "- please check the function documentation for details on ",
         "the updated parameters") %>%
      stop(call. = FALSE)
  }
  if (length(dots) > 0) {
    glue("unkown parameter(s): ",
         "'{collapse(names(dots), sep=\"', '\")}' ") %>%
      stop(call. = FALSE)
  }

  # global vars
  time <- type <- value <- file_id <- category <- data_without_units <- NULL
  is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL

  # collect raw data
  raw_data <- iso_get_raw_data(iso_files, gather = TRUE, quiet = TRUE, include_file_info = everything())
  if (nrow(raw_data) == 0) stop("no raw data in supplied iso_files", call. = FALSE)

  # check for column existence and expression evaluation
  aes_quos <- list(panel = enquo(panel), color = enquo(color),
                   linetype = enquo(linetype), label = enquo(label))

  aes_cols <- get_column_names(raw_data, panel = aes_quos$panel, n_reqs = list(panel = "?"))
  isoreader:::check_expressions(raw_data, aes_quos$color, aes_quos$linetype, aes_quos$label)

  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 )
    stop("data not available in the provided iso_files: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- dplyr::filter(raw_data, data %in% select_data)

  # time column
  time_pattern <- "^time\\.(.*)$"
  time_column <- stringr::str_subset(names(raw_data), time_pattern)
  if (length(time_column) != 1)
    stop("unclear which column is the time column, consider an explicit 'iso_convert_time' call before plotting, found: ",
         str_c(time_column, collapse = ", "), call. = FALSE)
  time_unit <- stringr::str_match(time_column, time_pattern) %>% {.[2] }
  raw_data$time <- raw_data[[time_column]]

  # time interval
  if (length(time_interval) == 2) {
    time_interval <- scale_time(time_interval, to = time_unit, from = time_interval_units)
  } else if (length(time_interval) > 0 && length(time_interval) != 2)
    stop("time interval needs to be a vector with two numeric entries, found: ", str_c(time_interval, collapse = ", "), call. = FALSE)
  time_unit <- sprintf("[%s]", time_unit)

  # general filter
  filter_quo <- enquo(filter)
  if (!quo_is_null(filter_quo)) {
    raw_data <- dplyr::filter(raw_data, !!filter_quo)
  }

  # border extrapolation function
  extrapolate_border <- function(cutoff, border, change, time, value) {
    cutoff <- unique(cutoff)
    if (length(cutoff) != 1) stop("problematic cutoff", call. = FALSE)
    bi <- which(border) # border indices
    bi2 <- bi + 1 - change[bi] # end point of border
    bi1 <- bi - change[bi] # start point of border
    border_time <- (cutoff - value[bi1])/(value[bi2] - value[bi1]) * (time[bi2] - time[bi1]) + time[bi1]
    time[bi] <- border_time
    return(time)
  }

  # normalize data
  normalize_data <- function(df) {
    group_by(df, data) %>%
      mutate(value = (value - min(value, na.rm = TRUE))/
               (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
      ungroup()
  }

  # plot data
  zoom_grouping <- unname(aes_cols$panel)
  plot_data <-
    raw_data %>%
    # add units to data for proper grouping
    mutate(
      data_without_units = data,
      data = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>%
    # make ratio identification simple
    mutate(is_ratio = category == "ratio") %>%
    arrange(time) %>%
    # find global zoom cutoffs per panel before time filtering (don't consider ratios)
    {
      if (!is.null(zoom)) {
        group_by(., !!sym(zoom_grouping)) %>%
          mutate(
            baseline = value[!is_ratio] %>% { if(length(.) == 0) NA else min(.) },
            max_signal = value[!is_ratio] %>% { if(length(.) == 0) NA else max(.) }) %>%
          ungroup()
      } else .
    } %>%
    # time filtering
    { if (length(time_interval) == 2) dplyr::filter(., time >= time_interval[1], time <= time_interval[2]) else . } %>%
    # normalizing
    {
      if (normalize) {
        normalize_data(.) %>%
          # zooming always based on the full (0 to 1) interval
          mutate(baseline = 0, max_signal = 1)
      } else .
    } %>%
    # zooming
    {
      if (!is.null(zoom)) {
        # extrapolate data (multi-panel requires this instead of coord_cartesian)
        group_by(., file_id, data) %>%
          mutate(
            cutoff = 1/zoom * max_signal + (1 - 1/zoom) * baseline, # cutoffs
            discard = ifelse(!is_ratio, value > cutoff, FALSE), # never zoom ratios
            change = c(0, diff(discard)), # check for where the cutoffs
            border = change == 1 | c(change[-1], 0) == -1, # identify borders
            gap = c(0, change[-n()]) == 1, # identify gaps next to one border so ggplot knows the line is interrupted
            time = extrapolate_border(cutoff, border, change, time, value), # calculate time at border
            value = ifelse(border, cutoff, ifelse(gap, NA, value)) # assign values
          ) %>%
          ungroup() %>%
          dplyr::filter(!discard | border | gap) %>%
          { # re-normalize after zooming if normalizing is turned on
            if (normalize) normalize_data(.)
            else .
          }
      } else .
    } %>%
    # switch to factors for proper grouping
    mutate(
      data_with_units = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>% {
      data_levels <- tibble::deframe(select(., data, data_without_units) %>% unique())
      data_sorting <- sapply(select_data, function(x) which(data_levels == x)) %>% unlist(use.names = F)
      mutate(., data = factor(data, levels = names(data_levels)[data_sorting]))
    }

  # generate plot
  p <- plot_data %>%
    ggplot() +
    aes(time, value, group = paste(file_id, data)) +
    geom_line() +
    scale_x_continuous(str_c("Time ", time_unit), expand = c(0, 0)) +
    scale_y_continuous(if(normalize) "Normalized Signal" else "Signal", expand = c(0, 0)) +
    theme_bw()

  # zoom ghost points to make sure the zooming frame remains the same (if zoom is set)
  if (!is.null(zoom)) {
    p <- p +
      geom_point(data = function(df) group_by_(df, .dots = zoom_grouping) %>%
                   summarize(time = mean(time, na.omit = TRUE), value = min(baseline, na.omit = TRUE)) %>%
                   dplyr::filter(!is.na(value)),
                 mapping = aes(x = time, y = value), inherit.aes = FALSE,
                 size = 0, alpha = 1, show.legend = FALSE) +
      geom_point(data = function(df) group_by_(df, .dots = zoom_grouping) %>%
                   summarize(time = mean(time, na.omit = TRUE), value = max(cutoff, na.omit = TRUE)) %>%
                   dplyr::filter(!is.na(value)),
                 mapping =aes(x = time, y = value), inherit.aes = FALSE,
                 size = 0, alpha = 1, show.legend = FALSE)
  }

  # display full time scale
  if (length(time_interval) == 2)
    p <- p + expand_limits(x = time_interval)

  # normalize plot y axis
  if (normalize)
    p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

  # paneling
  if (!is_empty(aes_cols$panel))
    p <- p + facet_grid(rlang::new_formula(sym(aes_cols$panel), sym(".")), scales = "free_y")

  # color
  if (!quo_is_null(aes_quos$color))
    p <- p %+% aes_(color = aes_quos$color)

  # linetype
  if (!quo_is_null(aes_quos$linetype))
    p <- p %+% aes_(linetype = aes_quos$linetype)

  # label
  if (!quo_is_null(aes_quos$label))
    p <- p %+% aes_(label = aes_quos$label)

  # return plot
  return(p)
}

#' Plot mass data from dual inlet files
#'
#' @inheritParams iso_plot_continuous_flow_data
#' @param filter any filter condition to apply to the data beyond the masses/ratio selection (param \code{data}) and time interval (param \code{time_interval}). For details on the available data columns see \link{iso_get_raw_data} with parameters \code{gather = TRUE} and \code{include_file_info = everything()} (i.e. all file info is available for plotting aesthetics).
#' @param panel whether to panel data by anything - any data column is possible (see notes in the \code{filter} parameter) but the most commonly used options are \code{panel = NULL} (overlay all), \code{panel = data} (by mass/ratio data), \code{panel = file_id} (panel by files, alternatively use any appropriate file_info column), and \code{panel = type} (panel by sample vs standard). Additionally it is possible to panel two variables against each other (i.e. use a \link[ggplot2]{facet_grid}), e.g. by specifying the formula \code{panel = data ~ file_id} (data in the panel rows, files in the panel columns) or \code{panel = data ~ type}.The default for this parameter is simple panelling by \code{data}.
#' @param shape whether to shape data points by anything, options are the same as for \code{panel} but the default is \code{type} (sample vs standard).
#' @param ... deprecated parameters
#' @note normalization is not useful for dual inlet data, except potentially between standard and sample - however, for this it is more meaningful to simply plot the relevant ratios together
#' @family plot functions
#' @export
iso_plot_dual_inlet_data <- function(
  iso_files, data = c(), filter = NULL,
  panel = data, color = file_id, linetype = NULL, shape = type, label = file_id,
  ...) {

  # checks
  if(!iso_is_dual_inlet(iso_files))
    stop("iso_plot_dual_inlet_data can only plot dual inlet iso_files", call. = FALSE)

  # check for deprecated parameters
  dots <- list(...)
  old <- c("panel_by", "color_by", "linetype_by", "shape_by")
  if (any(old %in% names(dots))) {
    glue("deprecated parameter(s): ",
         "'{collapse(old[old %in% names(dots)], sep=\"', '\")}' ",
         "- please check the function documentation for details on ",
         "the updated parameters") %>%
      stop(call. = FALSE)
  }
  if (length(dots) > 0) {
    glue("unkown parameter(s): ",
         "'{collapse(names(dots), sep=\"', '\")}' ") %>%
      stop(call. = FALSE)
  }

  # global vars
  cycle <- value <- type <- data_without_units <- NULL

  # collect raw data
  raw_data <- iso_get_raw_data(iso_files, gather = TRUE, quiet = TRUE, include_file_info = everything())
  if (nrow(raw_data) == 0) stop("no raw data in supplied iso_files", call. = FALSE)

  # check for column existence
  aes_quos <- list(panel = enquo(panel), color = enquo(color),
                   linetype = enquo(linetype), shape = enquo(shape),
                   label = enquo(label))
  aes_cols <- list()
  isoreader:::check_expressions(raw_data, aes_quos$color, aes_quos$linetype, aes_quos$shape, aes_quos$label)

  if (quo_is_null(aes_quos$panel)) {
    # no panel
    aes_cols$panel <- c()
  } else if (quo_is_symbol(aes_quos$panel)) {
    # single symbol --> facet_wrap
    aes_cols <- c(aes_cols, get_column_names(raw_data, panel = aes_quos$panel))
  } else {
    # formula --> facet_grid
    aes_cols <- c(aes_cols, get_column_names(
      raw_data,
      panel_rows = aes_quos$panel %>% quo_expr() %>% rlang::f_lhs(),
      panel_cols = aes_quos$panel %>% quo_expr() %>% rlang::f_rhs()))
  }

  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 )
    stop("data not available in the provided iso_files: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- dplyr::filter(raw_data, data %in% select_data)

  # general filter
  filter_quo <- enquo(filter)
  if (!quo_is_null(filter_quo)) {
    raw_data <- dplyr::filter(raw_data, !!filter_quo)
  }

  # plot data
  plot_data <-
    raw_data %>%
    # data with units and in correct order
    mutate(
      data_without_units = data,
      data = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>% {
      data_levels <- tibble::deframe(select(., data, data_without_units) %>% unique())
      data_sorting <- sapply(select_data, function(x) which(data_levels == x)) %>% unlist(use.names = F)
      mutate(., data = factor(data, levels = names(data_levels)[data_sorting]))
    }

  # generate plot
  p <- plot_data %>%
    ggplot() +
    aes(cycle, value, group = paste(file_id, type, data)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous("Cycle", breaks = c(0:max(plot_data$cycle))) +
    scale_y_continuous("Signal") +
    theme_bw()

  # paneling
  if (!quo_is_null(aes_quos$panel)) {
    if (quo_is_symbol(aes_quos$panel))
      p <- p + facet_wrap(rlang::new_formula(NULL, sym(aes_cols$panel)), scales = "free_y")
    else
      p <- p + facet_grid(rlang::new_formula(sym(aes_cols$panel_rows), sym(aes_cols$panel_cols)), scales = "free_y")
  }

  # color
  if (!quo_is_null(aes_quos$color))
    p <- p %+% aes_(color = aes_quos$color)

  # linetype
  if (!quo_is_null(aes_quos$linetype))
    p <- p %+% aes_(linetype = aes_quos$linetype)

  # shape_by
  if (!quo_is_null(aes_quos$shape))
    p <- p %+% aes_(shape = aes_quos$shape)

  # label
  if (!quo_is_null(aes_quos$label))
    p <- p %+% aes_(label = aes_quos$label)

  # return plot
  return(p)
}



# reference peaks =========

#' Plot reference peaks
#'
#' Visualize how consistent the reference peaks are across a serious of samples.
#'
#' @inheritParams iso_prepare_for_calibration
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

# calibrations =========

#' Visualize calibration parameters
#'
#' Visualize calibration coefficients and summary.
#'
#' @inheritParams iso_prepare_for_calibration
#' @param calibration name of the calibration, must match the name used in \link{iso_generate_calibration} (if any)
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
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!vis_quos$dt, calib_vars$model_params)

  # data
  dt <- dt %>% mutate(..rowid.. = row_number())
  data <- dt %>% iso_unnest_data()

  # check existence of x and y
  dt_cols <- get_column_names(data, x = vis_quos$x, y = vis_quos$y)

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
        tibble(panel = dt_cols$y, panel_y_error = dt_cols$y_error),
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
