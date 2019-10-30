# helper functions ===

# find the name of the time column and its unit in a raw data frame
find_time_column <- function(df) {
  # time column
  time_pattern <- "^time\\.(.*)$"
  time_column <- stringr::str_subset(names(df), time_pattern)
  if (length(time_column) != 1)
    stop("unclear which column is the time column, consider an explicit 'iso_convert_time' call, found: ",
         str_c(time_column, collapse = ", "), call. = FALSE)
  time_unit <- stringr::str_match(time_column, time_pattern) %>% {.[2] }
  return(list(column = time_column, unit = time_unit))
}

# plotting data ====

#' Prepare plotting data from continuous flow files
#'
#' This function helps with the preparation of plotting data from continuous flow data files (i.e. the raw chromatogram data). Call either explicity and pass the result to \code{\link{iso_plot_continuous_flow_data}} or let \code{\link{iso_plot_continuous_flow_data}} take care of preparing the plotting data directly from the \code{iso_files}. If a \code{peak_table} is provided for peak annotation purposes, uses \code{\link{iso_combine_raw_data_with_peak_table}} to combine the raw data from the iso_files with the provided peak data table.
#'
#' @param iso_files collection of iso_file objects
#' @param data which masses and ratios to plot (e.g. \code{c("44", "45", "45/44")} - without the units), if omitted, all available masses and ratios are plotted. Note that ratios should be calculated using \code{\link{iso_calculate_ratios}} prior to plotting.
#' @param time_interval which time interval to plot
#' @param time_interval_units which units the time interval is in, default is "seconds"
#' @param filter any filter condition to apply to the data beyond the masses/ratio selection (param \code{data}) and time interval (param \code{time_interval}). For details on the available data columns see \link{iso_get_raw_data} with parameters \code{gather = TRUE} and \code{include_file_info = everything()} (i.e. all file info is available for plotting aesthetics).
#' @param normalize whether to normalize the data (default is FALSE, i.e. no normalization). If TRUE, normalizes each trace across all files (i.e. normalized to the global max/min). This is particularly useful for overlay plotting different mass and/or ratio traces (\code{panel = NULL}). Note that zooming (if \code{zoom} is set) is applied after normalizing.
#' @param zoom if not set, automatically scales to the maximum range in the selected time_interval in each plotting panel. If set, scales by the indicated factor, i.e. values > 1 are zoom in, values < 1 are zoom out, baseline always remains the bottom anchor point. Note that zooming is always relative to the max in each zoom_group (by default \code{zoom_group = data}, i.e. each trace is zoomed separately). The maximum considered may be outside the visible time window. Note that for \code{zoom_group} other than \code{data} (e.g. \code{file_id} or \code{NULL}), zooming is relative to the max signal across all mass traces. Typically it makes most sense to set the \code{zoom_group} to the same variable as the planned \code{panel} parameter to the plotting function. Lastly, note that zooming only affects masses, ratios are never zoomed.
#' @param peak_table a data frame that describes the peaks in this chromatogram. By default, the chromatographic data is prepared WITHOUT peaks information. Supply this parameter to add in the peaks data. Typically via \code{\link{iso_get_peak_table}}. Note that the following parameters must also be set correctly IF \code{peak_table} is supplied and has non-standard column names: \code{file_id}, \code{rt}, \code{rt_start}, \code{rt_end}.
#' @inheritParams iso_combine_raw_data_with_peak_table
#' @family plot functions
#' @export
iso_prepare_continuous_flow_plot_data <- function(
  iso_files, data = c(), include_file_info = NULL,
  time_interval = c(), time_interval_units = "seconds",
  filter = NULL,
  normalize = FALSE, zoom = NULL, zoom_group = data,
  peak_table = NULL, file_id = default(file_id),
  rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end),
  rt_unit = NULL) {

  # safety checks
  if(!iso_is_continuous_flow(iso_files))
    stop("can only prepare continuous flow iso_files for plotting", call. = FALSE)

  # global vars
  # FIXME for CRAN

  # collect raw data
  raw_data <- iso_get_raw_data(iso_files, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied iso_files", call. = FALSE)

  # add in file info
  file_info <- iso_get_file_info(iso_files, select = !!enquo(include_file_info), quiet = TRUE)
  raw_data <- dplyr::left_join(raw_data, file_info, by = "file_id")

  # check for zoom_gruop column(s) existence
  aes_quos <- list(zoom_group = enquo(zoom_group))
  if (rlang::quo_is_null(aes_quos$zoom_group)) aes_quos$zoom_group <- quo(1)
  isoreader:::check_expressions(raw_data, aes_quos$zoom_group)

  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 )
    stop("data not available in the provided iso_files (don't include units): ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- dplyr::filter(raw_data, data %in% select_data)

  # time column
  time_info <- find_time_column(raw_data)

  # time interval
  if (length(time_interval) == 2) {
    time_interval <- scale_time(time_interval, to = time_info$unit, from = time_interval_units)
    raw_data <- mutate(raw_data, time_min = time_interval[1], time_max = time_interval[2])
  } else if (length(time_interval) > 0 && length(time_interval) != 2) {
    stop("time interval needs to be a vector with two numeric entries, found: ", str_c(time_interval, collapse = ", "), call. = FALSE)
  } else {
    raw_data <- mutate(raw_data, time_min = -Inf, time_max = Inf)
  }
  raw_data <- select(raw_data, 1:time_info$column, time_min, time_max, everything())

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
  plot_data <-
    raw_data %>%
    # make ratio identification simple
    mutate(is_ratio = category == "ratio") %>%
    # add units to data for proper grouping
    mutate(
      data_wo_units = data,
      data = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>%
    select(1:category, is_ratio, data, data_wo_units, everything()) %>%
    arrange(!!sym(time_info$column)) %>%
    # find global zoom cutoffs per group before time filtering (don't consider ratios)
    {
      if (!is.null(zoom)) {
        mutate(., ..zoom_group = !!aes_quos$zoom_group) %>%
          group_by(..zoom_group) %>%
          mutate(
            baseline = value[!is_ratio] %>% { if(length(.) == 0) NA else min(.) },
            max_signal = value[!is_ratio] %>% { if(length(.) == 0) NA else max(.) }) %>%
          ungroup() %>%
          select(-..zoom_group)
      } else .
    } %>%
    # time filtering
    { if (length(time_interval) == 2) dplyr::filter(., dplyr::between(!!sym(time_info$column), time_interval[1], time_interval[2])) else . } %>%
    # info fields
    mutate(zoom_cutoff = NA_real_, normalized = FALSE) %>%
    # normalizing
    {
      if (normalize) {
        normalize_data(.) %>%
          # zooming always based on the full (0 to 1) interval
          mutate(baseline = 0, max_signal = 1, normalized = TRUE)
      } else .
    } %>%
    # zooming
    {
      if (!is.null(zoom)) {
        # extrapolate data (multi-panel requires this instead of coord_cartesian)
        group_by(., file_id, data) %>%
          # note: this does not do perfectly extrapolating the line when only a single
          # data point is missing (extrapolation on the tail is missing) because it
          # would require adding another row to account both for gap and extrapolated
          mutate(
            zoom_cutoff = 1/zoom * max_signal + (1 - 1/zoom) * baseline, # cutoffs
            discard = ifelse(!is_ratio, value > zoom_cutoff, FALSE), # never zoom ratios
            change = c(0, diff(discard)), # check for where the cutoffs
            border = change == 1 | c(change[-1], 0) == -1, # identify borders
            gap = c(0, change[-dplyr::n()]) == 1, # identify gaps next to one border so ggplot knows the line is interrupted
            !!sym(time_info$column) := extrapolate_border(zoom_cutoff, border, change, !!sym(time_info$column), value), # calculate time at border
            value = ifelse(border, zoom_cutoff, ifelse(gap, NA, value)) # assign values
          ) %>%
          ungroup() %>%
          dplyr::filter(!discard | border | gap) %>%
          select(-discard, -change, -border, -gap)
          # #%>%
          # { # re-normalize after zooming if normalizing is turned on
          #   if (normalize) normalize_data(.) %>% mutate(zoom_cutoff = 1.0)
          #   else .
          # }
      } else .
    } %>%
    # switch to factors for proper grouping
    {
      data_levels <- tibble::deframe(select(., data, data_wo_units) %>% unique())
      data_sorting <- sapply(select_data, function(x) which(data_levels == x)) %>% unlist(use.names = F)
      mutate(.,
             data = factor(data, levels = names(data_levels)[data_sorting]),
             data_wo_units = factor(data_wo_units, levels = unique(as.character(data_levels)[data_sorting])))
    } %>%
    dplyr::arrange(tp, data)

  # peaks table

  if (!is.null(peak_table) && nrow(peak_table) > 0) {
    plot_data <- iso_combine_raw_data_with_peak_table(
      plot_data, peak_table,
      file_id = !!enquo(file_id), data_trace = data,
      rt = !!enquo(rt), rt_start = !!enquo(rt_start), rt_end = !!enquo(rt_end),
      rt_unit = rt_unit)
  }

  # return
  return(plot_data)
}

# format numbers for plotting ====

#' Format iso numbers
#'
#' Function to easily format number columns for peak labels or other text output.
#' @param ... variable names with data. Must have the same dimensions if multiple are supplied. Can be named to rename variable name output. Will include units in output for all \link{iso_double_with_units}.
#' @param signif number of significant digits for numbered data
#' @param format_names how to format the variable names, set to \code{NULL} to remove names
#' @param format_units how to format the units from \code{\link{iso_double_with_units}} variables, set to \code{NULL} to omit units
#' @param replace_permil whether to replace the term 'permil' with the permil symbol (\\u2030)
#' @param sep separator between variables if multiple are provided in \code{...}
#' @family plot functions
#' @examples
#' x <- iso_double_with_units(1:5, "V")
#' y <- iso_double_with_units(1:5, "permil")
#' iso_format(x, y)
#' iso_format(amplitude = x, d13C = y)
#' @export
iso_format <- function(..., signif = 3, format_names = "%s: ", format_units="%s", replace_permil = TRUE, sep = "\n") {
  # find variable names
  vars <- rlang::enquos(...)
  has_name <- nchar(names(vars)) > 0
  names(vars)[!has_name] <- map_chr(vars[!has_name], rlang::as_label)

  # evaluate variables
  vars <- map(vars, rlang::eval_tidy)

  # check length
  vars_size <- map_int(vars, length)
  if (!all(vars_size == vars_size[1]))
    stop("iso_format encountered variables with unequal lengths", call. = FALSE)

  # format data
  values <- map2(vars, names(vars), ~{
    value <-
      if (iso_is_double_with_units(.x) && !is.null(format_units))
        paste0(signif(.x, digits = signif), sprintf(format_units, iso_get_units(.x)))
      else if (iso_is_double_with_units(.x) || is.numeric(.x))
        as.character(signif(as.numeric(.x), digits = signif))
      else as.character(.x)
    if (!is.null(format_names)) value <- paste0(sprintf(format_names, .y), value)
    value
  })

  # full text
  return(
    do.call(paste, args = c(values, list(sep = "\n"))) %>%
      stringr::str_replace(fixed("permil"), "\u2030")
  )
}

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


#' Plot chromatogram from continuous flow data
#'
#' This function provides easy plotting for mass and ratio chromatograms from continuous flow IRMS data. It can be called either directly with a set of \code{iso_file} objects, or with a data frame prepared for plotting chromatographic data (see \code{\link{iso_prepare_continuous_flow_plot_data}}).
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @family plot functions
#' @export
iso_plot_continuous_flow_data <- function(...) {
  UseMethod("iso_plot_continuous_flow_data")
}

#' @export
iso_plot_continuous_flow_data.default <- function(x, ...) {
  stop("this function is not defined for objects of type '",
       class(x)[1], "'", call. = FALSE)
}

#' @export
iso_plot_continuous_flow_data.iso_file <- function(iso_files, ...) {
  iso_plot_continuous_flow_data(iso_as_file_list(iso_files), ...)
}

#' @inheritParams iso_prepare_continuous_flow_plot_data
#' @param peak_table a data frame that describes the peaks in this chromatogram. By default, the peak table from the \code{iso_files} is used if any peak features are requested in the plot (e.g. \code{peak_marker=TRUE} or \code{peak_bounds=TRUE}). If \code{peak_table} is supplied with non-standard column names, the following parameters must also be set correctly: \code{file_id}, \code{rt}, \code{rt_start}, \code{rt_end}, and potentially \code{rt_unit}.
#' @rdname iso_plot_continuous_flow_data
#' @export
iso_plot_continuous_flow_data.iso_file_list <- function(
  iso_files, data = c(),
  time_interval = c(), time_interval_units = "seconds",
  filter = NULL,
  normalize = FALSE, zoom = NULL,
  panel = data, color = file_id, linetype = NULL, label = file_id,
  peak_table = iso_get_peak_table(iso_files, quiet = TRUE), file_id = default(file_id),
  rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end),
  rt_unit = NULL,
  peak_marker = FALSE, peak_bounds = FALSE, peak_bgrd = FALSE,
  peak_label = NULL, peak_label_filter = NULL, peak_label_size = 2, peak_label_repel = 1) {

  # safety checks
  if(!iso_is_continuous_flow(iso_files))
    stop("iso_plot_continuous_flow_data can only plot continuous flow iso_files", call. = FALSE)

  # need peak table?
  peak_table_quo <- enquo(peak_table)
  peak_label_quo <- enquo(peak_label)
  if (peak_marker || peak_bounds || peak_bgrd || !rlang::quo_is_null(peak_label_quo)) {
    peak_table <- rlang::eval_tidy(peak_table_quo)
  } else {
    peak_table <- NULL
  }

  # retrieve data (with all info so additional aesthetics are easy to include)
  panel_quo <- enquo(panel)
  plot_data <- iso_prepare_continuous_flow_plot_data(
    iso_files,
    data = data,
    include_file_info = everything(),
    time_interval = time_interval,
    time_interval_units = time_interval_units,
    filter = !!enquo(filter),
    normalize = normalize,
    zoom = zoom,
    zoom_group = !!panel_quo,
    peak_table = peak_table,
    file_id = !!enquo(file_id),
    rt = !!enquo(rt),
    rt_start = !!enquo(rt_start),
    rt_end = !!enquo(rt_end),
    rt_unit = rt_unit
  )

  # plot
  iso_plot_continuous_flow_data(
    plot_data,
    panel = !!enquo(panel),
    color = !!enquo(color),
    linetype = !!enquo(linetype),
    label = !!enquo(label),
    peak_marker = peak_marker,
    peak_bounds = peak_bounds,
    peak_label = !!peak_label_quo,
    peak_label_filter = !!enquo(peak_label_filter),
    peak_label_size = peak_label_size,
    peak_label_repel = peak_label_repel
  )
}

#' @rdname iso_plot_continuous_flow_data
#' @param df a data frame of the chromatographic data prepared for plotting (see \code{\link{iso_prepare_continuous_flow_plot_data}})
#' @param panel whether to panel plot by anything - any column or complex expression is possible (see notes in the \code{filter} parameter for available raw data columns and \code{\link{iso_get_file_info}} for available file info columns) but the most commonly used options are \code{panel = NULL} (overlay all), \code{panel = data} (by mass/ratio data), \code{panel = file_id} (panel by files, alternatively use any appropriate file_info column or expression that's unique for each file). The default is panelling by the \code{data} column.
#' @param color whether to color plot by anything, options are the same as for \code{panel} but the default is \code{file_id}
#' @param linetype whether to differentiate by linetype, options are the same as for \code{panel} but the default is \code{NULL} (i.e. no linetype aesthetic). Note that a limited number of linetypes (6) is defined by default and the plot will fail if a higher number is required unless specified using \code{\link[ggplot2]{scale_linetype}}.
#' @param label this is primarily of use for turning the generated ggplots into interactive plots via \code{\link[plotly]{ggplotly}} as the \code{label} will be rendered as an additional mousover label. Any unique file identifier is a useful choice, the default is \code{file_id}.
#' @param peak_marker whether to mark identified peaks with a vertical line at the peak retention time. Only works if a \code{peak_table} was provided to identify the peaks and will issue a warning if \code{peak_marker = TRUE} but no peaks were identified.
#' @param peak_bounds whether to mark the boundaries of identified peaks with a vertical line at peak start and end retention times. Only works if a \code{peak_table} was provided to identify the peaks and will issue a warning if \code{peak_bounds = TRUE} but no peaks were identified.
#' @param peak_bgrd NOT YET IMPLEMENTED whether to show the background of identified peaks from start to end retention times. Only works if a \code{peak_table} was provided that has \code{bgrdX_start} and \code{bgrdX_end} columns in the same units as the raw data.
#' @param peak_label whether to label identified peaks. Any valid column or complex expression is supported and ALL columns in the provided \code{peak_table} can be used in this expression. The easiest way to generate well constructed peak labels is via the \code{\link{iso_format}} function. To provide more space for peak labels, it is sometimes useful to use a \code{zoom} value smaller than 1 to zoom out a bit, e.g. \code{zoom = 0.9}. If peak labels overlap, consider changing \code{peak_label_size} and/or \code{peak_label_repel}. Note that this only works if a \code{peak_table} was provided to identify the peaks and will issue a warning if \code{peak_label} is set but no peaks were identified. Also note that peaks whose value at the peak retention time is not visible on the panel due to e.g. a high \code{zoom} value will not have a visible label either.
#' @param peak_label_filter a filter for the peak labels (if supplied). Can be useful for highlighting only a subset of peaks with peak labels (e.g. only one data trace, or only those in a certain portion of the chromatogram). Only interpreted if \code{peak_table} is set.
#' @param peak_label_size the font size for the peak labels. Depends largely on how much data is shown and how busy the chromatograms are. Default is a rather small font size (2), adjust as needed.
#' @param peak_label_repel how strongly the labels repel each other. Increase the value if large labels overlap (e.g. to 5 or 10).
#' @export
iso_plot_continuous_flow_data.data.frame <- function(
  df, panel = data, color = file_id, linetype = NULL, label = file_id,
  peak_marker = FALSE, peak_bounds = FALSE, peak_bgrd = FALSE,
  peak_label = NULL, peak_label_filter = NULL, peak_label_size = 2, peak_label_repel = 1
  ) {

  # check for data
  if (nrow(df) == 0) stop("no data provided", call. = FALSE)

  # check for time column
  time_info <- find_time_column(df)

  # quos and other column checks
  aes_quos <- list(panel = enquo(panel), color = enquo(color), linetype = enquo(linetype), label = enquo(label), peak_label = enquo(peak_label))

  aes_cols <- get_column_names(
    df,
    file_id = quo("file_id"),
    time_min = quo("time_min"), time_max = quo("time_max"),
    is_ratio = quo("is_ratio"), data = quo("data"), value = quo("value"))
  peak_cols <- c("peak_marker", "peak_point", "peak_start", "peak_end") %in% names(df)
  isoreader:::check_expressions(df, aes_quos$color, aes_quos$linetype, aes_quos$label, aes_quos$panel)

  # add panel column to allow expressions
  if (!quo_is_null(aes_quos$panel)) {
    df <- mutate(df, ..panel = !!aes_quos$panel)
  }

  # find overall plot parameters from data frame
  normalize <- col_in_df(df, "normalized") & df$normalized[1]
  zoom <- col_in_df(df, "zoom_cutoff") & !all(is.na(df$zoom_cutoff[1]))
  if (col_in_df(df, "normalize")) stopifnot(all(df$normalized == df$normalized[1])) # should be a single value

  # generate plot
  p <- ggplot(df) +
    aes(!!sym(time_info$column), value, group = paste(file_id, data)) +
    scale_x_continuous(str_c("Time ", time_info$unit), expand = c(0, 0)) +
    scale_y_continuous(if(normalize) "Normalized Signal" else "Signal", expand = c(0, 0)) +
    theme_bw()

  # peak cols safety check
  if (!all(peak_cols) && (peak_marker || peak_bounds || !quo_is_null(aes_quos$peak_label))) {
    peak_marker <- FALSE
    peak_bounds <- FALSE
    aes_quos$peak_label <- quo(NULL)
    glue::glue(
      "peak features requested but peak identifications seem to be missing - ",
      "ignoring all peak feature parameters. Please make sure to provide a peak_table.") %>%
      warning(immediate. = TRUE, call. = FALSE)
  }
  if (!quo_is_null(aes_quos$peak_label)) {
    isoreader:::check_expressions(df, aes_quos$peak_label)
  }

  # peak boundaries - consider making this an area background
  if (peak_bounds && nrow(dplyr::filter(df, peak_point > 0 & (peak_start | peak_end))) > 0) {
    p <- p +
      geom_rect(
        data = function(df) dplyr::filter(df, peak_point > 0 & (peak_start | peak_end)) %>%
          dplyr::group_by(peak_point) %>%
          dplyr::mutate(xmin = ifelse(any(peak_start),
                                      min((!!sym(time_info$column))[peak_start]), -Inf),
                        xmax = ifelse(any(peak_end),
                                      max((!!sym(time_info$column))[peak_end]), Inf)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.infinite(xmin) | !is.infinite(xmax)) %>%
          # collapse double entries without loosing any other potentially important aesthetics info
          dplyr::select(-!!sym(time_info$column), -value, -peak_start, -peak_end, -peak_marker) %>%
          { if ("tp" %in% names(.)) dplyr::select(., -tp) else . } %>%
          unique(),
        mapping = aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, color = NULL),
        fill = "grey20", color = NA, alpha = 0.1, show.legend = FALSE
      )
  }

  # peak markers
  if (peak_marker) {
    p <- p +
      geom_vline(
        data = function(df) dplyr::filter(df, peak_marker),
        mapping = aes(xintercept = !!sym(time_info$column), color = NULL),
        color = "black", linetype = 2
      )
  }

  # peak backgrounds
  # NOT YET IMPLEMENTED
  # note that this requires some scaling (just like the time units) to do it right
  if (peak_bgrd) {
    warning("sorry, peak bgrds are not yet implemented", call. = FALSE, immediate. = FALSE)
  }

  # draw chromatograms
  p <- p + geom_line()

  # peak labels
  if (!quo_is_null(aes_quos$peak_label)) {
    peak_label_filter_quo <- enquo(peak_label_filter)
    has_any_labels <-
      dplyr::filter(df, peak_marker) %>%
      {
        if(!quo_is_null(peak_label_filter_quo))
          dplyr::filter(., !!peak_label_filter_quo)
        else
          .
      } %>%
      nrow()

    if (has_any_labels > 0) {
      p <- p +
        ggrepel::geom_label_repel(
          data = function(df)
            dplyr::filter(df, peak_marker) %>%
            {
              if(!quo_is_null(peak_label_filter_quo))
                dplyr::filter(., !!peak_label_filter_quo)
              else
                .
            },
          mapping = aes_(label = aes_quos$peak_label),
          show.legend = FALSE,
          force = peak_label_repel,
          #box.padding = 1,
          min.segment.length = 0,
          size = peak_label_size,
          segment.color = "black",
          segment.alpha = 0.5,
          segment.size = 0.5,
          direction = "both"
        )
    }
  }

  # zoom ghost points to make sure the zooming frame remains the same (if zoom is set)
  if (zoom) {
    panel_zoom_group <- if (!rlang::quo_is_null(aes_quos$panel)) quo(..panel) else quo(1)

    get_column_names(df, baseline = quo(baseline)) # check that baseline exists
    p <- p +
      geom_point(data = function(df)
        df %>%
          group_by(!!panel_zoom_group) %>%
          summarize(time = mean(!!sym(time_info$column), na.omit = TRUE), value = min(baseline, na.rm = TRUE)) %>%
          dplyr::filter(!is.na(value)),
        mapping = aes(x = time, y = value), inherit.aes = FALSE,
        size = 0, alpha = 1, show.legend = FALSE) +
      geom_point(data = function(df)
        df %>%
          group_by(!!panel_zoom_group) %>%
          summarize(time = mean(!!sym(time_info$column), na.omit = TRUE), value = max(zoom_cutoff, na.rm = TRUE)) %>%
          dplyr::filter(!is.na(value)),
        mapping = aes(x = time, y = value), inherit.aes = FALSE,
        size = 0, alpha = 1, show.legend = FALSE)
  }

  # display full time scale
  if (!is.infinite(df$time_min[1]))
    p <- p + expand_limits(x = df$time_min[1])
  if (!is.infinite(df$time_max[1]))
    p <- p + expand_limits(x = df$time_max[1])

  # normalize plot y axis
  if (normalize)
    p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

  # paneling
  if (!quo_is_null(aes_quos$panel))
    p <- p + facet_grid(..panel ~ ., scales = "free_y")

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
  cycle <- value <- type <- data_wo_units <- NULL

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
      panel_rows = aes_quos$panel %>% rlang::quo_squash() %>% rlang::f_lhs(),
      panel_cols = aes_quos$panel %>% rlang::quo_squash() %>% rlang::f_rhs()))
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
      data_wo_units = data,
      data = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>% {
      data_levels <- tibble::deframe(select(., data, data_wo_units) %>% unique())
      data_sorting <- sapply(select_data, function(x) which(data_levels == x)) %>% unlist(use.names = F)
      mutate(., data = factor(data, levels = names(data_levels)[data_sorting]))
    }

  # generate plot
  group_quos <- list(quo(file_id))
  p <- plot_data %>%
    ggplot() +
    aes(cycle, value) +
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
  if (!quo_is_null(aes_quos$color)) {
    p <- p %+% aes_(color = aes_quos$color)
    group_quos <- c(group_quos, aes_quos['color'])
  }

  # linetype
  if (!quo_is_null(aes_quos$linetype)) {
    p <- p %+% aes_(linetype = aes_quos$linetype)
    group_quos <- c(group_quos, aes_quos['linetype'])
  }

  # shape_by
  if (!quo_is_null(aes_quos$shape)) {
    p <- p %+% aes_(shape = aes_quos$shape)
    group_quos <- c(group_quos, aes_quos['shape'])
  }

  # group quo
  p <- p %+% aes_(group = quo(paste(!!!group_quos)))

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
      ref_peak_nr = 1:dplyr::n()) %>%
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
#' @param size variable to use for size aesthetic for the plot or constant value for the points size
#' @param date_breaks what breaks to use for the x axis if it is a datetime
#' @param date_labels datetime label pattern for x axis if it is a datetime
#' @export
iso_plot_calibration_parameters <- function(dt, calibration = "", x,
                                       select_from_summary = c(R2 = adj.r.squared, RMSD = sigma),
                                       color = NULL, shape = NULL, size = 4,
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
    geom_errorbar(data = function(df) filter(df,!is.na(std.error)),
                  map = aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0) +
    facet_grid(rlang::new_formula(sym("term"), sym(calib_vars$model_name)),
               scales = "free_y", space = "free_x") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(plot.margin = margin(10, 5, 15, 20)) +
    labs(y="")

  # points
  if (!quo_is_null(vis_quos$size)) {
    if (quo_is_symbol(vis_quos$size) || quo_is_call(vis_quos$size))
      p <- p + geom_point(mapping = aes_q(size = vis_quos$size))
    else
      p <- p + geom_point(size = eval_tidy(vis_quos$size))
  } else
    p <- p + geom_point()

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
#' This is a convenience function to visualize the calibration ranges in a plot generated by \code{\link{iso_plot_data}} with a gray area behind the data points. Note that \code{\link{iso_evaluate_calibration_range}} must have been called at an earlier point to establish calibration ranges for different terms. Also note that if one of the chosen dimensions (x or y) is not part of the available calibration range for a panel but the other dimension is, it simply shows two lines bracketing the samples in the dimension that is part of the calibration range.
#' @inheritParams iso_plot_calibration_parameters
#' @param p a plot generated by \code{\link{iso_plot_data}} (usually piped to this function)
#' @export
iso_mark_calibration_range <- function(p, calibration = "") {

  # safety checks
  if (missing(p)) stop("no base plot provided", call. = FALSE)

  # check for model parameters
  calib_vars <- get_calibration_vars(calibration)

  # x (only one that needs to come from plot)
  x <- rlang::as_label(p$mapping$x)

  # range marker calucations
  prepare_range_marker_data <- function(plot_data) {

    # make sure calibration cols are present
    check_calibration_cols(plot_data, c(calib_vars$model_name, calib_vars$model_params))

    # check existence of panel column (introduced by iso_plot_data)
    dt_cols <- get_column_names(plot_data, panel = quo(panel))

    # ys
    ys <- plot_data$panel %>% unique() %>% as.character()

    # needed ranges
    range_req <- tibble(x = x, y = ys, panel = ys) %>%
      crossing(select(plot_data, !!sym(calib_vars$model_name)) %>% unique())

    # model ranges
    range_data <- plot_data %>%
      mutate(panel = as.character(panel)) %>%
      select(panel, !!sym(calib_vars$model_name), !!sym(calib_vars$model_params)) %>%
      unique() %>%
      iso_unnest_calibration_range(keep_other_list_data = FALSE)

    # resulting data frame
    ranges <-
      full_join(
        # x-range
        left_join(range_req, range_data, by = c(calib_vars$model_name, "panel", "x" = "term")) %>%
          select(!!sym(calib_vars$model_name), panel, x, xmin = min, xmax = max),
        # y-range
        left_join(range_req, range_data, by = c(calib_vars$model_name, "panel", "y" = "term")) %>%
          select(!!sym(calib_vars$model_name), panel, y, ymin = min, ymax = max),
        by = c(calib_vars$model_name, "panel")
      ) %>%
      filter(!(is.na(xmin) & is.na(xmax) & is.na(ymin) & is.na(ymax))) %>%
      mutate(
        xmin = ifelse(is.na(xmin), -Inf, xmin),
        xmax = ifelse(is.na(xmax), Inf, xmax),
        ymin = ifelse(is.na(ymin), -Inf, ymin),
        ymax = ifelse(is.na(ymax), Inf, ymax)
      )

    return(ranges)
  }

  # add geom rect for calibration range
  p$layers <-
    c(
      geom_rect(
        data = prepare_range_marker_data,
        mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      x = NULL, y = NULL, color = NULL, fill = NULL,
                      linetype = NULL, label = NULL, shape = NULL),
        color = "black", fill = "grey", alpha = 0.3),
      p$layers
    )

  return(p)
}

#' Plot calibration range
#'
#' This function is deprecated, please use \link{iso_plot_data} and \link{iso_mark_calibration_range} instead.
#' @param ... deprecated
#' @export
iso_plot_calibration_range <- function(...) {
  warning("iso_plot_calibration_range was deprecated as part of a major change in treatment of calibration ranges in isoprocessor version 0.3.8. Please use iso_evaluate_calibration_range to calculate calibration ranges for your terms of interest, iso_plot_data to visualize the data, and iso_mark_calibration_range to highlight the calculated calibration ranges visually.", immediate. = TRUE, call. = FALSE)
}

#' Visualize the data
#'
#' General purpose convenience visualization function. Simply add other ggplot components after calling this function to customize more (e.g. with \link[ggplot2]{facet_wrap} or \link[ggplot2]{theme} calls). Make sure to specify \code{lines = TRUE} and/or \code{points = TRUE} to add the layers.
#'
#' @inheritParams iso_map_peaks
#' @inheritParams iso_plot_calibration_parameters
#' @param y which columns to visualize, combine with c()
#' @param group what to group by, multiple columns allowed (combine with paste(...)), usually not necessary if groupings are fully defined through other aesthetics
#' @param fill variable to use for the fill aesthetic of the plot
#' @param linetype variable to use for linetype aesthetic for the plot
#' @param y_error an error column for drawing y error bars - if multiple \code{y} are provided, error needs to point to the same number of columns
#' @param lines whether to plot lines (FALSE by default)
#' @param points whether to plot points (FALSE by default)
#' @param label this is primarily of use for turning the generated ggplots into interactive plots via \code{\link[plotly]{ggplotly}} as the \code{label} will be rendered as an additional mousover label.
#' @export
#' @note should probably make sure that the default columns for gather 'panel' and 'value' do not exist...
#' @note it would be great to allow renaming of the columns via this (especially the y column)
#' @export
iso_plot_data <- function(dt, x, y, y_error = NULL, group = NULL, color = NULL, fill = NULL, shape = NULL, size = 4, linetype = NULL, label = NULL, lines = FALSE, points = FALSE) {
  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(x)) stop("have to provide an x to plot", call. = FALSE)
  if (missing(y)) stop("have to provide at least one column to plot", call. = FALSE)

  # warnings
  if (!lines && !points) {
    warning("no geoms (points or lines) included, plot will be blank", immediate. = TRUE, call. = FALSE)
  }

  # quos
  vis_quos <-
    list(dt = enquo(dt),
         x = enquo(x), y = enquo(y), y_error = enquo(y_error),
         group = enquo(group),
         color = enquo(color), fill = enquo(fill), shape = enquo(shape),
         linetype = enquo(linetype), size = enquo(size),
         label = enquo(label))

  # check existence of ys and errors
  dt_cols <- get_column_names(!!vis_quos$dt, y = vis_quos$y, y_error = vis_quos$y_error,
                              n_reqs = list(y = "+", y_error = "*"))

  # double check same number of ys and errors
  if (length(dt_cols$y_error) > 0 && length(dt_cols$y) != length(dt_cols$y_error))
    glue("not the same number of y ({collapse(dt_cols$y, sep = ', ')}) and ",
         "y_error columns ({collapse(dt_cols$y_error, sep = ', ')}) provided") %>%
    stop(call. = FALSE)

  # strip units if there are any
  dt <- iso_strip_units(dt)

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
    { if(!quo_is_null(vis_quos$fill)) aes_q(fill = vis_quos$fill) } +
    { if(!quo_is_null(vis_quos$shape)) aes_q(shape = vis_quos$shape) } +
    { if(!quo_is_null(vis_quos$linetype)) aes_q(linetype = vis_quos$linetype) } +
    { if(!quo_is_null(vis_quos$label)) aes_q(label = vis_quos$label) } +
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
    if (!quo_is_null(vis_quos$size)) {
      if (quo_is_symbol(vis_quos$size) || quo_is_call(vis_quos$size))
        p <- p + geom_point(mapping = aes_q(size = vis_quos$size))
      else
        p <- p + geom_point(size = eval_tidy(vis_quos$size))
    } else
      p <- p + geom_point()
  }

  return(p)
}
