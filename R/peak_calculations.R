# combining raw and chromatographic data =====

#' Generate chromatographic data with peak table information.
#'
#' This function combines chromatogram and peak table information and is used internally in several plotting and peak table calculation functions. Usually it is not called directly by the user but may be of use for extending functionality and is hence provided as a stand-alone function.
#'
#' @param raw_data the raw chromatographic data. If in long format, \code{data_trace} must be set to identify peaks correctly with the individual data traces.
#' @param peak_table a data frame that describes the peaks in this chromatogram. Peaks must have at minimum an apex retention time or a start and end retention time (ideally all 3). If no apex retention time is provided, peak marker points cannot be identified. If not both start and end retention time are provided, peak start and end are identified to lie right before and after the apex point.
#' @param file_id the column (or columns) that uniquely identifies individual analyses for proper matching of the raw chromatography data with the peak_table data. In most cases dealing with isoreader data, the default (\code{file_id}) should work fine.
#' @param rt the column in the \code{peak_table} that holds the retention time of the peak.
#' @param rt_start the column in the\code{peak_table} that holds the retention time where each peak starts.
#' @param rt_end the column in the\code{peak_table} that holds the retention time where each peak ends.
#' @param rt_unit which time unit the retention time columns (\code{rt}, \code{rt_start}, \code{rt_end}) are in. Only required if the retention time columns are not \code{\link[isoreader]{iso_double_with_units}} columns and are not in the same unit as the time column of the raw data. If provided, will override column units.
#' @param data_trace expression to identify individual data traces for raw data in long format. This is not usually needed for calculations but can be useful when working with data frames already gathered for plotting. If the data is indeed in gathered (long) format, and this is not set correctly, peak table data is integrated across all traces in the raw data leading to unpredictable peak border and apex identifications.
#' @export
iso_combine_raw_data_with_peak_table <- function(
  raw_data, peak_table,
  file_id = default(file_id), data_trace = NULL,
  rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end),
  rt_unit = NULL) {

  # safety checks
  peak_table_quo <- enquo(peak_table)
  if (missing(raw_data) || missing(peak_table)) stop("must supply raw_data and peak_table", call. = FALSE)
  if (nrow(peak_table) == 0) stop("peaks table supplied but there is no peaks data in it", call. = FALSE)

  # find raw data columns
  raw_data_cols <- isoreader:::get_column_names(
    raw_data, file_id = enquo(file_id), n_reqs = list(file_id = "+"))
  data_trace_quo <- enquo(data_trace)
  isoreader:::check_expressions(raw_data, data_trace_quo)
  if (rlang::quo_is_null(data_trace_quo)) data_trace_quo <- quo("all")

  # find peak table columns
  peak_table_cols <- isoreader:::get_column_names(
    peak_table, file_id = enquo(file_id), rt = enquo(rt), rt_start = enquo(rt_start), rt_end = enquo(rt_end),
    n_reqs = list(file_id = "+"))

  # make sure retention columns are numeric (any numeric is okay)
  if (!all(purrr::map_lgl(peak_table[with(peak_table_cols, c(rt, rt_start, rt_end))], is.numeric)))
      stop("retention time columns must be numeric", call. = FALSE)

  # file_id quos
  file_id_quos <- purrr::map(raw_data_cols$file_id, sym)

  # find time units
  time_info <- find_time_column(raw_data)
  if (is.null(rt_unit)) {
    # find rt unit (none provided)
    time_units <- c()
    if (iso_is_double_with_units(peak_table[[peak_table_cols$rt]]))
      time_units <- c(time_units, iso_get_units(peak_table[[peak_table_cols$rt]]))
    if (iso_is_double_with_units(peak_table[[peak_table_cols$rt_start]]))
      time_units <- c(time_units, iso_get_units(peak_table[[peak_table_cols$rt_start]]))
    if (iso_is_double_with_units(peak_table[[peak_table_cols$rt_end]]))
      time_units <- c(time_units, iso_get_units(peak_table[[peak_table_cols$rt_end]]))
    if (length(time_units) > 0) {
      if(!all(time_units == time_units[1]))
        # note could in theory resolve each one individually but different time units
        # in the retention times is a mess no matter what
        glue::glue(
          "inferring retention time units from peak table columns but encountered ",
          "conflicting units: {paste(unique(time_units), collapse = ', ')}") %>%
        stop(call. = FALSE)
      else
        rt_unit <- time_units[1]
    } else
      rt_unit <- time_info$unit
  }

  # deal with time units (make sure they are all in the plot data time units)
  peak_table <- peak_table %>%
    dplyr::mutate(
      # note that this peak ID goes across ALL files, it's truly unique,
      # NOT just a counter within each file - this should  make processing faster
      ..peak_id = dplyr::row_number(),
      ..rt = scale_time(as.numeric(!!sym(peak_table_cols$rt)), to = time_info$unit, from = rt_unit),
      ..rt_start = scale_time(as.numeric(!!sym(peak_table_cols$rt_start)), to = time_info$unit, from = rt_unit),
      ..rt_end = scale_time(as.numeric(!!sym(peak_table_cols$rt_end)), to = time_info$unit, from = rt_unit)
    )

  # safety check on retention times
  if (filter(peak_table, !is.na(..rt_start) & !is.na(..rt_end) & ..rt_start > ..rt_end) %>% nrow() > 0)
    stop("impossible peak definition: some peaks in the peak table have start times after their end times", call. = FALSE)
  if (filter(peak_table, !is.na(..rt) & !is.na(..rt_end) & ..rt_end < ..rt) %>% nrow() > 0)
    stop("impossible peak definition: some peaks in the peak table have end times before their apex times", call. = FALSE)
  if (filter(peak_table, !is.na(..rt) & !is.na(..rt_start) & ..rt_start > ..rt) %>% nrow() > 0)
    stop("impossible peak definition: some peaks in the peak table have start times after their apex times", call. = FALSE)
  if (filter(peak_table, is.na(..rt) & (is.na(..rt_start) | is.na(..rt_end))) %>% nrow() > 0)
    stop("impossible peak definition: not enough information (missing apex rt and missing peak start & end rt)", call. = FALSE)

  # kep columns in the raw data
  raw_data <- raw_data %>%
    dplyr::mutate(
      ..data_id = dplyr::row_number(),
      ..time = !!sym(time_info$column),
      ..data = as.character(!!data_trace_quo)
    )

  # combine raw data with peaks
  raw_data_peaks_all <-
    # all possible combinations in each file
    dplyr::inner_join(
      dplyr::select(raw_data, !!!file_id_quos, ..data_id, ..time, ..data),
      dplyr::select(peak_table, !!!file_id_quos, ..peak_id, ..rt, ..rt_start, ..rt_end),
      by = raw_data_cols$file_id
    ) %>%
    # right away remove peaks that are outside the available raw data time range
    dplyr::group_by(!!!file_id_quos) %>%
    dplyr::filter(is.na(..rt) | (..rt >= min(..time, na.rm = TRUE) & ..rt <= max(..time, na.rm = TRUE))) %>%
    dplyr::ungroup()

  raw_data_peaks <-
    raw_data_peaks_all %>%
    # identify the location of the peak marker
    dplyr::group_by(..data, ..peak_id) %>%
    dplyr::mutate(peak_marker = if (length(..rt) == 0) NA else abs(..rt - ..time) == min(abs(..rt - ..time))) %>%
    dplyr::ungroup() %>%
    # remove everything that doesn't actually belong to a peak
    dplyr::filter(peak_marker | (..time >= ..rt_start & ..time <= ..rt_end)) %>%
    dplyr::select(..data_id, ..peak_id, peak_marker)

  # safety checks
  all_peaks <- unique(raw_data_peaks_all$..peak_id)
  missing <- setdiff(all_peaks, unique(raw_data_peaks$..peak_id))
  if (length(all_peaks) == 0) {
    glue::glue(
      "there are no peaks that fit into the time window of the chromatogram. ",
      "Please double check that the rt_unit parameter is set to correctly identify ",
      "the time units of the peak table - currently assumed to be '{rt_unit}'.") %>%
      warning(immediate. = TRUE, call. = FALSE)
  }
  if (length(missing) > 0) {
    # can this ever happen?
    glue::glue(
      "only {length(all_peaks) - length(missing)} of the {length(all_peaks)} ",
      "applicable peaks in the peak table could be identified in the chromatogram") %>%
      warning(immediate. = TRUE, call. = FALSE)
  }

  # add the peak information to the chromatogram
  all_data <- raw_data %>%
    dplyr::left_join(raw_data_peaks, by = "..data_id") %>%
    # find peak delimiters
    dplyr::arrange(..time) %>% # sorting is important!
    dplyr::group_by(!!!file_id_quos, ..data) %>%
    dplyr::mutate(
      peak_marker = !is.na(peak_marker) & peak_marker,
      peak_point = ifelse(!is.na(..peak_id), ..peak_id, 0), # makes it possible to identify peak clusters afterwards
      peak_start = c(0, diff(peak_point)) > 0 | c(diff(peak_point), 0) > 0,
      #c(0, diff(peak_point)) > 0 | c(diff(peak_point), 0) > 0,
      peak_end =
        c(diff(peak_point), 0) < 0 | c(0, diff(peak_point)) < 0 |
        # take care of case when two peaks touch and there is no 0 in between
        (peak_start & peak_point > 0 & c(0, peak_point[-length(peak_point)]) > 0 & c(peak_point[-1], 0) > 0)
      #c(diff(peak_point), 0) > 0 | c(0, diff(peak_point)) > 0,
    ) %>%
    dplyr::ungroup()

  # add peak info
  all_data <- all_data %>%
    dplyr::left_join(
      dplyr::select(peak_table, raw_data_cols$file_id, "..peak_id",
                    # ensure there are no column duplications
                    names(peak_table) %>% { .[!.%in% names(all_data)] }),
      by = c(raw_data_cols$file_id, "..peak_id")) %>%
    dplyr::arrange(..data_id) %>% # return to original sorting
    dplyr::select(-starts_with(".."))

  return(all_data)
}


# integrate peaks =====

# should be an S3 method that works for iso_files and combination of raw_data and peak_table
# calculates peak areas, peak heights, backgrounds, etc.

#' Integrate peak table peaks
#'
#' Not yet implemented.
#'
#' @export
iso_integrate_peaks <- function(...) {
  stop("sorry, not yet implemented", call. = FALSE)
}

# calculate peak ratios ====

# should be S3 method for iso_files and peak_table data frames

#' Calculate peak ratios
#'
#' Not yet implemented.
#'
#' @export
iso_calculate_peak_ratios <- function(...) {
  stop("sorry, not yet implemented", call. = FALSE)
}

# calculate peak deltas =====

#' Calculate peak deltas
#'
#' Not yet implemented
#'
#' @export
iso_calculate_peak_delats <- function(...) {
  stop("sorry, not yet implemented", call. = FALSE)
}

