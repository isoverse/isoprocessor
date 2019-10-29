# peak finding algorithms =====

#' Find chromatographic peaks
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @export
iso_find_peaks <- function(...) {
  UseMethod("iso_find_peaks")
}

#' @export
iso_find_peaks.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_find_peaks.iso_file <- function(iso_files, ...) {
  iso_find_peaks(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_find_peaks
#' @param iso_files collection of continuous flow iso_file objects
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_find_peaks.iso_file_list <- function(iso_files, ..., quiet = default(quiet)) {

  # only dual inlet supported for now
  if(!iso_is_continuous_flow(iso_files))
    stop("can only find peaks in continuous flow files", call. = FALSE)

  # find peaks
  peak_table <-
    iso_get_raw_data(iso_files, quiet = TRUE) %>%
    iso_find_peaks(...)

  # set peak table
  return(iso_set_peak_table(iso_files, peak_table, quiet = TRUE))
}

#' @rdname iso_find_peaks
#' @param df a data frame of raw continuouus flow data, must have at minimum the columns 'file_id'
#' @export
iso_find_peaks.data.frame <- function(df, quiet = default(quiet)) {
  # stop, not implemented yet
  stop("sorry, peak finding algorightms are not yet implemented", call. = FALSE)
}

# combining raw and chromatographic data =====

#' Generate chromatographic data with peak table information.
#'
#' This function combines chromatogram and peak table information for various downstream calculations (e.g. peak areas, backgrounds).
#'
#' @param raw_data the raw chromatographic data. If in long format, \code{data_trace} must be set to identify peaks correctly with the individual data traces.
#' @param peak_table a data frame that describes the peaks in this chromatogram. Peaks must have at minimum an apex retention time or a start and end retention time (ideally all 3). If no apex retention time is provided, peak marker points cannot be identified. If not both start and end retention time are provided, peak start and end are identified to lie right before and after the apex point.
#' @param file_id the column (or columns) that uniquely identifies individual analyses for proper matching of the raw chromatography data with the peak_table data. In most cases dealing with isoreader data, the default (\code{file_id}) should work fine.
#' @param rt the column in the \code{peak_table} that holds the retention time of the peak.
#' @param rt_start the column in the\code{peak_table} that holds the retention time where each peak starts.
#' @param rt_end the column in the\code{peak_table} that holds the retention time where each peak ends.
#' @param rt_unit which time unit the retention time columns (\code{rt}, \code{rt_start}, \code{rt_end}) are in. If not specified (i.e. the default), assumes it is the same unit as the time column of the raw data.
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
    !!peak_table_quo, file_id = enquo(file_id), rt = enquo(rt), rt_start = enquo(rt_start), rt_end = enquo(rt_end),
    n_reqs = list(file_id = "+"), type_reqs = list(rt = "numeric", rt_start = "numeric", rt_end = "numeric"))

  # file_id quos
  file_id_quos <- purrr::map(raw_data_cols$file_id, sym)

  # deal with time units (make sure they are all in the plot data time units)
  time_info <- find_time_column(raw_data)
  rt_unit <- if(is.null(rt_unit)) time_info$unit else rt_unit
  peak_table <- peak_table %>%
    dplyr::mutate(
      # note that this peak ID goes across ALL files, it's truly unique,
      # NOT just a counter within each file - this should  make processing faster
      ..peak_id = dplyr::row_number(),
      ..rt = scale_time(!!sym(peak_table_cols$rt), to = time_info$unit, from = rt_unit),
      ..rt_start = scale_time(!!sym(peak_table_cols$rt_start), to = time_info$unit, from = rt_unit),
      ..rt_end = scale_time(!!sym(peak_table_cols$rt_end), to = time_info$unit, from = rt_unit)
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
