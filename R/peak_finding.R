# peak finding algorithms =====

#' Find chromatographic peaks
#'
#' NOT YET IMPLEMENTED.
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

