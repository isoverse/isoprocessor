# setting table =====

# NOTE: debatable whether reset_missing = TRUE or FALSE should be the default
# went with TRUE to avoid situations of mixed peak table information as an unexpected default
#' Set peak table
#'
#' Set peak table for a collection of isofiles. Peak tables have to have at least a \code{file_id} column but are otherwise flexible. That said, isoprocessor functionality that operates on peak tables assumes that all columns with numbers are \code{\link[isoreader]{iso_double_with_units}} and that retention times are recorded in \code{rt_start}, \code{rt_end}, and \code{rt} (apex retention time) columns.
#'
#' This funciton will issue a warning if the \code{peak_table} has \code{file_id} values that are not part of the \code{iso_files}. It will reset peak tables for all \code{iso_files} whose \code{file_id} is not part of \code{peak_table} unless \code{reset_missing} is set to \code{FALSE}, in which case pre-existing peak table data will be preserved unaltered. To reset all peak tables, simply call this function without the \code{peak_table} parameter.
#'
#' @param iso_files iso file(s) for which to set peak tables
#' @param peak_table peak table data frame, must have a 'file_id' column at minimum (to match with the iso_files)
#' @param reset_missing whether to reset the \code{peak_table} of any \code{iso_files} whose\code{file_id} is not in the \code{peak_table}. If set to \code{FALSE}, will preserve pre-existing peak table data instead.
#' @family peak table functions
#' @export
iso_set_peak_table <- function(iso_files, peak_table = tibble(file_id = character(0)), reset_missing = TRUE, quiet = default(quiet)) {

  # continuous flow file check
  if (!isoreader::iso_is_continuous_flow(iso_files))
    stop("peak table information can only be set for continuous flow files", call. = FALSE)

  if (!is.data.frame(peak_table))
    stop("peak_table must be a data frame", call. = FALSE)

  if (!"file_id" %in% names(peak_table))
    stop("peak_table does not have a 'file_id' column, cannot match to iso_files", call. = FALSE)

  # single file
  is_single <- iso_is_file(iso_files)
  if (is_single) iso_files <- iso_as_file_list(iso_files)

  # split peak_table
  peak_table <- split(peak_table, peak_table$file_id)

  # information
  files_file_ids <- names(iso_files)
  peaks_file_ids <- names(peak_table)
  updates <- intersect(files_file_ids, peaks_file_ids)
  resets <- setdiff(files_file_ids, peaks_file_ids)
  warnings <- setdiff(peaks_file_ids, files_file_ids)

  if (!quiet) {
    glue::glue(
      "Info: setting peak table for {length(updates)}/{length(files_file_ids)} iso files",
      if(reset_missing && length(resets) > 0) "; resetting peak table for {length(resets)} files"
      else if (!reset_missing && length(resets) > 0) "; keeping peak table unchanged for {length(resets)} files"
      else "",
      if (length(warnings) > 0) "; ignoring peak table data for {length(warnings)} unmatched file IDs"
      else "",
      "."
    ) %>% message()
  }

  # update iso files
  iso_files <- map(
    iso_files,
    ~{
      if (.x$file_info$file_id %in% updates)
        .x$peak_table <- dplyr::select(peak_table[[.x$file_info$file_id]], -file_id)
      else if (reset_missing && .x$file_info$file_id %in% resets)
        .x$peak_table <- tibble()
      .x
    }
  )

  if (is_single) return(iso_files[[1]])
  else return(iso_as_file_list(iso_files))
}

# setting from vendor data table =====

#' Set peak table from vendor data
#'
#' Set the peak table from vendor data in each iso_file. Only the vendor data table columns that do exist in a file will be used, so this function can set peak table information across a variety of data files without errors. Provides a detailed info message about the columns that are used.
#'
#' @param direct_rename select which columns to select and rename directly from the vendor data table. Must be a named vector with the values the vendor data table column names and the names the corresponding peak table column names.
#' @param regex_rename select columns by regular expression and rename using \link[stringr]{str_replace}. Must be a named vector with the values the regular expression to find vendor data table columns and the names the replacement expression for the correponding peak table column names.
#' @family peak table functions
#' @export
iso_set_peak_table_from_vendor_data_table <- function(iso_files, direct_rename = c(), regex_rename = c(), quiet = default(quiet)) {

  # continuous flow file check
  if (!isoreader::iso_is_continuous_flow(iso_files))
    stop("peak table information can only be set for continuous flow files", call. = FALSE)

  # parameter checks
  if (length(direct_rename) > 0 && (!is.character(direct_rename) || !rlang::is_named(direct_rename)))
    stop("direct_rename must be a named character vector", call. = FALSE)
  if (length(regex_rename) > 0 && (!is.character(regex_rename) || !rlang::is_named(regex_rename)))
    stop("regex_rename must be a named character vector", call. = FALSE)
  if (length(direct_rename) == 0 && length(regex_rename) == 0)
    stop("must provide either direct_rename or regex_rename", call. = FALSE)

  # single file
  is_single <- iso_is_file(iso_files)
  if (is_single) iso_files <- iso_as_file_list(iso_files)

  # pull out all vendor data table columns
  cols <- tibble(
    file_id = names(iso_files),
    col = purrr::map(
      iso_files, ~
        if(is.null(.x$vendor_data_table)) { character(0) }
      else { names(.x$vendor_data_table) }
    )
  ) %>%
    tidyr::unnest(col)

  # single cols
  if (length(direct_rename) > 0)
    direct_rename <- tibble(
      fits = map(as.character(direct_rename), ~dplyr::filter(cols, col == .x)),
      new = names(direct_rename)
    ) %>% tidyr::unnest(fits)
  else
    direct_rename <- tibble()

  # regex cols
  if (length(regex_rename) > 0)
    regex_rename <- tibble(
      fits = map2(
        as.character(regex_rename),
        names(regex_rename),
        ~ dplyr::filter(cols, stringr::str_detect(col, .x)) %>%
          dplyr::mutate(new = stringr::str_replace(col, .x, .y))
      )
    ) %>% tidyr::unnest(fits)
  else
    regex_rename <- tibble()

  # all renames
  all_cols <- vctrs::vec_rbind(direct_rename, regex_rename) %>%
    mutate(idx = dplyr::row_number())

  # info
  if (!quiet) {
    info <- all_cols %>%
      group_by(file_id) %>%
      summarize(label = paste(sprintf("'%s'->'%s'", col, new), collapse = ", ")) %>%
      dplyr::count(label) %>%
      mutate(label = sprintf(" - for %d file(s): %s", n, label)) %>%
      arrange(desc(n))

    glue::glue("Info: setting peak table from vendor data table with the following renames:\n",
               paste(info$label, collapse = "\n")) %>%
      message()
  }

  # generate mutates to account for potential column replication
  # (maybe do this faster with mutates only where necessary?)
  all_cols <- all_cols %>%
    mutate(col_quo = purrr::map(col, ~sym(.x))) %>%
    arrange(file_id, idx) %>%
    { split(., .$file_id) }

  # update iso files
  iso_files <- map(
    iso_files,
    ~{
      if (.x$file_info$file_id %in% names(all_cols)) {
        mutate_quos <- with(all_cols[[.x$file_info$file_id]], setNames(col_quo, new))
        .x$peak_table <- dplyr::mutate(.x$vendor_data_table, !!!mutate_quos) %>%
          dplyr::select(!!!all_cols[[.x$file_info$file_id]]$new)
      }
      .x
    })

  if (is_single) return(iso_files[[1]])
  else return(iso_as_file_list(iso_files))

}

#' @rdname iso_set_peak_table_from_vendor_data_table
#' @aliases iso_set_peak_table_from_isodat_vendor_data_table
#' @details \code{iso_set_peak_table_from_isodat_vendor_data_table} provides specialized functionality to set peak table information from an Isodat vendor data tables. For compatibility with all downstream isoprocessor calculations, the resulting peak table has a very specific set of columns which are listed below. Mapping for Isodat data tables:
#' \itemize{
#'   \item peak_nr: "Nr."
#'   \item is_ref: "Is Ref.?"
#'   \item rt_start: "Start"
#'   \item rt: "Rt"
#'   \item rt_end: "End"
#'   \item ampX: "Ampl X"
#'   \item bgrdX_start: "BGD X"
#'   \item bgrdX_end: "BGD X" (Isodat only reports a single background amplitude)
#'   \item areaX: "rIntensity X" (recorded intensities)
#'   \item rX/Y: "rR X/Y" (recorded ratios)
#'   \item rX/Y_ref: extrapolated reference ratio at the peak (not available from Isodat peak table)
#'   \item rdX/Y: "rd X/Y" (recorded delta -> rX/Y / rX/Y_ref - 1, not shifted for true isotopic value of the ref)
#'   \item dX/Y: "d X/Y" (delta frame shifted wrt to the true isotopic value of the ref; note that if this is a single element delta keeps just the numerator with the element symbol to fit conventional naming, e.g. d13C or d18O; whereas for a molecule omits the formula and keeps the mass ratios, e.g. d45/44)
#'   \item atX: "AT%% X/Y"
#' }
#' @export
iso_set_peak_table_from_isodat_vendor_data_table <- function(iso_files, quiet = default(quiet)) {

  direct_rename <- c(peak_nr = "Nr.", is_ref = "Is Ref.?",
                   rt_start = "Start", rt = "Rt", rt_end = "End")
  regex_rename <- c("amp\\1" = "^Ampl (\\d+)$",
                  "bgrd\\1_start" = "^BGD (\\d+)$", "bgrd\\1_end" = "^BGD (\\d+)$",
                  "area\\1" = "^rIntensity (\\d+)$",
                  "r\\1/\\2" = "^rR (\\d+)[^/]*\\/(\\d+).*$",
                  "rd\\1/\\2" = "^rd (\\d+)[^/]*\\/(\\d+).*$",
                  "d\\1/\\2" = "^d (\\d+)[^0-9][^/]+\\/(\\d+).*$",
                  "d\\1" = "^d (\\d+[^/])\\/(\\d+).*$",
                  "at\\1" = "^AT\\% (\\d+[^/]*)\\/(\\d+).*$")

  iso_set_peak_table_from_vendor_data_table(
    iso_files,
    direct_rename = direct_rename,
    regex_rename = regex_rename,
    quiet = quiet
  )
}

# mutate peak table =====

#' Mutate peak table
#'
#' Mutate the peak table (\code{\link{iso_get_peak_table}}) within isofile objects by changing existing columns or introducing new ones. Works just like dplyr's \link[dplyr]{mutate}.
#'
#' @inheritParams isoreader::iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{mutate} conditions applied to the combined peak_table (see \code{\link{iso_get_peak_table}})
#' @family peak table functions
#' @export
iso_mutate_peak_table <- function(iso_files, ..., quiet = default(quiet)) {
  UseMethod("iso_mutate_peak_table")
}

#' @export
iso_mutate_peak_table.default <- function(iso_files, ..., quiet = default(quiet)) {
  stop("this function is not defined for objects of type '",
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_mutate_peak_table.iso_file <- function(iso_files, ..., quiet = default(quiet)) {
  iso_mutate_peak_table(iso_as_file_list(iso_files), ..., quiet = quiet)[[1]]
}

#' @export
iso_mutate_peak_table.iso_file_list <- function(iso_files, ..., quiet = default(quiet)) {

  # continuous flow file check
  if (!isoreader::iso_is_continuous_flow(iso_files))
    stop("peak tables can only exist in continuous flow files", call. = FALSE)

  # information
  if (!quiet) {
    glue::glue("Info: mutating peak table for {length(iso_files)} data file(s)") %>%
      message()
  }

  # mutate iso_files' file info
  peak_table <- iso_get_peak_table(iso_files, quiet = TRUE)
  original_cols <- names(peak_table)
  mutate_quos <- rlang::enquos(...)
  new_cols <- names(mutate_quos)
  peak_table <- dplyr::mutate(peak_table, !!!mutate_quos)
  split_peak_table <- split(peak_table, peak_table$file_id)

  # mutate this way to ensure that only original columns are kept rather than
  # a bunch of NA columns if peak_tables differ between iso_files
  iso_files <- map(
    iso_files,
    ~{
      if (!is.null(.x$peak_table)) {
        original_cols <- names(.x$peak_table)
        .x$peak_table <- split_peak_table[[.x$file_info$file_id]][unique(c(original_cols, new_cols))]
      }
      .x
    })

  # return
  return(iso_as_file_list(iso_files))
}

# aggregation ======

#' Aggregate peak table
#'
#' Aggregate peak table from continous flow files. The return peak table has implicit units, to make the units explicit, use \code{\link[isoreader]{iso_make_units_explicit}}.
#'
#' @inheritParams isoreader::iso_get_vendor_data_table
#' @family peak table functions
#' @export
iso_get_peak_table <- function(
  iso_files, select = everything(), include_file_info = NULL, with_explicit_units = FALSE, quiet = default(quiet)) {

  # parameters
  iso_files <- isoreader::iso_as_file_list(iso_files)
  include_file_info_quo <- rlang::enquo(include_file_info)

  # continuous flow file check
  if (!isoreader::iso_is_continuous_flow(iso_files))
    stop("peak table information is only available in continuous flow files", call. = FALSE)

  # info message
  if (!quiet) {
    sprintf(
      "Info: aggregating peak table%s from %d data file(s)%s",
      if (with_explicit_units) " with explicit units" else "",
      length(iso_files),
      isoreader:::get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>%
      message()
  }

  # check whether there are any files
  if (length(iso_files) == 0) return(tibble())

  # get vendor data
  column <- units <- NULL # global vars

  # fetch peak table data
  peak_table <-
    # fetch data
    tibble(
      file_id = names(iso_files),
      pt = map(iso_files, ~.x$peak_table),
      has_pt = map_lgl(pt, ~!is.null(.x))
    )

  # warnings
  if (all(!peak_table$has_pt)) {
    warning("none of the iso files has a peak_table yet. To use peak table functionality, make sure to create a peak table by either adopting the vendor_data_table (?iso_set_peak_table_from_vendor_data_table), finding peaks (?iso_find_peaks), or setting a table manually (?iso_set_peak_table).", call. = FALSE, immediate. = TRUE)
  }

  # make sure to include only existing data
  peak_table <- filter(peak_table, has_pt)

  # check for any rows
  if (nrow(peak_table) == 0) return(tibble(file_id = character(0)))

  # make units explicit if wanted
  if (with_explicit_units) {
    peak_table <- peak_table %>%
      mutate(pt = map(pt, isoreader::iso_make_units_explicit))
  }

  # unnest
  peak_table <- dplyr::select(peak_table, file_id, pt) %>% unnest(pt)

  # get include information
  select_cols <- get_column_names(peak_table, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols)
    select_cols <- c("file_id", select_cols) # file info always included

  # focus on selected columns only (also takes care of the rename)
  peak_table <- dplyr::select(peak_table, !!!select_cols)

  # include file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    peak_table <- right_join(info, peak_table, by = "file_id")
  }

  return(peak_table)

}


