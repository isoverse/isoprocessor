# peak mapping ======

#' Map peaks based on retention time
#'
#' This function makes it easy to map peaks based on pre-defined peak maps. It is implemented to work on \code{iso_files} as well as directly on \code{peak_table} data frames. It reports all peaks including missing peaks and ambiguous peaks by adding a set of information columns for each entry (\code{is_identified}, \code{is_missing}, \code{is_ambiguous}, \code{n_matches}, \code{n_overlapping}). For routine downstream data processing, this function is usually followed by \code{\link{iso_summarize_peak_mappings}} and \code{\link{iso_get_problematic_peak_mappings}} to inspect the problematic peaks and \code{\link{iso_remove_problematic_peak_mappings}} to proceed only with mapped peaks that are clearly identified. Note that without this filter, one must proceed with great caution interpreting the ambiguous peaks. Also note that if the \code{compound} column already exists in \code{peak_table}, it will be overwritten with the new mappings from the peak maps but will issue a warning that this is happening.
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @export
iso_map_peaks <- function(...) {
  UseMethod("iso_map_peaks")
}

#' @export
iso_map_peaks.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_map_peaks.iso_file <- function(iso_files, ...) {
  iso_map_peaks(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_map_peaks
#' @param iso_files collection of continuous flow iso_file objects
#' @param peak_maps data frame with the peak map(s). At minimum, this data frame must have a \code{compound} and \code{rt} column but may have additional information columns. If multiple peak maps are provided, the \code{peak_table} data frame requires a \code{map_id} column to identify which peak map should be used and the peak maps data frame must have a \code{rt:<map_id>} column for each used value of \code{map_id}. The names of all these columns can be changed if necessary using the \code{compound}, code{rt} and \code{map_id} parameters.
#' @param map_id the column that indicates which map to use for which file (only necessary if multiple peak maps are used)
#' @param compound the column in peak_maps that holds compound information
#' @param rt the column in peak_table and colum prefix in peak_maps ("rt:...") that holds retention time information
#' @param rt_start the column in peak_table that holds start of peak retention times
#' @param rt_end the column in peak_table that holds end of peak retention times
#' @param rt_prefix_divider the divider after the retention time column prefix in peak_maps to identify the map id values (e.g. "rt:map_id_value")
#' @inheritParams iso_show_default_processor_parameters
#' @return data frame with mapped peaks and the following information columns:
#' \itemize{
#' \item{\code{peak_info}: }{a label for the peak with its name and retention time plus indicators of any ambiguity in identification in the form of \code{'?'} for either compound name or retention time for an expected peak that was not found}
#' \item{\code{is_identified}: }{a logical TRUE/FALSE indicating peaks that have been successfully identified (includes missing peaks from the peak map!) (note that this information could also be derived from !is.na(compound) but is provided for convenience)}
#' \item{\code{is_missing}: }{a logical TRUE/FALSE indicating peaks that are in the peak map definition but have no matching peak}
#' \item{\code{is_ambiguous}: }{a logical TRUE/FALSE indicating peaks that are ambiguous in their definition either because they have multiple matches or because they overlap with other, overlapping peaks that were identified the same (note that this information could also be derived from n_overlapping > 1 | n_matches > 1 but is provided for convenience)}
#' \item{\code{n_matches}: }{the number of matches each peak has in the peak map}
#' \item{\code{n_overlapping}: }{the number of overlapping peaks that match the same peak definition}
#' }
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_map_peaks.iso_file_list <- function(
  iso_files, peak_maps, map_id = default(map_id), compound = default(compound),
  rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end), rt_prefix_divider = ":",
  quiet = default(quiet)) {

  # continuous flow file check
  if (!isoreader::iso_is_continuous_flow(iso_files))
    stop("peak tables can only exist in continuous flow files", call. = FALSE)

  # peak_table
  map_id_quo <- resolve_defaults(enquo(map_id))
  peak_table <- iso_get_peak_table(iso_files, quiet = TRUE)
  if (nrow(peak_table) == 0) return(iso_files)

  # see if map id column comes from file info
  file_info <- iso_get_file_info(iso_files, select = !!map_id_quo, quiet = TRUE)
  file_info_cols <- names(file_info) %>% stringr::str_subset(fixed("file_id"), negate = TRUE)
  if (length(file_info_cols) > 0) {
    peak_table <- dplyr::left_join(file_info, peak_table, by = "file_id")
  }

  # map peaks
  mapped_peak_table <-
    peak_table %>%
    iso_map_peaks(
      peak_maps = peak_maps,
      file_id = file_id,
      map_id = !!map_id_quo,
      compound = !!enquo(compound),
      rt = !!enquo(rt),
      rt_start = !!enquo(rt_start),
      rt_end = !!enquo(rt_end),
      rt_prefix_divider = rt_prefix_divider,
      quiet = quiet
    )

  # remove extra file info columns again
  if (length(file_info_cols) > 0) {
    mapped_peak_table <- dplyr::select(mapped_peak_table, !!!map(file_info_cols, ~quo(-!!sym(.x))))
  }

  # assign peak table (note: go for direct assigment even if it generates some
  # NAs in columns that differ between iso_files, at this point if files are
  # processed together they should have similar enough peak tables and it's too
  # risky potentially missing an updated column)
  return(iso_set_peak_table(iso_files, mapped_peak_table, quiet = TRUE))
}

#' @param peak_table data frame with the peak table
#' @param file_id the column(s) in peak_table that uniquely identify a file/set of peaks that belong together
#' @rdname iso_map_peaks
#' @family peak mapping functions
#' @export
iso_map_peaks.data.frame <- function(
  peak_table, peak_maps, file_id = default(file_id), map_id = default(map_id), compound = default(compound),
  rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end), rt_prefix_divider = ":",
  quiet = default(quiet)) {

  # safety checks
  if (missing(peak_maps)) stop("no peak map(s) supplied", call. = FALSE)
  if (nrow(peak_table) == 0) stop("no data in the peak table", call. = FALSE)

  # find regular peak_table columns
  peak_table_cols <- isoreader:::get_column_names(
    peak_table, file_id = enquo(file_id), rt = enquo(rt), rt_start = enquo(rt_start), rt_end = enquo(rt_end),
    n_reqs = list(file_id = "+"))

  # find peak_table map_id column
  map_quo <- resolve_defaults(enquo(map_id))
  peak_table_cols$map_id <- tidyselect::vars_select(names(peak_table), map_id = !!map_quo, .strict = FALSE)
  if (length(peak_table_cols$map_id) > 1)
    glue::glue("map id must be stored in a single column, not: '{paste(peak_table_cols$map_id, collapse = \"', '\")}'") %>%
    stop(call. = FALSE)

  # find peak_table compound column
  compound_quo <- resolve_defaults(enquo(compound))
  peak_table_cols$compound <- tidyselect::vars_select(names(peak_table), compound = !!compound_quo, .strict = FALSE)

  # find peak map columns
  pm_cols <- isoreader:::get_column_names(!!enquo(peak_maps), compound = compound_quo)

  # find new columns
  new_cols <- isoprocessor:::get_new_column_names(peak_info = quo(peak_info), is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous), n_matches = quo(n_matches), n_overlapping = quo(n_overlapping))

  # deal with pre-existing compound column
  n_overwritten <- 0L
  if (length(peak_table_cols$compound) > 0) {
    n_overwritten <- sum(!is.na(peak_table[[peak_table_cols$compound]]))
    peak_table <- select(peak_table, -!!sym(peak_table_cols$compound))
  }

  # read peak maps
  if (peak_maps %>% select(starts_with(peak_table_cols$rt)) %>% ncol() == 0) {
    glue::glue("peak maps do not have any columns that match or start with the ",
               "provided retention time column name '{peak_table_cols$rt}'. ",
               "Available columns: {paste(names(peak_maps), collapse = ', ')}") %>%
      stop(call. = FALSE)
  }

  maps <- peak_maps %>%
    # ignore empty rows (safety precaution)
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    # add NA compound to map undefined peaks
    vctrs::vec_rbind(tibble(compound = NA_character_)) %>% unique() %>%
    # gather map retention times
    gather(..map_id.., ..rt_target.., starts_with(peak_table_cols$rt)) %>%
    # replace the rt prefix to get to the actual map name
    mutate(..map_id.. := str_replace(..map_id.., fixed(str_c(peak_table_cols$rt, rt_prefix_divider)), "")) %>%
    # only keep the NA compound and everything that has a ..rt_target..
    filter(is.na(!!sym(pm_cols$compound)) | !is.na(..rt_target..))

  # check map id type
  map_ids <- unique(maps$..map_id..)
  if (length(map_ids) == 1 && str_detect(map_ids, str_c("^", peak_table_cols$rt, "$"))) {
    # single map
    peak_table <- peak_table %>% mutate(..map_id.. = !!peak_table_cols$rt)
    multiple_maps <- FALSE
  } else if (length(map_ids) == 1 && length(peak_table_cols$map_id) == 0) {
    glue::glue(
      "map id defined ('{map_ids}') ",
      "but the '{quo_text(map_quo)}' column does not exist in the data frame.") %>%
      stop(call. = FALSE)
  } else if (length(map_ids) >= 1 && length(peak_table_cols$map_id) == 0) {
    # multiple maps but no map id defined
    glue::glue(
      "more than one map defined ({paste(map_ids, collapse = ', ')}) ",
      "but the '{quo_text(map_quo)}' column does not exist in the maps data frame.") %>%
      stop(call. = FALSE)
  } else {
    # all in order
    peak_table <- peak_table %>% rename(..map_id.. = !!sym(peak_table_cols$map_id))
    multiple_maps <- TRUE
  }

  # safety check --> look for data entries without peak maps
  if ( nrow(missing <- filter(peak_table, is.na(..map_id..))) > 0 ) {
    glue::glue(
      "cannot proceed - {nrow(missing)} data table entries do not have a ",
      "map defined in the '{peak_table_cols$map_id}' column. Make sure to remove ",
      "entries with missing map metadata first.") %>%
      stop(call. = FALSE)
  }

  # safety check --> look for missing maps
  if (length(missing <- setdiff(unique(peak_table$..map_id..), maps$..map_id..)) > 0) {
    glue::glue(
      "the following maps are referenced in the data table but do not exist in ",
      "the peak maps (make sure that the 'rt' and 'rt_prefix_divider' parameters ",
      "are set correctly): '{collapse(missing, \"', '\")}'. Available peak maps: ",
      "'{collapse(unique(maps$..map_id..), \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combine peak map data and file data
  found_peaks <-
    maps %>%
    # right join to keep map columns first
    right_join(
      # add unique id per peak for identification simplicity
      peak_table %>% mutate(..peak_id.. = 1:dplyr::n()),
      by = "..map_id.."
    ) %>%
    # find the peak that the retention time window matches
    mutate(
      ..is_target.. = is.na(..rt_target..) | (!!sym(peak_table_cols$rt_start) <= ..rt_target.. & !!sym(peak_table_cols$rt_end) >= ..rt_target..),
      ..peak_info.. = ifelse(!is.na(!!sym(pm_cols$compound)), compound, "?")
    ) %>%
    # figure out which peak definitions match multiple peaks
    group_by(!!!map(peak_table_cols$file_id, sym), ..rt_target..) %>%
    mutate(..n_overlapping.. := ifelse(!is.na(!!sym(pm_cols$compound)), sum(..is_target..), 0) %>% as.integer()) %>%
    ungroup() %>%
    # figure out which peaks match multiple definitions (n_matches) --> the -1 is because all will match the NA placeholder compound
    group_by(!!!map(peak_table_cols$file_id, sym), ..peak_id..) %>%
    mutate(
      ..n_matches.. := as.integer(sum(..is_target..) - 1L),
      ..peak_info.. = ifelse(..n_matches.. > 1, paste(na.omit( (!!sym(pm_cols$compound))[..is_target..]), collapse = " or "), ..peak_info..)
    ) %>%
    ungroup() %>%
    # only keep definitions that fit
    filter( ..is_target.., (..n_matches.. >= 1 & ..n_overlapping.. >=1) | (..n_matches.. == 0 & ..n_overlapping.. == 0) ) %>%
    # complete file info
    mutate(
      ..peak_info.. =
        paste0(
          ..peak_info..,
          ifelse(..n_matches.. != 1 | ..n_overlapping.. != 1, "?", ""),
          " (", signif((!!sym(peak_table_cols$rt)), digits = 4), ")"
        )
    )

  # figure out which peaks are missing based on the expected peaks and the anti_join with the found ones
  missing_peaks <-
    unique(peak_table[c(peak_table_cols$file_id, "..map_id..")]) %>%
    left_join(maps, by = "..map_id..") %>%
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    anti_join(found_peaks, by = c(peak_table_cols$file_id, "..map_id..", pm_cols$compound)) %>%
    mutate(
      ..peak_info.. = paste0(compound, " (", signif(..rt_target.., digits = 4), "??)"),
      ..n_matches.. = 0L,
      ..n_overlapping.. = 0L
    )

  # combine found and missing peaks
  all_data <-
    vctrs::vec_rbind(found_peaks, missing_peaks) %>%
    # fill in convenience information columns and unify rt for missing peaks
    mutate(
      !!new_cols$is_identified := !is.na(compound),
      !!new_cols$is_missing := is.na(..peak_id..),
      !!new_cols$is_ambiguous := ..n_overlapping.. > 1 | ..n_matches.. > 1,
      !!peak_table_cols$rt := ifelse(!is.na(!!sym(peak_table_cols$rt)), !!sym(peak_table_cols$rt), ..rt_target..)
    )

  # information
  if (!quiet) {
    simplification <- . %>% select(..peak_id..) %>% unique() %>% nrow()
    n_files <- nrow(unique(all_data[peak_table_cols$file_id]))
    matched_peaks <- all_data %>% filter(!!sym(new_cols$is_identified), !(!!sym(new_cols$is_missing)))
    n_matched_peaks <- matched_peaks %>% simplification
    n_unidentified_peaks <- all_data %>% filter(!(!!sym(new_cols$is_identified))) %>% simplification
    n_missing_matches <- all_data %>% filter(!!sym(new_cols$is_missing)) %>% nrow()

    # ambiguous peaks breakdown
    ambiguous <- matched_peaks %>% filter(!!sym(new_cols$is_ambiguous)) %>%
      group_by(..peak_id..) %>%
      summarize(..n_overlapping.. = max(..n_overlapping..),
                ..n_matches.. = max(..n_matches..)) %>%
      ungroup()
    n_ambiguous_peaks <- ambiguous %>% simplification
    n_multiple_and_overlapping <- ambiguous %>% filter(..n_overlapping.. > 1 & ..n_matches.. > 1) %>% simplification
    n_multiple_matches <- ambiguous %>% filter(..n_overlapping.. <= 1 & ..n_matches.. > 1) %>% simplification
    n_overlapping_matches <- ambiguous %>% filter(..n_overlapping.. > 1 & ..n_matches.. <= 1) %>% simplification

    glue::glue(
      "Info: {n_matched_peaks} of {nrow(peak_table)} peaks in {n_files} files were successfully mapped ",
      if (multiple_maps) "using {length(unique(peak_table$..map_id..))} peak maps ('{paste(unique(peak_table$..map_id..), collapse = \"', '\")}')"
      else "using a single peak map",
      if (n_ambiguous_peaks > 0) str_c(' but ', n_ambiguous_peaks, ' of these peak mappings are ambiguous. ') else '. ',
      if (n_unidentified_peaks > 0) str_c(n_unidentified_peaks, ' peak(s) could not be mapped. ') else '',
      if (n_missing_matches > 0) str_c(n_missing_matches, ' expected peak(s) are missing. ') else '',
      if (n_overwritten > 0) str_c('All previous peak mappings (', n_overwritten, ') were overwritten. ') else '',
      if (n_multiple_matches > 0)
        "\n\t- {n_multiple_matches} were ambiguous because of multiple matching peak assignments" else '',
      if (n_overlapping_matches > 0)
        "\n\t- {n_overlapping_matches} were ambiguous because of overlapping peaks that fit the same assignment" else '',
      if (n_multiple_and_overlapping > 0)
        "\n\t- {n_multiple_and_overlapping} were ambiguous because of both multiple as well as overlapping peak assignments" else ''
    ) %>%
      message()
  }

  # clean up map id
  if (multiple_maps) {
    all_data <- rename(all_data, !!peak_table_cols$map_id := ..map_id..)
  } else {
    all_data <- select(all_data, -..map_id..)
  }

  # return
  all_data %>%
    # clean up unnecessary columns and rename those with specific names
    select(-..is_target.., -..peak_id.., -..rt_target..)%>%
    rename(
      !!new_cols$peak_info := ..peak_info..,
      !!new_cols$n_matches := ..n_matches..,
      !!new_cols$n_overlapping := ..n_overlapping..) %>%
    # put the file id, map id and compound information at the front
    select(peak_table_cols$file_id, pm_cols$compound, new_cols$peak_info, everything()) %>%
    # re-arrange by retention time
    arrange(!!!map(peak_table_cols$file_id, sym), !!sym(peak_table_cols$rt))
}

# problematic peaks mappings =====

#' Renamed to iso_get_problematic_peak_mappings
#' @family peak mapping functions
#' @param ... deprecated
#' @export
iso_get_problematic_peaks <- function(...) {
  warning("this function was renamed for clarification --> calling iso_get_problematic_peak_mappings() instead",
          immediate. = TRUE, call. = FALSE)
  iso_get_problematic_peak_mappings(...)
}

#' Fetch problematic peak mappings
#'
#' Fetch peak mappings that were problematic in any way (from \code{iso_files} or directly from a \code{peak_table}). This function is typically called after \link{iso_map_peaks} to inspect problematic entries. Use the \code{select} parameter to select only the most informative columns (always includes at minimum the \code{peak_info} and \code{problem} columns to identify why the peak is problematic). Note that peaks that are ambiguous because of multiple potential map matches have a data table entry for each potential match.
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @export
iso_get_problematic_peak_mappings <- function(...) {
  UseMethod("iso_get_problematic_peak_mappings")
}

#' @export
iso_get_problematic_peak_mappings.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_get_problematic_peak_mappings.iso_file <- function(iso_files, ...) {
  iso_get_problematic_peak_mappings(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_get_problematic_peak_mappings
#' @param iso_files collection of continuous flow iso_file objects
#' @param select which column to select, by default all
#' @param unidentified whether to include peaks that are problematics because they are unidentified
#' @param missing whether to include peaks that are problematics because they are missing
#' @param ambiguous whether to include peaks that are problematics because they are ambiguously identified
#' @inheritParams iso_get_peak_table
#' @inheritParams iso_show_default_processor_parameters
#' @return data table with rows for problematic peaks and the \code{select}-identified columns
#' @export
iso_get_problematic_peak_mappings.iso_file_list <- function(iso_files, select = everything(), include_file_info = NULL, unidentified = TRUE, missing = TRUE, ambiguous = TRUE, quiet = default(quiet)) {
  iso_files %>%
    iso_get_peak_table(include_file_info = !!enquo(include_file_info), quiet = TRUE) %>%
    iso_get_problematic_peak_mappings(
      select = !!enquo(select),
      unidentified = unidentified,
      missing = missing,
      ambiguous = ambiguous,
      quiet = quiet
    )
}

#' @rdname iso_get_problematic_peak_mappings
#' @param peak_table data frame with mapped peaks. Requires the \code{is_identified}, \code{is_missing} and \code{is_ambiguous} columns to be present.
#' @export
iso_get_problematic_peak_mappings.data.frame <- function(peak_table, select = everything(), unidentified = TRUE, missing = TRUE, ambiguous = TRUE, quiet = default(quiet)) {

  # safety checks
  if (missing(peak_table)) stop("no data table supplied", call. = FALSE)
  peak_table_cols <- get_column_names(!!enquo(peak_table), select = enquo(select),
                              is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous),
                              n_reqs = list(select = "+"))

  # filtering
  peak_table_out <- peak_table %>%
    filter( (unidentified & !(!!sym(peak_table_cols$is_identified))) |
              (missing & !!sym(peak_table_cols$is_missing)) |
              (ambiguous & !!sym(peak_table_cols$is_ambiguous))) %>%
    # add tpye information
    mutate(
      problem = case_when(
        !!sym(peak_table_cols$is_missing) ~ "missing",
        !(!!sym(peak_table_cols$is_identified)) ~ "unidentified",
        !!sym(peak_table_cols$is_ambiguous) ~ "ambiguous",
        TRUE ~ NA_character_
      )
    ) %>%
    # include type problem column
    dplyr::select(!!!c(peak_table_cols$select, "peak_info", "problem"))


  # info
  if(!quiet) {
    types <- c()
    if (unidentified) types <- c(types, "unidentified")
    if (missing) types <- c(types, "missing")
    if (ambiguous) types <- c(types, "ambiguous")
    glue::glue("Info: fetching {nrow(peak_table_out)} of {nrow(peak_table)} data table entries with problematic peak identifications ",
         "({collapse(types, sep = ', ', last = ' or ')})") %>%
      message()
  }

  return(peak_table_out)
}

# summarize peak mappings ======

#' Summarize peaks
#'
#' Summarize peaks after peak mapping. This function is called after \link{iso_map_peaks} and can be used in combination with \link{iso_get_problematic_peaks} to inspect problematic peaks in particular. For the \code{file_id} parameter, make sure to use the same set or a subset of the columns used to identify individual files in the \link{iso_map_peaks} call before.
#' @return summary data table with one row for each unique combination of the \code{file_id} parameter
#' \itemize{
#' \item{\code{mapped}: }{number of peaks that were identified during the mapping process (out of the total number of peaks in each sample)}
#' \item{\code{ambiguous}: }{number of mapped peaks that were ambiguous (out of all the mapped peaks) because they either have multiple matches or because they overlap with other mapped peaks}
#' \item{\code{missing}: }{number of peaks that were listed in the peak map but appear to be missing in the sample (out of the total number of peaks listed in the peak map)}
#' \item{\code{peak_info}: }{concatenated text with all the peaks and their retention times (with \code{'?'} for unknown peak names, retention times or other ambiguities}
#' }
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @family peak mapping functions
#' @export
iso_summarize_peak_mappings <- function(...) {
  UseMethod("iso_summarize_peak_mappings")
}

#' @export
iso_summarize_peak_mappings.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_summarize_peak_mappings.iso_file <- function(iso_files, ...) {
  iso_summarize_peak_mappings(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_summarize_peak_mappings
#' @inheritParams iso_map_peaks.iso_file_list
#' @inheritParams iso_get_peak_table
#' @export
iso_summarize_peak_mappings.iso_file_list <- function(iso_files, include_file_info = NULL, compound = default(compound), rt = default(rt)) {
  peak_table <- iso_get_peak_table(iso_files, quiet = TRUE)
  file_info <- iso_get_file_info(iso_files, select = !!enquo(include_file_info), quiet = TRUE)
  if (ncol(file_info) > 1) {
    peak_table <- dplyr::left_join(file_info, peak_table, by = "file_id")
  }
  iso_summarize_peak_mappings(
    peak_table,
    file_id = !!names(file_info),
    compound = !!enquo(compound),
    rt = !!enquo(rt)
  )
}

#' @rdname iso_summarize_peak_mappings
#' @inheritParams iso_map_peaks.data.frame
#' @export
iso_summarize_peak_mappings.data.frame <- function(peak_table, file_id = default(file_id), compound = default(compound), rt = default(rt)) {

  # safety checks
  peak_table_cols <-
    isoreader:::get_column_names(
      peak_table, file_id = enquo(file_id), peak_info = quo(peak_info),
      compound = enquo(compound), rt = enquo(rt),
      is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous),
      n_overlapping = quo(n_overlapping),
      n_reqs = list(file_id = "+"))

  # renames
  col_renames <- peak_table_cols[names(peak_table_cols) != "file_id"] %>% unname() %>% unlist()

  # calculate summary stats
  peak_table %>%
    rename(!!!col_renames) %>%
    group_by(!!!map(peak_table_cols$file_id, sym)) %>%
    # generate peak id field for
    summarize(
      mapped = sprintf(
        "%d/%d",
        length(unique(rt[is_identified & !is_missing])),
        length(unique(rt[!is_missing]))
        ),
      ambiguous = sprintf(
        "%d/%d",
        length(unique(rt[is_identified & !is_missing & is_ambiguous])),
        length(unique(rt[is_identified & !is_missing]))
      ),
      missing =
        sprintf(
          "%d/%d",
          length(unique(rt[is_identified & is_missing])),
          # this calculation is a bit complicated because of potentially
          # identically named compounds and multiple entries for overlapping ones
          length(unique(paste(compound, rt)[is_identified & n_overlapping <= 1])) +
            sum(!duplicated(compound[is_identified & n_overlapping > 1]))
        ),
      peak_info = paste(unique(peak_info), collapse = ", ")
    )

}

# remove problematic peak mappings ======

#' Renamed to iso_remove_problematic_peak_mappings
#' @param ... deprecated
#' @export
iso_remove_problematic_peaks <- function(...) {
  warning("this function was renamed for clarification --> calling iso_remove_problematic_peak_mappings() instead",
          immediate. = TRUE, call. = FALSE)
  iso_remove_problematic_peak_mappings(...)
}

#' Remove problematic peak mappings
#'
#' Remove peak mappings that were problematic (unidentified, missing, or ambiguous) during peak mapping.
#' @return the data with the problematic peak mappings remove
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @family peak mapping functions
#' @export
iso_remove_problematic_peak_mappings <- function(...) {
  UseMethod("iso_remove_problematic_peak_mappings")
}

#' @export
iso_remove_problematic_peak_mappings.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_remove_problematic_peak_mappings.iso_file <- function(iso_files, ...) {
  iso_remove_problematic_peak_mappings(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_remove_problematic_peak_mappings
#' @inheritParams iso_get_problematic_peak_mappings.iso_file_list
#' @param remove_mapping_info_column whether to automatically remove mapping info columns. If true and:
#' \itemize{
#'  \item{\code{remove_unidentified = TRUE} - }{\code{is_identified} column automatically removed}
#'  \item{\code{remove_missing = TRUE} - }{\code{is_missing} column automatically removed}
#'  \item{\code{remove_ambiguous = TRUE} - }{\code{is_ambiguous}, \code{n_matches}, and \code{n_overlapping} columns automatically removed}
#' }
#' @export
iso_remove_problematic_peak_mappings.iso_file_list <- function(
  iso_files, remove_unidentified = TRUE, remove_missing = TRUE, remove_ambiguous = TRUE,
  remove_mapping_info_columns = TRUE, quiet = default(quiet)) {

  # peak_table
  peak_table <- iso_get_peak_table(iso_files, quiet = TRUE)
  if (nrow(peak_table) == 0) return(iso_files)

  # remove prob peaks
  peak_table <- iso_remove_problematic_peak_mappings(
    peak_table,
    remove_unidentified = remove_unidentified,
    remove_missing = remove_missing,
    remove_ambiguous = remove_ambiguous,
    remove_mapping_info_columns = remove_mapping_info_columns,
    quiet = quiet
  )

  return(iso_set_peak_table(iso_files, peak_table, quiet = TRUE))
}

#' @rdname iso_remove_problematic_peak_mappings
#' @inheritParams iso_get_problematic_peak_mappings.data.frame
#' @export
iso_remove_problematic_peak_mappings.data.frame <- function(
  peak_table, remove_unidentified = TRUE, remove_missing = TRUE, remove_ambiguous = TRUE,
  remove_mapping_info_columns = TRUE, quiet = default(quiet)) {

  # safety checks
  if (missing(peak_table)) stop("no data table supplied", call. = FALSE)
  peak_table_cols <- get_column_names(!!enquo(peak_table), is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous))

  # filtering
  peak_table_out <- peak_table %>%
    filter( !remove_unidentified | !!sym(peak_table_cols$is_identified),
            !remove_missing | !(!!sym(peak_table_cols$is_missing)),
            !remove_ambiguous | !(!!sym(peak_table_cols$is_ambiguous)))

  # info
  if(!quiet) {
    types <- c()
    if (remove_unidentified) types <- c(types, "unidentified")
    if (remove_missing) types <- c(types, "missing")
    if (remove_ambiguous) types <- c(types, "ambiguous")
    glue::glue("Info: removing {nrow(peak_table) - nrow(peak_table_out)} of {nrow(peak_table)} peak table entries because of problematic peak identifications ",
         "({collapse(types, sep = ', ', last = ' or ')})") %>%
      message()
  }

  # remove columns
  if (remove_mapping_info_columns && remove_unidentified)
    peak_table_out <- peak_table_out %>% select(-!!sym(peak_table_cols$is_identified))
  if (remove_mapping_info_columns && remove_missing)
    peak_table_out <- peak_table_out %>% select(-!!sym(peak_table_cols$is_missing))
  if (remove_mapping_info_columns && remove_ambiguous) {
    peak_table_out <- peak_table_out %>% select(-!!sym(peak_table_cols$is_ambiguous))
    if ("n_matches" %in% names(peak_table_out))
      peak_table_out <- peak_table_out %>% select(-n_matches)
    if ("n_overlapping" %in% names(peak_table_out))
      peak_table_out <- peak_table_out %>% select(-n_overlapping)
  }

  return(peak_table_out)

}
