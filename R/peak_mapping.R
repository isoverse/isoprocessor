# peak mapping ======

#' Map peaks based on retention time
#'
#' This function makes it easy to map peaks based on peak maps. It reports all peaks including missing peaks and ambiguous peaks by adding a set of information columns for each entry (\code{is_identified}, \code{is_missing}, \code{is_ambiguous}, \code{n_matches}, \code{n_overlapping}). For routine downstream data processing, this function is usually followed by \code{\link{iso_summarize_peak_mappings}} and \code{\link{iso_get_problematic_peak_mappings}} to inspect the problematic peaks and \code{\link{iso_remove_problematic_peak_mappings}} to proceed only with mapped peaks that are clearly identified. Note that without this filter, one must proceed with great caution interpreting the ambiguous peaks. Also note that if the \code{compound} column alreadty exists in \code{dt}, it will be overwritten with the new mappings from the peak maps but will issue a warning that this is happening.
#'
#' @param dt data frame with peak data
#' @param peak_maps data frame with the peak map(s). At minimum, this data frame must have a \code{compound} and \code{rt} column but may have additional information columns. If multiple peak maps are provided, the \code{dt} data frame requires a \code{map_id} column to identify which peak map should be used and the peak maps data frame must have a \code{rt:<map_id>} column for each used value of \code{map_id}. The names of all these columns can be changed if necessary using the \code{compound}, code{rt} and \code{map_id} parameters.
#' @param file_id the column(s) in dt that uniquely identify a file/set of peaks that belong together
#' @param map_id the column in dt that indicates which map to use for which file (only necessary if multiple peak maps are used)
#' @param compound the column in peak_maps that holds compound information
#' @param rt the column in dt and colum prefix in peak_maps ("rt:...") that holds retention time information
#' @param rt_start the column in dt that holds start of peak retention times
#' @param rt_end the column in dt that holds end of peak retention times
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
#' @family peak mapping functions
#' @export
iso_map_peaks <- function(
  dt, peak_maps, file_id = default(file_id), map_id = default(map_id), compound = default(compound),
  rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end), rt_prefix_divider = ":",
  quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(peak_maps)) stop("no peak map(s) supplied", call. = FALSE)
  dt_quo <- enquo(dt)
  if (nrow(dt) == 0) stop("no data in the data table", call. = FALSE)

  # find regular dt columns
  dt_cols <- isoreader:::get_column_names(
    !!dt_quo, file_id = enquo(file_id), rt = enquo(rt), rt_start = enquo(rt_start), rt_end = enquo(rt_end),
    n_reqs = list(file_id = "+"))

  # find dt map_id column
  map_quo <- resolve_defaults(enquo(map_id))
  dt_cols$map_id <- tidyselect::vars_select(names(dt), map_id = !!map_quo, .strict = FALSE)
  if (length(dt_cols$map_id) > 1)
    glue::glue("map id must be stored in a single column, not: '{paste(dt_cols$map_id, collapse = \"', '\")}'") %>%
    stop(call. = FALSE)

  # find dt compound column
  compound_quo <- resolve_defaults(enquo(compound))
  dt_cols$compound <- tidyselect::vars_select(names(dt), compound = !!compound_quo, .strict = FALSE)

  # find peak map columns
  pm_cols <- isoreader:::get_column_names(!!enquo(peak_maps), compound = compound_quo)

  # find new columns
  new_cols <- isoprocessor:::get_new_column_names(peak_info = quo(peak_info), is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous), n_matches = quo(n_matches), n_overlapping = quo(n_overlapping))

  # deal with pre-existing compound column
  n_overwritten <- 0L
  if (length(dt_cols$compound) > 0) {
    n_overwritten <- sum(!is.na(dt[[dt_cols$compound]]))
    dt <- select(dt, -!!sym(dt_cols$compound))
  }

  # SK note: allowing pre-existing assignments to remain (via overwrite param) seems more
  # hassle than it's worth, too many ambiguous columns that are tricky to fill (n_overlapping, etc.)
  # @param overwrite whether to overwrite existing peak mappings. If set to \code{FALSE}, will ignore all peaks that already have values in the \code{compound} column.
  # pre_dt <- data_frame()
  # if (length(dt_cols$compound) > 0) {
  #   # got a pre-existing compound column
  #   pre_dt <- filter(dt, !is.na(!!sym(dt_cols$compound))) %>%
  #     mutate(..peak_info.. = paste0(!!sym(dt_cols$compound), " (", signif((!!sym(dt_cols$rt)), digits = 4), ")"))
  #   if (!overwrite) {
  #     # map the ones not overwriting
  #     dt <- filter(dt, is.na(!!sym(dt_cols$compound)))
  #     if (nrow(dt) == 0) return(pre_dt)
  #   }
  #   dt <- select(dt, -!!sym(dt_cols$compound))
  # }

  # read peak maps
  maps <- peak_maps %>%
    # ignore empty rows (safety precaution)
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    # add NA compound to map undefined peaks
    bind_rows(tibble(compound = NA)) %>% unique() %>%
    # gather map retention times
    gather(..map_id.., ..rt_target.., starts_with(dt_cols$rt)) %>%
    # replace the rt prefix to get to the actual map name
    mutate(..map_id.. := str_replace(..map_id.., fixed(str_c(dt_cols$rt, rt_prefix_divider)), "")) %>%
    # only keep the NA compound and everything that has a ..rt_target..
    filter(is.na(!!sym(pm_cols$compound)) | !is.na(..rt_target..))

  # check map id type
  map_ids <- unique(maps$..map_id..)
  if (length(map_ids) == 1 && str_detect(map_ids, str_c("^", dt_cols$rt, "$"))) {
    # single map
    dt <- dt %>% mutate(..map_id.. = !!dt_cols$rt)
    multiple_maps <- FALSE
  } else if (length(map_ids) == 1 && length(dt_cols$map_id) == 0) {
    glue::glue(
      "map id defined ('{map_ids}') ",
      "but the '{quo_text(map_quo)}' column does not exist in the data frame.") %>%
      stop(call. = FALSE)
  } else if (length(map_ids) >= 1 && length(dt_cols$map_id) == 0) {
    # multiple maps but no map id defined
    glue::glue(
      "more than one map defined ({paste(map_ids, collapse = ', ')}) ",
      "but the '{quo_text(map_quo)}' column does not exist in the data frame.") %>%
      stop(call. = FALSE)
  } else {
    # all in order
    dt <- dt %>% rename(..map_id.. = !!sym(dt_cols$map_id))
    multiple_maps <- TRUE
  }

  # safety check --> look for data entries without peak maps
  if ( nrow(missing <- filter(dt, is.na(..map_id..))) > 0 ) {
    glue::glue(
      "cannot proceed - {nrow(missing)} data table entries do not have a ",
      "map defined in the '{dt_cols$map_id}' column. Make sure to remove ",
      "entries with missing map metadata first.") %>%
      stop(call. = FALSE)
  }

  # safety check --> look for missing maps
  if (length(missing <- setdiff(unique(dt$..map_id..), maps$..map_id..)) > 0) {
    glue::glue(
      "the following maps are referenced in the data table but do not exist in ",
      "the peak maps: '{collapse(missing, \"', '\")}'. Available peak maps: ",
      "'{collapse(unique(maps$..map_id..), \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combine peak map data and file data
  found_peaks <-
    maps %>%
    # right join to keep map columns first
    right_join(
      # add unique id per peak for identification simplicity
      dt %>% mutate(..peak_id.. = 1:dplyr::n()),
      by = "..map_id.."
    ) %>%
    # find the peak that the retention time window matches
    mutate(
      ..is_target.. = is.na(..rt_target..) | (!!sym(dt_cols$rt_start) <= ..rt_target.. & !!sym(dt_cols$rt_end) >= ..rt_target..),
      ..peak_info.. = ifelse(!is.na(!!sym(pm_cols$compound)), compound, "?")
    ) %>%
    # figure out which peak definitions match multiple peaks
    group_by(!!!map(dt_cols$file_id, sym), ..rt_target..) %>%
    mutate(..n_overlapping.. := ifelse(!is.na(!!sym(pm_cols$compound)), sum(..is_target..), 0) %>% as.integer()) %>%
    ungroup() %>%
    # figure out which peaks match multiple definitions (n_matches) --> the -1 is because all will match the NA placeholder compound
    group_by(!!!map(dt_cols$file_id, sym), ..peak_id..) %>%
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
          " (", signif((!!sym(dt_cols$rt)), digits = 4), ")"
        )
    )

  # figure out which peaks are missing based on the expected peaks and the anti_join with the found ones
  missing_peaks <-
    unique(dt[c(dt_cols$file_id, "..map_id..")]) %>%
    left_join(maps, by = "..map_id..") %>%
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    anti_join(found_peaks, by = c(dt_cols$file_id, "..map_id..", pm_cols$compound)) %>%
    mutate(
      ..peak_info.. = paste0(compound, " (", signif(..rt_target.., digits = 4), "??)"),
      ..n_matches.. = 0L,
      ..n_overlapping.. = 0L
    )

  # combine found and missing peaks
  all_data <-
    bind_rows(found_peaks, missing_peaks) %>%
    # fill in convenience information columns and unify rt for missing peaks
    mutate(
      !!new_cols$is_identified := !is.na(compound),
      !!new_cols$is_missing := is.na(..peak_id..),
      !!new_cols$is_ambiguous := ..n_overlapping.. > 1 | ..n_matches.. > 1,
      !!dt_cols$rt := ifelse(!is.na(!!sym(dt_cols$rt)), !!sym(dt_cols$rt), ..rt_target..)
    )

  # information
  if (!quiet) {
    simplification <- . %>% select(..peak_id..) %>% unique() %>% nrow()
    n_files <- nrow(unique(all_data[dt_cols$file_id]))
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
      "Info: {n_matched_peaks} of {nrow(dt)} peaks in {n_files} files were successfully mapped ",
      if (multiple_maps) "using {length(unique(dt$..map_id..))} peak maps ('{paste(unique(dt$..map_id..), collapse = \"', '\")}')"
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
    all_data <- rename(all_data, !!dt_cols$map_id := ..map_id..)
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
    select(dt_cols$file_id, pm_cols$compound, new_cols$peak_info, everything()) %>%
    # re-arrange by retention time
    arrange(!!!map(dt_cols$file_id, sym), !!sym(dt_cols$rt))
}


#' Renamed to iso_get_problematic_peak_mappings
#' @param ... deprecated
#' @export
iso_get_problematic_peaks <- function(...) {
  warning("this function was renamed for clarification --> calling iso_get_problematic_peak_mappings() instead",
          immediate. = TRUE, call. = FALSE)
  iso_get_problematic_peak_mappings(...)
}

#' Fetch problematic peak mappings
#'
#' Fetch peak mappings that were problematic in any way. This function is typically called after \link{iso_map_peaks} to inspect problematic entries. Use the \code{select} parameter to select only the most informative columns (always includes at minimum the \code{peak_info} and \code{problem} columns to identify why the peak is problematic). Note that peaks that are ambiguous because of multiple potential map matches have a data table entry for each potential match.
#'
#' @inheritParams iso_get_missing_metadata
#' @param dt data table with mapped peaks. Requires the \code{is_identified}, \code{is_missing} and \code{is_ambiguous} columns to be present.
#' @param unidentified whether to include peaks that are problematics because they are unidentified
#' @param missing whether to include peaks that are problematics because they are missing
#' @param ambiguous whether to include peaks that are problematics because they are ambiguously identified
#' @inheritParams iso_show_default_processor_parameters
#' @return data table with rows for problematic peaks and the \code{select}-identified columns
#' @family peak mapping functions
#' @export
iso_get_problematic_peak_mappings <- function(dt, select = everything(), unidentified = TRUE, missing = TRUE, ambiguous = TRUE, quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), select = enquo(select),
                              is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous),
                              n_reqs = list(select = "+"))

  # filtering
  dt_out <- dt %>%
    filter( (unidentified & !(!!sym(dt_cols$is_identified))) |
              (missing & !!sym(dt_cols$is_missing)) |
              (ambiguous & !!sym(dt_cols$is_ambiguous))) %>%
    # add tpye information
    mutate(
      problem = case_when(
        !!sym(dt_cols$is_missing) ~ "missing",
        !(!!sym(dt_cols$is_identified)) ~ "unidentified",
        !!sym(dt_cols$is_ambiguous) ~ "ambiguous",
        TRUE ~ NA_character_
      )
    ) %>%
    # include type problem column
    dplyr::select(!!!c(dt_cols$select, "peak_info", "problem"))


  # info
  if(!quiet) {
    types <- c()
    if (unidentified) types <- c(types, "unidentified")
    if (missing) types <- c(types, "missing")
    if (ambiguous) types <- c(types, "ambiguous")
    glue::glue("Info: fetching {nrow(dt_out)} of {nrow(dt)} data table entries with problematic peak identifications ",
         "({collapse(types, sep = ', ', last = ' or ')})") %>%
      message()
  }

  return(dt_out)
}


#' Summarize peaks
#'
#' Summarize peaks after peak mapping. This function is called after \link{iso_map_peaks} and can be used in combination with \link{iso_get_problematic_peaks} to inspect problematic peaks in particular. For the \code{file_id} parameter, make sure to use the same set or a subset of the columns used to identify individual files in the \link{iso_map_peaks} call before.
#'
#' @inheritParams iso_map_peaks
#' @return summary data table with one row for each unique combination of the \code{file_id} parameter
#' \itemize{
#' \item{\code{mapped}: }{number of peaks that were identified during the mapping process (out of the total number of peaks in each sample)}
#' \item{\code{ambiguous}: }{number of mapped peaks that were ambiguous (out of all the mapped peaks) because they either have multiple matches or because they overlap with other mapped peaks}
#' \item{\code{missing}: }{number of peaks that were listed in the peak map but appear to be missing in the sample (out of the total number of peaks listed in the peak map)}
#' \item{\code{peak_info}: }{concatenated text with all the peaks and their retention times (with \code{'?'} for unknown peak names, retention times or other ambiguities}
#' }
#' @family peak mapping functions
#' @export
iso_summarize_peak_mappings <- function(dt, file_id = default(file_id), compound = default(compound), rt = default(rt)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <-
    isoreader:::get_column_names(
      dt, file_id = enquo(file_id), peak_info = quo(peak_info),
      compound = enquo(compound), rt = enquo(rt),
      is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous),
      n_overlapping = quo(n_overlapping),
      n_reqs = list(file_id = "+"))

  # renames
  col_renames <- dt_cols[names(dt_cols) != "file_id"] %>% unname() %>% unlist()

  # calculate summary stats
  dt %>%
    rename(!!!col_renames) %>%
    group_by(!!!map(dt_cols$file_id, sym)) %>%
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
#'
#' @inheritParams iso_get_problematic_peak_mappings
#' @param remove_mapping_info_column whether to automatically remove mapping info columns. If true and:
#' \itemize{
#'  \item{\code{remove_unidentified = TRUE} - }{\code{is_identified} column automatically removed}
#'  \item{\code{remove_missing = TRUE} - }{\code{is_missing} column automatically removed}
#'  \item{\code{remove_ambiguous = TRUE} - }{\code{is_ambiguous}, \code{n_matches}, and \code{n_overlapping} columns automatically removed}
#' }
#' @family peak mapping functions
#' @export
iso_remove_problematic_peak_mappings <- function(dt, remove_unidentified = TRUE, remove_missing = TRUE, remove_ambiguous = TRUE,
                                         remove_mapping_info_columns = TRUE, quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous))

  # filtering
  dt_out <- dt %>%
    filter( !remove_unidentified | !!sym(dt_cols$is_identified),
            !remove_missing | !(!!sym(dt_cols$is_missing)),
            !remove_ambiguous | !(!!sym(dt_cols$is_ambiguous)))

  # info
  if(!quiet) {
    types <- c()
    if (remove_unidentified) types <- c(types, "unidentified")
    if (remove_missing) types <- c(types, "missing")
    if (remove_ambiguous) types <- c(types, "ambiguous")
    glue::glue("Info: removing {nrow(dt) - nrow(dt_out)} of {nrow(dt)} data table entries because of problematic peak identifications ",
         "({collapse(types, sep = ', ', last = ' or ')})") %>%
      message()
  }

  # remove columns
  if (remove_mapping_info_columns && remove_unidentified)
    dt_out <- dt_out %>% select(-!!sym(dt_cols$is_identified))
  if (remove_mapping_info_columns && remove_missing)
    dt_out <- dt_out %>% select(-!!sym(dt_cols$is_missing))
  if (remove_mapping_info_columns && remove_ambiguous) {
    dt_out <- dt_out %>% select(-!!sym(dt_cols$is_ambiguous))
    if ("n_matches" %in% names(dt_out))
      dt_out <- dt_out %>% select(-n_matches)
    if ("n_overlapping" %in% names(dt_out))
      dt_out <- dt_out %>% select(-n_overlapping)
  }

  return(dt_out)

}
