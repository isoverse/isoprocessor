# peak mapping ======

#' Map peaks based on retention time
#'
#' This function makes it easy to map peaks based on peak maps. It reports all peaks including missing peaks and ambiguous peaks by adding a set of information columns for each entry (\code{is_identified}, \code{is_missing}, \code{is_ambiguous}, \code{n_matches}, \code{n_overlapping}).
#' For routine downstream data processing, this function is usually followed by \code{\link{iso_get_problematic_peaks}} to inspect the problematic peaks and \code{\link{iso_remove_problematic_peaks}} to proceed only with peaks that do not have any problems. Note that without this filter, one must proceed with great caution interpreting the ambiguous peaks.
#'
#' @param dt data frame with peak data
#' @param peak_maps data frame with the peak map
#' @param file_id the column(s) in dt that uniquely identify a file/set of peaks that belong together
#' @param map_id the column in dt that indicates which map to use for which file
#' @param compound the column in peak_maps that holds compound information
#' @param rt the column in dt and colum prefix in peak_maps ("rt:...") that holds retention time information
#' @param rt_start the column in dt that holds start of peak retention times
#' @param rt_end the column in dt that holds end of peak retention times
#' @param rt_prefix_divider the divider after the retention time column prefix in peak_maps to identify the map id values (e.g. "rt:map_id_value")
#' @return data frame with mapped peaks and the following information columns:
#' \itemize{
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
  dt_cols <- get_column_names(!!enquo(dt), file_id = enquo(file_id), map_id = enquo(map_id), rt = enquo(rt), rt_start = enquo(rt_start), rt_end = enquo(rt_end),
                              n_reqs = list(file_id = "+"))
  pm_cols <- get_column_names(!!enquo(peak_maps), compound = enquo(compound))
  new_cols <- get_new_column_names(is_identified = quo(is_identified), is_missing = quo(is_missing), is_ambiguous = quo(is_ambiguous), n_matches = quo(n_matches), n_overlapping = quo(n_overlapping))

  # read peak maps
  maps <- peak_maps %>%
    # ignore empty rows (safety precaution)
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    # add NA compound to map undefined peaks
    bind_rows(data_frame(compound = NA)) %>% unique() %>%
    # gather map retention times
    gather(..map_id.., ..rt_target.., starts_with(dt_cols$rt)) %>%
    # replace the rt prefix to get to the actual map name
    mutate(!!sym(dt_cols$map_id) := str_replace(..map_id.., fixed(str_c(dt_cols$rt, rt_prefix_divider)), "")) %>%
    select(-..map_id..) %>%
    # only keep the NA compound and everything that has a ..rt_target..
    filter(is.na(!!sym(pm_cols$compound)) | !is.na(..rt_target..))

  # safety check --> look for data entries without peak maps
  if ( nrow(missing <- filter(dt, is.na(!!sym(dt_cols$map_id)))) > 0 ) {
    glue("cannot proceed - {nrow(missing)} data table entries do not have a map defined in the '{dt_cols$map_id}' column. Make sure to remove entries with missing map metadata first.") %>%
      stop(call. = FALSE)
  }

  # safety check --> look for missing maps
  if (length(missing <- setdiff(unique(dt[[dt_cols$map_id]]), maps[[dt_cols$map_id]])) > 0) {
    glue("the following maps are referenced in the data table but do not exist in the peak maps: '{collapse(missing, \"', '\")}'. Available peak maps: '{collapse(unique(maps[[dt_cols$map_id]]), \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combine peak map data and file data
  found_peaks <-
    maps %>%
    # right join to keep map columns first
    right_join(
      # add unique id per peak for identification simplicity
      dt %>% mutate(..peak_id.. = 1:n()),
      by = dt_cols$map_id
    ) %>%
    # find the peak that the retention time window matches
    mutate(..is_target.. = is.na(..rt_target..) | (!!sym(dt_cols$rt_start) <= ..rt_target.. & !!sym(dt_cols$rt_end) >= ..rt_target..)) %>%
    # figure out which peaks match multiple definitions (n_matches) --> the -1 is because all will match the NA placeholder compound
    group_by(!!!map(dt_cols$file_id, sym), ..peak_id..) %>%
    mutate(..n_matches.. := as.integer(sum(..is_target..) - 1L)) %>%
    ungroup() %>%
    # figure out which peak definitions match multiple peaks
    group_by(!!!map(dt_cols$file_id, sym), ..rt_target..) %>%
    mutate(..n_overlapping.. := ifelse(!is.na(!!sym(pm_cols$compound)), sum(..is_target..), 0) %>% as.integer()) %>%
    ungroup() %>%
    # only keep definitions that fit
    filter( ..is_target.., (..n_matches.. >= 1 & ..n_overlapping.. >=1) | (..n_matches.. == 0 & ..n_overlapping.. == 0) )

  # figure out which peaks are missing based on the expected peaks and the anti_join with the found ones
  missing_peaks <-
    unique(dt[c(dt_cols$file_id, dt_cols$map_id)]) %>%
    left_join(maps, by = dt_cols$map_id) %>%
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    anti_join(found_peaks, by = c(dt_cols$file_id, dt_cols$map_id, pm_cols$compound)) %>%
    mutate(..n_matches.. = 0L, ..n_overlapping.. = 0L)

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
    glue("Info: {n_matched_peaks} of {nrow(dt)} peaks in {n_files} files were successfully mapped",
         "{if (n_ambiguous_peaks > 0) str_c(' but ', n_ambiguous_peaks, ' of these are ambiguous') }. ",
         "{if (n_unidentified_peaks > 0) str_c(n_unidentified_peaks, ' peak(s) could not be mapped. ')}",
         "{if (n_missing_matches > 0) str_c(n_missing_matches, ' expected peak(s) are missing.')}") %>%
      message()

    if (n_multiple_matches > 0)
      glue("      - {n_multiple_matches} were ambiguous because of multiple matching peak assignments") %>% message()
    if (n_overlapping_matches > 0)
      glue("      - {n_overlapping_matches} were ambiguous because of overlapping peaks that fit the same assignment") %>% message()
    if (n_multiple_and_overlapping > 0)
      glue("      - {n_multiple_and_overlapping} were ambiguous because of both multiple as well as overlapping peak assignments") %>% message()
  }

  all_data %>%
    # clean up unnecessary columns and rename those with specific names
    select(-..is_target.., -..peak_id.., -..rt_target..)%>%
    rename(!!new_cols$n_matches := ..n_matches..,
           !!new_cols$n_overlapping := ..n_overlapping..) %>%
    # put the file id, map id and compound information at the front
    select(dt_cols$file_id, dt_cols$map_id, pm_cols$compound, everything()) %>%
    # re-arrange by retention time
    arrange(!!!map(dt_cols$file_id, sym), !!sym(dt_cols$rt))
}


#' Fetch problematic peaks
#'
#' Fetch peaks that were problematic during peak mapping. This function is typically called after \link{iso_add_metadata} to inspect problematic entries. Use the \code{select} parameter to select only the most informative columns (always includes a \code{problem} column that identifies why the peak is problematic). Note that peaks that are ambiguous because of multiple potential map matches have a data table entry for each potential match.
#'
#' @inheritParams iso_get_missing_metadata
#' @param dt data table with mapped peaks. Requires the \code{is_identified}, \code{is_missing} and \code{is_ambiguous} columns to be present.
#' @param unidentified whether to include peaks that are problematics because they are unidentified
#' @param missing whether to include peaks that are problematics because they are missing
#' @param ambiguous whether to include peaks that are problematics because they are ambiguously identified
#' @return data table with rows for problematic peaks and the \code{select}-identified columns
#' @family peak mapping functions
#' @export
iso_get_problematic_peaks <- function(dt, select = everything(), unidentified = TRUE, missing = TRUE, ambiguous = TRUE, quiet = default(quiet)) {

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
    dplyr::select(!!!c(dt_cols$select, "problem"))


  # info
  if(!quiet) {
    types <- c()
    if (unidentified) types <- c(types, "unidentified")
    if (missing) types <- c(types, "missing")
    if (ambiguous) types <- c(types, "ambiguous")
    glue("Info: fetching {nrow(dt_out)} of {nrow(dt)} data table entries with problematic peak identifications ",
         "({collapse(types, sep = ', ', last = ' or ')})") %>%
      message()
  }

  return(dt_out)
}

#' Remove problematic peaks
#'
#' Remove peaks that were problematic during peak mapping.
#'
#' @inheritParams iso_get_problematic_peaks
#' @param remove_mapping_info_column whether to automatically remove mapping info columns. If true and:
#' \itemize{
#'  \item{\code{remove_unidentified = TRUE} - }{\code{is_identified} column automatically removed}
#'  \item{\code{remove_missing = TRUE} - }{\code{is_missing} column automatically removed}
#'  \item{\code{remove_ambiguous = TRUE} - }{\code{is_ambiguous}, \code{n_matches}, and \code{n_overlapping} columns automatically removed}
#' }
#' @family peak mapping functions
#' @export
iso_remove_problematic_peaks <- function(dt, remove_unidentified = TRUE, remove_missing = TRUE, remove_ambiguous = TRUE,
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
    glue("Info: removing {nrow(dt) - nrow(dt_out)} of {nrow(dt)} data table entries because of problematic peak identifications ",
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
