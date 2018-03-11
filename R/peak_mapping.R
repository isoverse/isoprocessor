# metadata assignment =====

#' Add metadata to data table
#'
#' This function adds metadata flexibly by providing the possibility to match_by multiple different columns (paramter \code{match_by}) in sequence.
#' This is equivalent to applying a set of increasingly more specific matching rules. For each metadata column, only the rows that have a defined non empty ("") value
#' for that column will be mapped to the data. Each set of metadata can overwrite the previous matches such that the last metadata column defined by \code{match_by}
#' will overwrite all previous matches for which it applies, even if they have already been a match for a previous column.
#'
#' This function also introduces a \code{has_metadata} column that would be typically used afterwards to inspect and/or filter data that has/doesn't have metadata.
#'
#' Note that this is a convenience function for easily adding metadata in a rule based way. If a direct \code{\link[dplyr]{join}} is suitable (i.e. if there is direct
#' 1-to-1 or 1-to-many mapping of a single ID column or several id columns in combination), it will be a lot faster to use the \code{\link[dplyr]{join}}.
#'
#' @param dt data frame with the data
#' @param metadata data frame with the metadata
#' @param match_by the column (or columns) to match the metadata by. Used sequently, i.e. if the first column is defined in the metadata, it will be used first before mapping the remainder of the metadata with the second column, etc. All columns must exist in both the \code{dt} and \code{metadata} data frames.
#' @param has_metadata the name for the new column which stores a logical TRUE/FALSE indicating whether the entry in \code{dt} has added metadata
#' @return merged data frame with data and metadata and new column defined by parameter \code{has_metadata} that holds information about which rows had metadata matches
#' @export
iso_add_metadata <- function(dt, metadata, match_by = default(match_by), has_metadata = default(has_metadata), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(metadata)) stop("no metadata supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), match_by = enquo(match_by), n_reqs = list(match_by = "+"))
  md_cols <- get_column_names(!!enquo(metadata), match_by = enquo(match_by), n_reqs = list(match_by = "+"))
  new_cols <- get_new_column_names(has_metadata = enquo(has_metadata))

  # figure out how metadata best maps to the data based on the match_by priority
  matching_idx <-
    data_frame(column = md_cols$match_by) %>%
    # find all filled metadata column indices
    mutate(
      priority = 1:n(),
      # all available metadata for this category
      mdata_idx = map(column, ~which(!is.na(metadata[[.x]]) & nchar(as.character(metadata[[.x]])) > 0))
    ) %>%
    unnest(mdata_idx) %>%
    mutate(
      # the data indices that fit the metadata mapping value
      data_idx = map2(column, mdata_idx, ~which(dt[[.x]] == metadata[[.x]][.y]))
    ) %>%
    unnest(data_idx) %>%
    # figure out which metadata set to use for which data index (based on priority)
    group_by(data_idx) %>%
    # the highest priority (last column in match_by) takes precedence
    filter(priority == max(priority)) %>%
    # if multiple metadata rows map to the same data_idx --> take the first one encountered
    filter(mdata_idx == min(mdata_idx)) %>%
    ungroup()

  # information
  if(!quiet) {
    idx_summary <- group_by(matching_idx, column) %>%
      summarize(md_rows = length(unique(mdata_idx)), data_rows = length(unique(data_idx))) %>%
      mutate(label = as.character(glue("{md_rows} metadata entries were mapped to {data_rows} data entries via column '{column}'")))
    glue("Info: metadata added to {nrow(matching_idx)} data rows, {nrow(dt) - nrow(matching_idx)} left without metadata:\n - {collapse(idx_summary$label, '\n - ')}") %>%
      message()
  }

  # prepare metadata
  metadata <- metadata %>%
    # remove the matching columns (since we already have the index matching)
    select(!!!cols_to_quos(dt_cols$match_by, negate = TRUE)) %>%
    # add index column
    mutate(..mdata_idx.. = 1:n(), ..mdata_present.. = TRUE)

  # do the actual matching
  dt %>%
    mutate(..data_idx.. = 1:n()) %>%
    left_join(select(matching_idx, ..data_idx.. = data_idx, ..mdata_idx.. = mdata_idx), by = "..data_idx..") %>%
    left_join(metadata, by = "..mdata_idx..") %>%
    mutate(!!new_cols$has_metadata := !is.na(..mdata_present..)) %>%
    select(-..data_idx.., -..mdata_idx.., -..mdata_present..)
}

#' Fetch entries with missing metadata
#'
#' Fetch data table entries that have missing metadata. This function is typically called after \link{iso_add_metadata} to inspect problematic entries. Returns only unique rows, use the \code{select} parameter to select only the most informative columns.
#'
#' @param dt data with metadata added
#' @param select which columns to select for display - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default but this more useful with a smaller subset of identifying columns.
#' @param has_metadata the name for the column which stores whether an entry in \code{dt} has metadata. Does NOT need to be specified if the default is used for the same paramter in \link{iso_add_metadata}.
#' @export
iso_get_missing_metadata <- function(dt, select = everything(), has_metadata = default(has_metadata), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), select = enquo(select), has_metadata = enquo(has_metadata), n_reqs = list(select = "+"))

  if(!quiet) {
    glue("Info: fetching data entries that are missing metadata") %>%
      message()
  }

  dt %>%
    filter(!(!!sym(dt_cols$has_metadata))) %>%
    dplyr::select(!!!dt_cols$select) %>%
    unique()
}



# peak mapping ======

#' Map peaks based on retention time
#'
#' This function makes it easy to map peaks based on peak maps. It reports all peaks including missing peaks and ambiguous peaks.
#' For routine downstream data processing, this function is usually followed by \code{filter(is_identified, !is_missing, !is_ambiguous)} to proceed only with
#' peaks that do not have any problems. Note that without this filter, one must proceed with great caution interpreting the ambiguous peaks.
#'
#' @param dt data frame with peak data
#' @param peak_maps data frame with the peak map
#' @param file_id the column in dt that holds file id information
#' @param map_id the column in dt that indicates which map to use for which file
#' @param compound the column in peak_maps that holds compound information
#' @param rt the column in dt and colum prefix in peak_maps ("rt:...") that holds retention time information
#' @param rt_start the column in dt that holds start of peak retention times
#' @param rt_end the column in dt that holds end of peak retention times
#' @param rt_prefix_divider the divider after the retention time column prefix in peak_maps to identify the map id values (e.g. "rt:map_id_value")
#' @param is_identified the name for the new column which stores a logical TRUE/FALSE indicating peaks that have been successfully identified (includes missing peaks from the peak map!)
#' (note that this information could also be derived from !is.na(compound) but is provided for convenience)
#' @param is_missing the name for the new column which stores a logical TRUE/FALSE indicating peaks that are in the peak map definition but have no matching peak
#' @param is_ambiguous the name for the new column which stores a logical TRUE/FALSE indicating peaks that are ambiguous in their definition either because
#' they have multiple matches or because they overlap with other, overlapping peaks that were identified the same (note that this information could also be derived
#' from n_overlapping > 1 | n_matches > 1 but is provided for convenience)
#' (note that this information could also be derived from n_overlapping == 0 but is provided for convenience)
#' @param n_matches the name for the new column which stores the number of matches each peak has in the peak map
#' @param n_overlapping the name for the new column which stores the number of overlapping peaks that match the same peak definition
#' @note: TODO - allow multiple file_id, not just one
#' @export
iso_map_peaks <- function(dt, peak_maps, file_id = default(file_id), map_id = default(map_id), compound = default(compound),
                          rt = default(rt), rt_start = default(rt_start), rt_end = default(rt_end), rt_prefix_divider = ":",
                          is_identified = default(is_identified), is_missing = default(is_missing), is_ambiguous = default(is_ambiguous),
                          n_matches = default(n_matches), n_overlapping = default(n_overlapping), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(peak_maps)) stop("no peak maps supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), file_id = enquo(file_id), map_id = enquo(map_id), rt = enquo(rt), rt_start = enquo(rt_start), rt_end = enquo(rt_end))
  pm_cols <- get_column_names(!!enquo(peak_maps), compound = enquo(compound))
  new_cols <- get_new_column_names(is_identified = enquo(is_identified), is_missing = enquo(is_missing), is_ambiguous = enquo(is_ambiguous), n_matches = enquo(n_matches), n_overlapping = enquo(n_overlapping))

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

  # safety check --> look for missing maps
  if (length(missing <- setdiff(unique(dt[[dt_cols$map_id]]), maps[[dt_cols$map_id]])) > 0) {
    glue("the following maps are referenced in the data table but do not exist in the peak maps: '{collapse(missing, \"', '\")}'. Available peak maps: '{collapse(unique(maps[[dt_cols$map_id]]), \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combine peak map data and file data
  found_peaks <-
    dt %>%
    # add unique id per peak for identification simplicity
    mutate(..peak_id.. = 1:n()) %>%
    # join in the specific peak maps
    left_join(maps, by = dt_cols$map_id) %>%
    # find the peak that the retention time window matches
    mutate(..is_target.. = is.na(..rt_target..) | (!!sym(dt_cols$rt_start) <= ..rt_target.. & !!sym(dt_cols$rt_end) >= ..rt_target..)) %>%
    # figure out which peaks match multiple definitions (n_matches) --> the -1 is because all will match the NA placeholder compound
    group_by(!!sym(dt_cols$file_id), ..peak_id..) %>% mutate(..n_matches.. := sum(..is_target..) - 1) %>%
    # figure out which peak definitions match multiple peaks
    group_by(!!sym(dt_cols$file_id), ..rt_target..) %>% mutate(..n_overlapping.. := ifelse(!is.na(!!sym(pm_cols$compound)), sum(..is_target..), 0)) %>%
    ungroup() %>%
    # only keep definitions that fit
    filter( ..is_target.., (..n_matches.. >= 1 & ..n_overlapping.. >=1) | (..n_matches.. == 0 & ..n_overlapping.. == 0) )

  # figure out which peaks are missing based on the expected peaks and the anti_join with the found ones
  missing_peaks <-
    unique(dt[c(dt_cols$file_id, dt_cols$map_id)]) %>%
    left_join(maps, by = dt_cols$map_id) %>%
    filter(!is.na(!!sym(pm_cols$compound))) %>%
    anti_join(found_peaks, by = c(dt_cols$file_id, dt_cols$map_id, pm_cols$compound)) %>%
    mutate(..n_matches.. = 0, ..n_overlapping.. = 0)

  # combine found and missing peaks
  all_data <-
    bind_rows(found_peaks, missing_peaks) %>%
    # fill in convenience information columns and unify rt for missing peaks
    mutate(
      !!new_cols$is_identified := !is.na(compound) > 0,
      !!new_cols$is_missing := is.na(..peak_id..),
      !!new_cols$is_ambiguous := ..n_overlapping.. > 1 | ..n_matches.. > 1,
      !!dt_cols$rt := ifelse(!is.na(!!sym(dt_cols$rt)), !!sym(dt_cols$rt), ..rt_target..)
    )

  # information
  if (!quiet) {
    n_files <- length(unique(all_data[[dt_cols$file_id]]))
    matched_peaks <- all_data %>% filter(!!sym(new_cols$is_identified), !(!!sym(new_cols$is_missing)))
    n_matched_peaks <- matched_peaks %>% select(..peak_id..) %>% unique() %>% nrow()
    n_unnassigned_peaks <- all_data %>% filter(is.na(!!sym(pm_cols$compound))) %>% nrow()
    n_missing_matches <- all_data %>% filter(!!sym(new_cols$is_missing)) %>% nrow()
    n_unambiguous_peaks <- matched_peaks %>% filter(!(!!sym(new_cols$is_ambiguous))) %>% select(..peak_id..) %>% unique() %>% nrow()
    n_multiple_matches <- matched_peaks %>% filter(..n_matches.. > 1) %>% select(..peak_id..) %>% unique() %>% nrow()
    n_overlapping_matches <- matched_peaks %>% filter(..n_overlapping.. > 1) %>% select(..peak_id..) %>% unique() %>% nrow()
    glue("Info: {n_matched_peaks} peaks in {n_files} files were successfully assigned, {n_unnassigned_peaks} could not be assigned, and {n_missing_matches} were missing") %>% message()
    if (n_matched_peaks - n_unambiguous_peaks > 0)
      glue("      {n_unambiguous_peaks} were assigned unambiguously, {n_matched_peaks - n_unambiguous_peaks} were problematic") %>% message()
    if (n_multiple_matches > 0)
      glue("      - {n_multiple_matches} were ambiguous because of multiple matching peak assignments") %>% message()
    if (n_overlapping_matches > 0)
      glue("      - {n_overlapping_matches} were ambiguous because of overlapping peaks that fit the same assignment") %>% message()
  }

  all_data %>%
    # clean up unnecessary columns and rename those with specific names
    select(-..is_target.., -..peak_id.., -..rt_target..)%>%
    rename(!!new_cols$n_matches := ..n_matches..,
           !!new_cols$n_overlapping := ..n_overlapping..) %>%
    # re-arrange by retention time
    arrange(!!sym(dt_cols$file_id), !!sym(dt_cols$rt)) %>%
    # return
    return()
}
