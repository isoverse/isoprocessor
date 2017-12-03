#' Map peaks based on retention time
#'
#' This function makes it easy to map peaks based on peak maps. It reports all peaks including missing peaks and ambiguous peaks.
#' For routine downstream data processing, this function is usually followed by \code{filter(is_identified, !is_missing, !is_ambiguous)} to proceed only with
#' peaks that do not have any problems. Note that without this filter, one must proceed with great caution interpreting the ambiguous peaks.
#'
#' @param dt data frame with peak data
#' @param peak_maps data frame with the pak map
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
    filter(!is.na(!!as.name(pm_cols$compound))) %>%
    # add NA compound to map undefined peaks
    bind_rows(data_frame(compound = NA)) %>% unique() %>%
    # gather map retention times
    gather(..map_id.., ..rt_target.., starts_with(dt_cols$rt)) %>%
    # replace the rt prefix to get to the actual map name
    mutate(!!as.name(dt_cols$map_id) := str_replace(..map_id.., fixed(str_c(dt_cols$rt, rt_prefix_divider)), "")) %>%
    select(-..map_id..) %>%
    # only keep the NA compound and everything that has a ..rt_target..
    filter(is.na(UQ(as.name(pm_cols$compound))) | !is.na(..rt_target..))

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
    # find the peak that the retention time window
    mutate(..is_target.. = is.na(..rt_target..) | (UQ(as.name(dt_cols$rt_start)) <= ..rt_target.. & UQ(as.name(dt_cols$rt_end)) >= ..rt_target..)) %>%
    # figure out which peaks match multiple definitions (n_matches) --> the -1 is because all will match the NA placeholder compound
    group_by(UQ(as.name(dt_cols$file_id)), ..peak_id..) %>% mutate(..n_matches.. := sum(..is_target..) - 1) %>%
    # figure out which peak definitions match multiple peaks
    group_by(UQ(as.name(dt_cols$file_id)), ..rt_target..) %>% mutate(..n_overlapping.. := ifelse(!is.na(UQ(as.name(pm_cols$compound))), sum(..is_target..), 0)) %>%
    ungroup() %>%
    # only keep definitions that fit
    filter( ..is_target.., (..n_matches.. >= 1 & ..n_overlapping.. >=1) | (..n_matches.. == 0 & ..n_overlapping.. == 0) )
    #filter( (!..is_target.. & ..n_overlapping.. == 0) | (..is_target.. & ..n_matches.. >= 1 & ..n_overlapping.. >=1) | (..is_target.. & ..n_matches.. == 0 & ..n_overlapping.. == 0) ) %>%
    # fill in convenience information columns
    # mutate(
    #   !!as.name(new_cols$is_identified) := n_matches > 0,
    #   !!as.name(new_cols$is_missing) := FALSE,
    #
    # ) %>%
    # # remove temp field is_target
    # select(-is_target) %>%
    # # make sure listed peaks that have no matches are captured
    # full_join(filter(maps, file_id %in% unique(dt$file_id))) %>% # dynamically join by all fields
    # # arrange in useful order
    # mutate(Rt_arrange = ifelse(!is.na(Rt_target), Rt_target, Rt)) %>%
    # arrange(file_id, Rt_arrange, Nr.) %>% select(-Rt_arrange) %>%
    # # remove redundant no peak placeholder
    # filter(!(is.na(Nr.) & is.na(compound))) %>%
    # # remove all files that are not supposed to be processed
    # filter(process == "yes") %>%

  # figure out which peaks are missing based on the expected peaks and the anti_join with the found ones
  missing_peaks <-
    unique(dt[c(dt_cols$file_id, dt_cols$map_id)]) %>%
    left_join(maps, by = dt_cols$map_id) %>%
    filter(!is.na(UQ(as.name(pm_cols$compound)))) %>%
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
      !!dt_cols$rt := ifelse(!is.na(UQ(as.name(dt_cols$rt))), UQ(as.name(dt_cols$rt)), ..rt_target..)
    )

  # information
  if (!quiet) {
    n_files <- length(unique(all_data[[dt_cols$file_id]]))
    matched_peaks <- all_data %>% filter(UQ(as.name(new_cols$is_identified)), !UQ(as.name(new_cols$is_missing)))
    n_matched_peaks <- matched_peaks %>% select(..peak_id..) %>% unique() %>% nrow()
    n_unnassigned_peaks <- all_data %>% filter(is.na(UQ(as.name(pm_cols$compound)))) %>% nrow()
    n_missing_matches <- all_data %>% filter(UQ(as.name(new_cols$is_missing))) %>% nrow()
    n_unambiguous_peaks <- matched_peaks %>% filter(!UQ(as.name(new_cols$is_ambiguous))) %>% select(..peak_id..) %>% unique() %>% nrow()
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
    arrange(!!as.name(dt_cols$file_id), !!as.name(dt_cols$rt)) %>%
    # return
    return()
}
