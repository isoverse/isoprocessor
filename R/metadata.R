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
#' @family metadata functions
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
#' @family metadata functions
#' @export
iso_get_missing_metadata <- function(dt, select = everything(), has_metadata = default(has_metadata), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), select = enquo(select), has_metadata = enquo(has_metadata), n_reqs = list(select = "+"))

  # info
  if(!quiet) {
    glue("Info: fetching data entries that are missing metadata") %>%
      message()
  }

  # filtering
  dt %>%
    filter(!(!!sym(dt_cols$has_metadata))) %>%
    dplyr::select(!!!dt_cols$select) %>%
    unique()
}


#' Remove entries with missing metadata
#'
#' Continue data processing without data that is missing metadata. This function is typically called after \link{iso_add_metadata} to focus on entries that have metadata.
#'
#' @inheritParams iso_get_missing_metadata
#' @family metadata functions
#' @export
iso_remove_missing_metadata <- function(dt, has_metadata = default(has_metadata), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), has_metadata = enquo(has_metadata))

  # filtering
  dt_out <- filter(dt, !!sym(dt_cols$has_metadata))

  # info
  if(!quiet) {
    glue("Info: removing {nrow(dt) - nrow(dt_out)} of {nrow(dt)} data entries because of missing metadata") %>%
      message()
  }

  return(dt_out)
}

