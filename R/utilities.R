# stringr utility for single match ====

# extract a specific matched group
# @param n = 1
str_extract_group <- function(string, pattern, n = 1) {
  matches <- str_match(string, pattern)
  if (ncol(matches) < (n+1))
    stop(glue("regexp capture group {n} requested but only {ncol(matches)-1} groups captured"), call. = FALSE)
  matches[, n+1]
}

# information recovery ====

#' Cleanup text vectors
#' Convenience function to convert text vectors that should be numeric but have a text prefix and/or suffix that interferes with conversion.
#' @param x the character vector
#' @param allow_negative whether to allow negative numbers
#' @param trim_text_prefix whether to trim the prefix (removes any prefix up to a continuous set of numbers)
#' @param trim_text_suffix whether to trim the suffix (removes any non-number suffix)
#' @export
cleanup_numeric <- function(x, allow_negative = TRUE, trim_text_prefix = TRUE, trim_text_suffix = TRUE) {

  # safety checks
  if (missing(x)) stop("parameter x not supplied", call. = FALSE)
  if (is.numeric(x)) return(x) # already numeric
  if (!is.character(x)) stop(glue("x is not a character vector but of type {class(x)[1]}"), call. = FALSE)

  # number regexp
  number_regexp <- "\\d+\\.?\\d*"
  if (allow_negative) number_regexp <- sprintf("-?%s", number_regexp)

  # full regexp
  if (trim_text_prefix && trim_text_suffix) {
    regexp <- sprintf("(%s)[^\\d]*$", number_regexp)
  } else if (trim_text_prefix && !trim_text_suffix) {
    regexp <- sprintf("(%s)$", number_regexp)
  } else if (!trim_text_prefix && trim_text_suffix) {
    regexp <- sprintf("^(%s)", number_regexp)
  } else {
    regexp <- sprintf("^(%s)$", number_regexp)
  }

  # capture number
  x_num <- str_extract_group(x, regexp, n = 1)
  as.numeric(x_num)
}

# information display ====

#' Print data table
#'
#' Convenience function to print a data table as part of a pipe.
#'
#' @param dt data table
#' @param select which columns to select (use c(...) to select multiple), supports all \link[dplyr]{select} syntax
#' @param filter any filter conditions to apply, by default does not filter any
#' @param ... additional parameters to \link[knitr]{kable}
#' @return the passed in data table (for piping)
#' @export
print_data_table <- function(dt, select = everything(), filter = TRUE, ...) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), select = enquo(select), n_reqs = list(select = "+"))
  filter_quo <- enquo(filter)

  # print knitr table
  dt %>% dplyr::filter(!!filter_quo) %>%
    dplyr::select(!!!dt_cols$select) %>%
    knitr::kable(...) %>%
    print()

  return(invisible(dt))
}

# nesting ======

# nest data based on the grouping
# this is basically just an inverse version (in terms of selection) of nest that can handle the sophisticated selection parameters of select
# @param group_by what to keep out of the nested data (i.e. what to group by), by default nests everything
nest_data <- function(dt, group_by = NULL, nested = nested_data) {

  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), group_by = enquo(group_by), n_reqs = list(group_by = "*"))
  nested_col <- resolve_defaults(enquo(nested))

  # perform the nest
  dt %>%
    as_data_frame() %>% # nest requires tbl
    nest(!!!cols_to_quos(dt_cols[c("group_by")], negate = TRUE), .key = !!nested_col)
}

# unnest parts of a data frame without loosing the rest
# note that this will lead to row duplication if the unnested variables have multiple entries per row of the \code{dt} data frame
# @param keep other nested data
unnest_select_data <- function(dt, select = c(), nested = nested_data, keep_other_nested_data = TRUE) {
  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), nested = enquo(nested))
  dt <- mutate(dt, ..row.. = row_number()) %>% as_data_frame()
  unnested_dt <- unnest(dt, !!as.name(dt_cols$nested))

  # figure out what the columns are
  original_cols <- setdiff(names(dt), dt_cols$nested)
  other_nested_cols <- setdiff(original_cols, names(unnested_dt))
  regular_cols <- setdiff(original_cols, other_nested_cols)
  select_cols <- get_column_names(unnested_dt, select = enquo(select), n_reqs = list(select = "*"))
  renesting_cols <- setdiff(names(unnested_dt), c(regular_cols, select_cols$select))

  # renest without the selected parameters
  if (length(renesting_cols) > 0)
    renested_dt <- unnested_dt %>% nest_data(group_by = c(regular_cols, select_cols$select), nested = !!as.name(dt_cols$nested))
  else
    renested_dt <- unnested_dt # implies complete unnesting

  # merge the extra columns back in
  if (keep_other_nested_data && length(other_nested_cols) > 0) {
    renested_dt <- left_join(renested_dt, dt[c("..row..", other_nested_cols)], by = "..row..")
  }

  # remove the ..row.. again (just used for ID purposes)
  return(dplyr::select(renested_dt, -..row..))
}


# regressions =====

run_regression <- function(dt, ..., nested = nested_data, calculate_residuals = TRUE) {
  # note for each model (...), does the calculation, adds a model parameter + fit + residuals row
  # i.e. you get one row for each calculation
  # a bit like gather residuals except that it is all in one
}

run_regression_more <- function(dt, ...) {
  # this one should do the nesting, regression analyses and unnesting all in one

}

unnest_model_parameters <- function(dt, ..., include_r2 = TRUE, include_rms = TRUE) {
  # unnest the model estimates as R2 and RMS
}


