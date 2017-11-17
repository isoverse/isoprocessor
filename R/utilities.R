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
  dt_cols <- get_column_names(!!enquo(dt), select = enquo(select), n_reqs = list(select = "1+"))
  filter_quo <- enquo(filter)

  # print knitr table
  dt %>% dplyr::filter(!!filter_quo) %>%
    dplyr::select(!!!dt_cols$select) %>%
    knitr::kable(...) %>%
    print()

  return(invisible(dt))
}
