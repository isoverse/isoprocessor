# utility functions that make it easier to support both standard and non-standard evaluation

# resolve defaults ====

# resolve default cols in a list of quos
resolve_defaults <- isoreader:::resolve_defaults
check_expressions <- isoreader:::check_expressions

# column name =====

# Get the column names for a set of parameters referencing columns in a data frame. Compatible with all dplyr type column selection
# criteria including both standard and non-standard evaluation. Throws errors if expressions cannot be evaluated or if an incorrect
# number of columns are identified for a given parameter.
# @param df the data frame
# @param ... named expressions with variable selection criteria (anything that tidyselect::eval_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @param type_reqs named list to specify what types certain columns must be, allowed: "list" (also includes "vctrs_list_of"), "numeric", "integer", "character", "logical"
# @param cols_must_exist - if TRUE, will throw an error if a column does not exist, otherwise just  warning
# @return list of column names for each entry (may contain multiple depending on selection conditions)
get_column_names <- isoreader:::get_column_names

# Get new columns names
# Resolves defaults and checks to make sure that the column name refers to a valid single symbol.
# @param ... named quos
# @return list of named strings for each new column
get_new_column_names <- function(...) {
  # make sure to evaluate calls to default
  cols_quos <- quos(!!!list(...)) %>% resolve_defaults()
  # get columns text names
  cols_quos %>% purrr::map(
    ~{
      if (rlang::is_symbol(.x)) rlang::as_name(.x)
      else if (rlang::is_quosure(.x) && rlang::quo_is_symbol(.x)) rlang::as_name(.x)
      else if (rlang::is_quosure(.x) && quo_is_value(.x)) as.character(rlang::eval_tidy(.x))
      else if (is.character(.x)) .x
      else stop("not a valid column name '", rlang::as_label(.x), "'", call. = FALSE)
    }
  )
}

# aesthetics quos ========

# convert an aesthetics quo into quos for generating mutate quos to make all the columns necessary for the aesthetics
# can deal with multiple expressions encapsulated by c() and list()
aesthetics_quo_to_mutate_quos <- function(q, drop_null = TRUE, drop_missing = TRUE) {
  if (rlang::quo_is_call(q) && rlang::call_name(q) %in% c("c", "list")) {
    qs <- quos(!!!rlang::call_args(q))
  } else {
    qs <- quos(!!q)
  }
  if (drop_null)
    qs <- qs[!map_lgl(qs, rlang::quo_is_null)]
  if (drop_missing)
    qs <- qs[!map_lgl(qs, rlang::quo_is_missing)]
  names(qs)[names(qs) == ""] <- purrr::map_chr(qs[names(qs) == ""], rlang::as_label)
  return(qs)
}

# check if a quo is non-symbolic, non-call
quo_is_value <- function(q) {
  return(!rlang::quo_is_symbol(q) && !rlang::quo_is_call(q))
}
# check if quos are non-symbolic, non-call
quos_are_values <- function(qs) {
  return(map_lgl(qs, quo_is_value))
}
# check if quos are unnamed non-symbolic, non-call
# here: unnamed = name != value
quos_are_unnamed_values <- function(qs) {
  return(quos_are_values(qs) & names(qs) == map_chr(qs, rlang::as_label))
}
