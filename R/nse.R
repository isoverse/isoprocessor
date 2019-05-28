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
# @param ... named quoted variable selection criteria (anything that tidyselect::vars_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @param type_reqs named list to specify what types certain columns must be, allowed: "list", "numeric", "integer", "character", "logical"
# @param cols_must_exist - if TRUE, will throw an error if a column does not exist, otherwise just  warning
# @return list of column names for each entry (may contain multiple depending on selection conditions)
get_column_names <- isoreader:::get_column_names

# Get new columns names
# Resolves defaults and checks to make sure that the column name refers to a valid single symbol.
# @param ... named quos
# @return list of named strings for each new column
get_new_column_names <- function(...) {
  cols_quos <- quos(!!!list(...))
  # make sure to evaluate calls to default
  cols_quos %>% resolve_defaults() %>% quos_to_text(variable = "column")
}

# Convert quo to text accounting for plain text and symbol quos
quos_to_text <- isoreader:::quos_to_text

# @NOTE: deprecate this with direct calls to map...sym, instead?
# Turn a column or set of columns into quosures (a list of quosure objects) for use in tidyverse functions
# such as group_by, nest, select, rename, etc.
# @param cols column names
# @param always_as_list if TRUE, returns even a single quosure as a list of quosures
# @param keep_names if TRUE, keeps names and can be used for e.g. renaming purposes
# @param negate whether to select the opposite
cols_to_quos <- function(cols, always_as_list = FALSE, keep_names = FALSE, negate = FALSE) {
  if (!all(ok <- map_lgl(cols, is.character)))
    stop("can only convert character column names to quos", call. = FALSE)
  cols <- unlist(cols)
  if (!keep_names) cols <- unname(cols)

  # whether to negate or not
  if (negate)
    lquos <- quos(!!!map(cols, ~quo(-!!sym(.x))))
  else
    lquos <- quos(!!!map(cols, sym))

  # whether to return single elements still as lists
  if (!always_as_list && length(lquos) == 1) return(lquos[[1]])
  return(lquos)
}

# Turn column into a list of symbols (this is an alternative to cols_to_quos for functions that can't deal with the quosures)
cols_to_symbol_list <- function(cols, keep_names = FALSE) {
  if (!all(ok <- map_lgl(cols, is.character)))
    stop("can only convert character column names to quos", call. = FALSE)
  cols <- unlist(cols)
  if (!keep_names) cols <- unname(cols)
  return(map(cols, sym))
}
