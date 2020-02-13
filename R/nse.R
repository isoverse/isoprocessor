# utility functions that make it easier to support both standard and non-standard evaluation

# resolve defaults ====

# resolve default cols in a list of quos
resolve_defaults <- isoreader:::resolve_defaults

# Test quo expressions by confirming they can create a valid column in a mutate
# @TODO: should this be locally evaluated or at least with tidy_eval just to insert the data frame rather than the whole call stack?
# @note that global variables will be interpreted even in the context of the data frame
# ideally this will be only within the data frame - is that correct?
# see isoreader::local_select_eval for example
check_expressions <- function(df, ...) {

  # df name and data frame test
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  df_name <- enquo(df) %>% rlang::as_label()
  df <- enquo(df) %>% eval_tidy()
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)

  # use a safe version of mutate to check all expressions
  # (use mutate instead of eval_tidy to make sure it's absolutely valid)
  safe_eval <- safely(mutate)
  expr_quos <- quos(!!!list(...)) %>%
    # make sure to evaluate calls to default
    resolve_defaults() %>%
    # make sure that the expressions are locally evaluated
    map(~quo(!!get_expr(.x)))
  expr_quos <- expr_quos[!map_lgl(expr_quos, quo_is_null)]
  expr_errors <- map(expr_quos, ~safe_eval(df, !!.x)$error)

  # check results
  ok <- map_lgl(expr_errors, ~is.null(.x))

  # summarize if there were any errors
  if (!all(ok)) {
    params <-
      map2_chr(names(expr_quos)[!ok], expr_quos[!ok], function(var, val) {
        if (nchar(var) > 0 && var != rlang::as_label(val)) str_c(var, " = ", rlang::as_label(val))
        else rlang::as_label(val)
      }) %>%
      collapse("', '", last = "' and '")
    errors <- map_chr(expr_errors[!ok], ~.x$message) %>% collapse("\n- ")
    err_msg <-
      if (sum(!ok) > 1)
        glue("'{params}' are invalid expressions in data frame '{df_name}':\n- {errors}")
    else
      glue("'{params}' is not a valid expression in data frame '{df_name}':\n- {errors}")
    stop(err_msg, call. = FALSE)
  }

  return(invisible(df))
}
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
