# utility functions that make it easier to support both standard and non-standard evaluation

# column name =====

# Get the column names for a set of parameters referencing columns in a data frame. Compatible with all dplyr type column selection
# criteria including both standard and non-standard evaluation. Throws errors if expressions cannot be evaluated or if an incorrect
# number of columns are identified for a given parameter.
# @param df the data frame
# @param ... named quoted variable selection criteria (anything that tidyselect::vars_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number and "1+" for at least one.
# @return list of column names for each entry (may contain multiple depending on selection doncitoins)
get_column_names <- function(df, ..., n_reqs = list()) {

  # df name and data frame test
  df_name <- enquo(df) %>% quo_text()
  df <- enquo(df) %>% eval_tidy()
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)

  # use a safe version of vars_select to get all the column names
  safe_vars_select <- safely(vars_select)
  cols_quos <- quos(!!!list(...))
  cols_results <- map(cols_quos, ~safe_vars_select(names(df), !!!.x))
  ok <- map_lgl(cols_results, ~is.null(.x$error))
  cols <- map(cols_results, ~as.character(.x$result))

  # summarize if there were any errors
  if (!all(ok)) {
    params <-
      str_c(names(cols_quos)[!ok] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
            map_chr(cols_quos[!ok], quo_text)) %>%
      collapse("', '", last = "' and '")
    errors <- map_chr(cols_results[!ok], ~.x$error$message) %>% collapse("\n- ")
    if (sum(!ok) > 1)
      glue("parameters '{params}' refer to invalid columns in data frame '{df_name}':\n- {errors}") %>%
      stop(call. = FALSE)
    else
      glue("parameter '{params}' refers to invalid column(s) in data frame '{df_name}':\n- {errors}") %>%
      stop(call. = FALSE)
  }

  # check on the number requirements for each column match
  if (any(missing <- !names(n_reqs) %in% names(cols)))
    glue("column requirements for unknow parameter(s) provided: {collapse(names(n_reqs[missing]), ', ')}") %>%
    stop(call. = FALSE)

  all_n_reqs <- rep(1, length(cols)) %>% as.list() %>% setNames(names(cols)) %>% modifyList(n_reqs) %>% { .[names(cols)] }

  col_meets_reqs <- map2_lgl(cols, all_n_reqs, function(col, req) {
    if (is_integerish(req) && length(col) == as.integer(req)) return(TRUE)
    else if (req == "1+" && length(col) > 0) return(TRUE)
    else if (req == "*") return(TRUE)
    else return(FALSE)
  })

  if (!all(col_meets_reqs)) {
    n_errors <-
      sprintf("'%s%s' refers to %d column(s) instead of %s",
              names(cols_quos)[!col_meets_reqs] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
              map_chr(cols_quos[!col_meets_reqs], quo_text),
              map_int(cols[!col_meets_reqs], length),
              map_chr(all_n_reqs[!col_meets_reqs], as.character)) %>%
      collapse("\n- ")
    glue("not all parameters refer to the correct number of columns in data frame '{df_name}':\n- {n_errors}") %>%
      stop(call. = FALSE)
  }

  return(cols)
}

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
    lquos <- quos(!!!map(cols, ~quo(-!!as.name(.x))))
  else
    lquos <- quos(!!!map(cols, as.name))

  # whether to return single elements still as lists
  if (!always_as_list && length(lquos) == 1) return(lquos[[1]])
  return(lquos)
}
