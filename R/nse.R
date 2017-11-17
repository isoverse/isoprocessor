# utility functions that make it easier to support both standard and non-standard evaluation

# column name =====

# Get the column names for a set of parameters referencing columns in a data frame. Compatible with all dplyr type column selection
# criteria including both standard and non-standard evaluation. Throws errors if expressions cannot be evaluated or if an incorrect
# number of columns are identified for a given parameter.
# @param df the data frame
# @param ... named quoted variable selection criteria (anything that tidyselect::vars_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @return list of column names for each entry (may contain multiple depending on selection doncitoins)
get_column_names <- function(df, ..., n_reqs = list()) {

  # df name and data frame test
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  df_name <- enquo(df) %>% quo_text()
  df <- enquo(df) %>% eval_tidy()
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)

  # use a safe version of vars_select to get all the column names
  safe_vars_select <- safely(vars_select)
  cols_quos <- quos(!!!list(...))
  # make sure to evaluate calls to default
  cols_quos <- map(cols_quos, function(x) if (quo_is_lang(x) && lang_head(x) == sym("default")) eval_tidy(x) else x)
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

  ## reqs lables
  all_n_reqs <- rep(1, length(cols)) %>% as.list() %>% setNames(names(cols)) %>% modifyList(n_reqs) %>% { .[names(cols)] }
  n_req_types <- c("*" = "any number", "+" = "at least one", "?" = "none or one", "integer" = "the specified number")
  all_n_req_types <- map_chr(all_n_reqs, function(req) {
    if (is_integerish(req)) return("integer")
    else if (req %in% names(n_req_types)) return(req)
    else return(NA_character_)
  })
  if ( any(unknown <- map_lgl(all_n_req_types, is.na))) {
    n_req_unknown <- map_chr(all_n_reqs[unknown], as.character)
    glue("unknown number requirement specification(s): '{collapse(n_req_unknown, \"', '\")}'. Allowed are: {collapse(names(n_req_types), ', ')}") %>%
      stop(call. = FALSE)
  }

  ## reqs test
  col_meets_reqs <- map2_lgl(cols, all_n_reqs, function(col, req) {
    if (is_integerish(req) && length(col) == as.integer(req)) return(TRUE)
    else if (req == "+" && length(col) > 0) return(TRUE)
    else if (req == "?" && length(col) %in% c(0L, 1L)) return(TRUE)
    else if (req == "*") return(TRUE)
    else return(FALSE)
  })

  ## report missing columns
  if (!all(col_meets_reqs)) {
    n_errors <-
      sprintf("'%s%s' refers to %d column(s) instead of %s (%s)",
              names(cols_quos)[!col_meets_reqs] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
              map_chr(cols_quos[!col_meets_reqs], quo_text),
              map_int(cols[!col_meets_reqs], length),
              all_n_req_types[!col_meets_reqs],
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
