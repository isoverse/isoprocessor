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
nest_data <- function(dt, group_by = NULL, nested_data = default(nested_data)) {

  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), group_by = enquo(group_by), n_reqs = list(group_by = "*"))
  nested_col <- resolve_defaults(enquo(nested_data))

  # perform the nest
  dt %>%
    as_data_frame() %>% # nest requires tbl
    nest(!!!cols_to_quos(dt_cols[c("group_by")], negate = TRUE), .key = !!nested_col)
}

# unnest parts of a data frame without loosing the rest
# note that this will lead to row duplication if the unnested variables have multiple entries per row of the \code{dt} data frame
# @param select which columns to unnest
# @param nested_data which column to unnest the \code{select} from
# @param keep_remaining_nested_data whether to keep any remaining parts of the partially unnested data
# @param keep_other_list_data keep other list data columns (e.g. other data or model columns)
unnest_select_data <- function(dt, select = everything(), nested_data = default(nested_data), keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE) {
  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), nested_data = enquo(nested_data), type_reqs = list(nested_data = "list"))
  dt <- mutate(dt, ..row.. = row_number()) %>% as_data_frame()

  # keep track of the different types of columns
  list_cols <- dt %>% map_lgl(is_list) %>% { names(.)[.] } %>% { .[.!=dt_cols$nested_data] }
  regular_cols <- setdiff(names(dt), c(list_cols, dt_cols$nested))

  # only unnest the main list column
  unnested_dt <- unnest(dt[c(regular_cols, dt_cols$nested_data)], !!as.name(dt_cols$nested_data))

  # safety check on whether the select columns exist in the unnested df
  select_cols <- get_column_names(unnested_dt, select = enquo(select), n_reqs = list(select = "*"))

  # renest without the selected parameters
  keep_cols <- c(regular_cols, select_cols$select) %>% unique()
  if (length(setdiff(names(unnested_dt), keep_cols)) > 0 && keep_remaining_nested_data) {
    # renest if un-nesting is incomplete (i.e. data remains) and remaining data should be kept
    renested_dt <- unnested_dt %>% nest_data(group_by = keep_cols, nested = !!as.name(dt_cols$nested_data))
  } else
    renested_dt <- unnested_dt[keep_cols]

  # merge the extra columns back in
  if (keep_other_list_data && length(list_cols) > 0) {
    renested_dt <- left_join(renested_dt, dt[c("..row..", list_cols)], by = "..row..")
  }

  # remove the ..row.. again (just used for ID purposes)
  return(dplyr::select(renested_dt, -..row..))
}


# convenience function for unnesting coefficients
# @param select which coefficient fields to unnest
unnest_model_coefficients <- function(dt, select = everything(), model_coefs = default(model_coefs),
                                      keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {
  unnest_select_data(dt, select = UQ(enquo(select)), nested_data = !!enquo(model_coefs),
                     keep_remaining_nested_data = keep_remaining_nested_data,
                     keep_other_list_data = keep_other_list_data)
}

# convenience function for unnesting coefficients
# @param select which coefficient fields to unnest
unnest_model_summary <- function(dt, select = everything(), model_summary = default(model_summary),
                                      keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {
  unnest_select_data(dt, select = UQ(enquo(select)), nested_data = !!enquo(model_summary),
                     keep_remaining_nested_data = keep_remaining_nested_data,
                     keep_other_list_data = keep_other_list_data)
}

# regressions =====

# run a set of regressions
# @param model_data the nested model data column
# @param model_name name of the new model column (uses the names of ...)
# @param model_fit the new model objects column
# @param model_coefs the new model coefficients nested data frame column
# @param model_summary the new model summary nested data frame column
# @param residual the new residual column in the nested model_data
# @param residuals resid name of the residuals column
run_regression <- function(dt, ..., model_data = default(model_data),
                           model_name = default(model_name), model_fit = default(model_fit),
                           model_coefs = default(model_coefs), model_summary = default(model_summary),
                           residual = default(residual)) {

  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), model_data = enquo(model_data), type_reqs = list(model_data = "list"))
  dt_new_cols <- get_new_column_names(
    model_name = enquo(model_name), model_fit = enquo(model_fit),
    model_coefs = enquo(model_coefs), model_summary = enquo(model_summary),
    residual = enquo(residual))

  # models
  lquos <- quos(...)
  if (length(lquos) == 0) stop("no regression models supplied", call. = FALSE)
  lquos_are_models <- map_lgl(lquos, function(lq) quo_is_lang(lq) && quo_text(lang_head(lq)) %in% c("lm", "glm"))
  if(!all(ok <- lquos_are_models)) {
    params <-
      str_c(names(lquos)[!ok] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
            map_chr(lquos[!ok], quo_text)) %>%
      collapse("', '", last = "' and '")
    if (sum(!ok) > 1)
      glue("parameters '{params}' do not refer valid models (lm or glm)") %>% stop(call. = FALSE)
    else
      glue("parameter '{params}' does not refer to a valid model (lm or glm)") %>% stop(call. = FALSE)
  }
  models <- data_frame(
    !!dt_new_cols$model_name := names(lquos),
    model_quo = lquos
  )

  # check for model names
  if (any(dups <- duplicated(models[[dt_new_cols$model_name]]))){
    dup_names <- models[[dt_new_cols$model_name]][dups]
    glue("regressions with multiple models require unique model names, encountered duplicate name(s) '{collapse(dup_names, \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combination of data and model
  data_w_models <-
    dt %>%
    merge(models) %>%
    as_data_frame() %>%
    # evaluation of model
    mutate(
      !!dt_new_cols$model_fit := map2(model_quo, !!as.name(dt_cols$model_data), ~eval_tidy(.x, data = .y)),
      !!dt_new_cols$model_coefs := map(
        !!as.name(dt_new_cols$model_fit),
        ~mutate(as_data_frame(tidy(.x)),
                # add in significant level summary
                signif = ifelse(p.value < 0.001, "***",
                                ifelse(p.value < 0.01, "**",
                                       ifelse(p.value < 0.05, "*",
                                              ifelse(p.value < 0.1, ".", ""))))
        )),
      !!dt_new_cols$model_summary := map(!!as.name(dt_new_cols$model_fit), ~as_data_frame(glance(.x)))
    )

  # add residuals
  data_w_models <-
    data_w_models %>%
    mutate(
      !!dt_cols$model_data := map2(!!as.name(dt_cols$model_data), !!as.name(dt_new_cols$model_fit),
                               ~add_residuals(.x, .y, var = dt_new_cols$residual))
    )

  return(select(data_w_models, -model_quo))
}

# run regressions in grouped blocks (uses nest_data and run_regression)
run_grouped_regression <- function(dt, group_by = NULL, ..., model_data = default(model_data)) {
  # this one should do the nesting, regression analyses all in once
  model_data_quo <- enquo(model_data)
  nest_data(dt, group_by = !!enquo(group_by), nested_data = !!model_data_quo) %>%
    run_regression(..., model_data = !!model_data_quo)
}


# invert regression =====

#' invert the regression for calibration purposes
invert_regression <- function() {

}
