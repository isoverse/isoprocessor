#' @export
magrittr::`%>%`

# just because it always is very confusing error if the base `filter` is used instead
#' @export
dplyr::filter

# information display ====

#' Print data table
#'
#' Convenience function to print a data table as part of a pipe.
#'
#' @param dt data table
#' @param select which columns to select (use c(...) to select multiple), supports all \link[dplyr]{select} syntax
#' @param filter any filter conditions to apply, by default does not filter any
#' @param print_func what function to use for printing (makes it easier to switch between different types of output)
#' @param title whether to provide a title message
#' @param unique whether to print only unique rows
#' @inheritParams iso_show_default_processor_parameters
#' @return the passed in data table (for piping)
#' @export
#' @note this is not working as well as planned - consider removing again or just allowing it to be a simplifying function..
iso_print_data_table <- function(dt, select = everything(), filter = TRUE, print_func = default(print_func), title = NULL, unique = TRUE, ...) {

  stop("iso_print_data_table is deprecated because of confusing behaviour. Please print the relevant information directly to console.", call. = FALSE)

}


# nesting ======

# note - documented but not exported
#' nest data based on the grouping
#' this is basically just an inverse version (in terms of selection) of nest that can handle the sophisticated selection parameters of select
#' @param group_by what to keep out of the nested data (i.e. what to group by), by default nests everything
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

#' Remove nested data
#'
#' Remove all nested data sets in the data table (e.g. in prep for printing with kable)
#' @note not unit tested
#' @export
iso_remove_nested_data <- function(dt) {

  if (missing(dt)) stop("no data table supplied", call. = FALSE)

  list_cols <- dt %>% map_lgl(is_list)
  dt[!list_cols]
}

# note - documented but not exported
#' unnest parts of a data frame without loosing the rest
#'
#' note that this will lead to row duplication if the unnested variables have multiple entries per row of the \code{dt} data frame
#' also note that this will remove rows that have NULL in the nested_data column
#' @param select which columns to unnest from the nested data
#' @param nested_data which column to unnest the \code{select} from
#' @param keep_remaining_nested_data whether to keep any remaining parts of the partially unnested data
#' @param keep_other_list_data keep other list data columns (e.g. other data or model columns)
unnest_select_data <- function(dt, select = everything(), nested_data = default(nested_data), keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE, keep_only_unique = TRUE) {
  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), nested_data = enquo(nested_data), type_reqs = list(nested_data = "list"))

  # add row number and remove NULL columns
  dt <-
    dt %>%
    filter(!map_lgl(!!sym(dt_cols$nested_data), is.null)) %>%
    mutate(..row.. = row_number()) %>%
    as_data_frame()

  # keep track of the different types of columns
  list_cols <- dt %>% map_lgl(is_list) %>% { names(.)[.] } %>% { .[.!=dt_cols$nested_data] }
  regular_cols <- setdiff(names(dt), c(list_cols, dt_cols$nested))

  # only unnest the main list column
  unnested_dt <- unnest(dt[c(regular_cols, dt_cols$nested_data)], !!sym(dt_cols$nested_data))

  # safety check on whether the select columns exist in the unnested df
  select_cols <- get_column_names(unnested_dt, select = enquo(select), n_reqs = list(select = "*"))

  # renest without the selected parameters
  keep_cols <- c(regular_cols, select_cols$select) %>% unique()
  if (length(setdiff(names(unnested_dt), keep_cols)) > 0 && keep_remaining_nested_data) {
    # renest if un-nesting is incomplete (i.e. data remains) and remaining data should be kept
    renested_dt <- unnested_dt %>% nest_data(group_by = keep_cols, nested = !!sym(dt_cols$nested_data))
  } else
    renested_dt <- unnested_dt[keep_cols]

  # merge the extra columns back in
  if (keep_other_list_data && length(list_cols) > 0) {
    renested_dt <- left_join(renested_dt, dt[c("..row..", list_cols)], by = "..row..")
  }

  # return
  renested_dt %>%
    # make sure no replication if only partial dataframe is unnested and rows are replicated despite remaining unique
    # includes ..row.. on purpose to make sure no unanticipated row collapes is possible
    unique() %>%
    # remove the ..row.. again (just used for ID purposes)
    dplyr::select(-..row..) %>%
    return()
}

# note - documented but not exported
#' Unnest model results
#'
#' Convenience functions for unnesting model results
#' @param model_results name of the model results column to unnest
#' @param select which fields from the selected model result column to unnest
#' @inheritParams unnest_select_data
unnest_model_results <- function(dt, model_results, select = everything(),
                                 keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {
  if (missing(model_results)) stop("specify which model results column to unnest", call. = FALSE)
  unnest_select_data(dt, select = !!(enquo(select)), nested_data = !!enquo(model_results),
                     keep_remaining_nested_data = keep_remaining_nested_data,
                     keep_other_list_data = keep_other_list_data)
}

# regressions =====

# note - documented but not exported
#' run a set of regressions
#'
#' @param dt data table
#' @param model the regression model or named list of regression models. If a named list is provided, the name(s) will be stored in the \code{model_name} column instead of the formula.
#' @param model_data the nested model data column
#' @param model_filter_condition a filter to apply to the data before running the regression, by default no filter
#' @param model_name new column with the model formulae or names if supplied
#' @param model_fit the new model objects column
#' @param model_coefs the new model coefficients nested data frame column
#' @param model_summary the new model summary nested data frame column
#' @param residual the new residual column in the nested model_data
#' @param residuals resid name of the residuals column
#' @param min_n_data_points - the minimum number of data points required for the regression
run_regression <- function(dt, model, model_data = default(model_data), model_filter_condition = default(),
                           model_name = default(model_name), model_fit = default(model_fit),
                           model_coefs = default(model_coefs), model_summary = default(model_summary),
                           residual = default(residual), min_n_data_points = 2) {

  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_quo <- enquo(dt)
  dt_cols <- get_column_names(!!dt_quo, model_data = enquo(model_data), type_reqs = list(model_data = "list"))
  dt_new_cols <- get_new_column_names(
    model_name = enquo(model_name), model_fit = enquo(model_fit),
    model_coefs = enquo(model_coefs), model_summary = enquo(model_summary),
    residual = enquo(residual))
  filter_quo <- enquo(model_filter_condition) %>% resolve_defaults() %>%
    { if(quo_is_missing(.)) quo(TRUE) else . }

  # models
  if (missing(model)) stop("no regression model supplied", call. = FALSE)
  model_quos <- enquo(model)
  # resolve list of models
  if (quo_is_lang(model_quos) && quo_text(lang_head(model_quos)) %in% c("c", "list")) {
    lquos <- quos(!!!lang_args(model_quos))
  } else {
    lquos <- quos(!!!model_quos)
  }

  # safety checks on models
  if (length(lquos) == 0) stop("no regression model supplied", call. = FALSE)
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

  # models data frame
  models <-
    data_frame(
      model_formula = map_chr(lquos, quo_text),
      !!dt_new_cols$model_name := ifelse(nchar(names(lquos)) > 0, names(lquos), model_formula),
      model_quo = lquos
    ) %>%
    # don't keep separate formula column
    select(-model_formula)

  # check for model names
  if (any(dups <- duplicated(models[[dt_new_cols$model_name]]))){
    dup_names <- models[[dt_new_cols$model_name]][dups]
    glue("regressions with multiple models require unique model formulae or names (if specified), encountered duplicates: '{collapse(dup_names, \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combination of data and model
  data_w_models <-
    eval_tidy(dt_quo) %>%
    merge(models) %>%
    as_data_frame() %>%
    # evaluation of model
    mutate(
      ..has_enough_data.. = map_int(!!sym(dt_cols$model_data), ~nrow(filter(.x, !!filter_quo)) >= min_n_data_points),
      !!dt_new_cols$model_fit :=
        pmap(list(m = model_quo, d = !!sym(dt_cols$model_data), run = ..has_enough_data..),
             function(m, d, run) if (run) eval_tidy(m, data = filter(d, !!filter_quo)) else NULL),
      !!dt_new_cols$model_coefs := map2(
        !!sym(dt_new_cols$model_fit), ..has_enough_data..,
        ~if (.y) {
          mutate(as_data_frame(tidy(.x)),
                # add in significant level summary
                signif = ifelse(p.value < 0.001, "***",
                                ifelse(p.value < 0.01, "**",
                                       ifelse(p.value < 0.05, "*",
                                              ifelse(p.value < 0.1, ".", "")))))
        } else NULL),
      !!dt_new_cols$model_summary :=
        map2(!!sym(dt_new_cols$model_fit), ..has_enough_data..,
             ~if (.y) { as_data_frame(glance(.x)) } else { NULL })
    )

  # add residuals
  data_w_models <-
    data_w_models %>%
    mutate(
      !!dt_cols$model_data :=
        pmap(list(d = !!sym(dt_cols$model_data), fit = !!sym(dt_new_cols$model_fit), run = ..has_enough_data..),
             function(d, fit, run) if (run) add_residuals(d, fit, var = dt_new_cols$residual) else d)
    )


  return(select(data_w_models, -model_quo, -..has_enough_data..))
}

# run regressions in grouped blocks (uses nest_data and run_regression)
# @note is this really used or should it be deprecated?
run_grouped_regression <- function(dt, group_by = NULL, model = NULL, model_data = default(model_data), ...) {
  # this one should do the nesting, regression analyses all in once
  if (missing(model)) stop("no model supplied", call. = FALSE)
  model_quo <- enquo(model)
  model_data_quo <- enquo(model_data)
  nest_data(dt, group_by = !!enquo(group_by), nested_data = !!model_data_quo) %>%
    run_regression(model = !!model_quo, model_data = !!model_data_quo, ...)
}

# invert regression =====

#' invert the regression for calibration purposes
invert_regression <- function() {
  #note I think the residual sum of squares is the same s the deviance
  stop("see inversion_testing.Rmd for code")
}
