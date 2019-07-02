# examples =======

#' Example files
#'
#' @description The isprocessor package comes with a few example data sets to make it easy to illustrate the functionality.
#'
#' @details \code{iso_get_processor_example}: retrieve the path to an isoprocessor example data set
#' @param filename the name of the example file for which to retrieve the system path
#' @export
iso_get_processor_example <- function(filename) {
  filepath <- system.file(package = "isoprocessor", "extdata", filename)
  if(!file.exists(filepath))
    sprintf("The example file '%s' does not exist. Please use iso_get_reader_examples() to see a list of all available example files.", filename) %>%
    stop(call. = FALSE)
  return(filepath)
}

#' @rdname iso_get_processor_example
#' @details \code{iso_get_processor_examples}: list of all available isoprocessor example data set
#' @export
iso_get_processor_examples <- function() {
  # global vars

  extension <- filename <- format <- NULL
  file_types <- isoreader::iso_get_supported_file_types()
  iso_expand_paths(
    ".", extensions = file_types$extension, root = system.file(package = "isoprocessor", "extdata")) %>%
    mutate(filename = basename(path)) %>%
    isoreader:::match_to_supported_file_types(file_types) %>%
    arrange(type, extension, filename) %>%
    select(filename, type, description)
}

# general helper functions ========

#' @export
magrittr::`%>%`

# check if a column is in a data frame (copied from isoreader)
col_in_df <- function(df, col) {
  stopifnot(is.data.frame(df))
  col %in% names(df)
}

# just because it always is very confusing error if the base `filter` is used instead
#' @export
dplyr::filter

# because it is super helpful for dealing with the isoprocessor nested data types
#' @export
tidyr::unnest

# collapse helper to deal with naming change in the glue package
collapse <- function(...) {
  if (exists("glue_collapse", where=asNamespace("glue"), mode="function"))
    glue::glue_collapse(...)
  else
    glue::collapse(...)
}

# information display ====

#' Print data table
#'
#' This function is DEPRECATED and will be removed in a future version.
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

#' Alias for summarize data table
#' @param ... deprecated
#' @export
iso_generate_summary_table <- function(...) {
  warning("this function was renamed --> calling iso_summarize_data_table() instead",
          immediate. = TRUE, call. = FALSE)
  iso_summarize_data_table(...)
}

# FIXME: write units tests
#' Summarize a data table
#'
#' Convenience function to summarize means and standard deviations for one or multiple data columns. Use \link[dplyr]{group_by} prior to calling this function to generate the data table for individual subsets of the data. The generated data table always includes an \code{n} column with the number of records per group. If no column(s) are specified, will automatically summarize all columns except for the grouping columns.
#'
#' @param dt data table, can already have a group_by if so desired
#' @param ... which data columns to include in data overview. All \link[dplyr]{select} style syntax is supported (including on the fly renaming). If no columns are specified, will summarize all numeric columns (excluding any grouping columns).
#' @param cutoff the minimum number of records per group in order to include the group
#' @export
iso_summarize_data_table <- function(dt, ..., cutoff = 1) {

  # get column selectors
  dots <- quos(...)
  if (length(dots) == 0) dots <- quos(everything())

  # find columns
  grp_vars <- dplyr::group_vars(dt) %>% { setNames(., .) }
  vars <- tidyselect::vars_select(names(dt), !!!dots) %>%
    # exclude grouping variables
    { .[!. %in% grp_vars] } %>%
    # only numeric
    { .[purrr::map_lgl(., ~is.numeric(dt[[.x]]))] }

  # safety check
  if (length(vars) == 0)
    stop("no data columns provided, please select at least 1", call. = FALSE)

  # generate mutate quos
  summarize_funcs <-
    tibble(var = names(vars)) %>% tidyr::crossing(tibble(func = c("mean", "sd"))) %>%
    dplyr::mutate(name = paste(var, func)) %>%
    with(purrr::map2(var, func, ~quo((!!.y)(!!sym(.x), na.rm = TRUE))) %>% setNames(name))

  # generate summary
  dt %>%
    dplyr::select(!!!c(grp_vars, vars)) %>%
    dplyr::summarize(n = dplyr::n(), !!!summarize_funcs) %>%
    dplyr::filter(n >= cutoff)
}


# nesting ======

# @note: consider deprecating and replacing with direct nest command (nest now supports the dplyr select semantics just as well)
# nest data based on the grouping
# this is basically just an inverse version (in terms of selection) of \link[tidyr]{nest} that can handle the sophisticated selection parameters of select
# @param group_by what to keep out of the nested data (i.e. what to group by), by default nests everything
nest_data <- function(dt, group_by = NULL, nested_data = nested_data) {

  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), group_by = enquo(group_by), n_reqs = list(group_by = "*"))
  nested_col <- resolve_defaults(enquo(nested_data))

  # perform the nest
  dt %>%
    as_tibble() %>% # nest requires tbl
    nest(!!!map(dt_cols$group_by, ~quo(-!!sym(.x))), .key = !!nested_col)
}

#' Remove nested data
#'
#' Convenience function to remove nested columns in the data table (e.g. in preparation for printing to console or RMarkdown).
#' @inheritParams iso_prepare_for_calibration
#' @note not unit tested
#' @export
iso_remove_list_columns <- function(dt) {

  if (missing(dt)) stop("no data table supplied", call. = FALSE)

  list_cols <- dt %>% map_lgl(is_list)
  dt[!list_cols]
}

# note - documented but not exported
#' unnest parts of a data frame without loosing the rest
#'
#' note that this will lead to row duplication if the unnested variables have multiple entries per row of the \code{dt} data frame
#' also note that this will remove rows that have NULL in the nested_data column
#' @param select which columns to unnest - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default (i.e. unnests an entire nested data frame).
#' @param nested_data which column to unnest the \code{select} from
#' @param keep_remaining_nested_data whether to keep any remaining parts of the partially unnested data (irrelevant if \code{select = everything()})
#' @param keep_other_list_data keep other list data columns (e.g. other data or model columns)
unnest_select_data <- function(dt, select = everything(), nested_data = nested_data, keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE, keep_only_unique = TRUE) {
  # safety checks and column matching
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), nested_data = enquo(nested_data), type_reqs = list(nested_data = "list"))

  # add row number and remove NULL columns
  dt <-
    dt %>%
    filter(!map_lgl(!!sym(dt_cols$nested_data), is.null)) %>%
    mutate(..row.. = row_number()) %>%
    as_tibble()

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
    renested_dt <- unnested_dt %>% nest_data(group_by = !!keep_cols, nested = !!sym(dt_cols$nested_data))
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
    # rename the select columns
    dplyr::rename(!!!select_cols$select) %>%
    # remove the ..row.. again (just used for ID purposes)
    dplyr::select(-..row..)
}

# note - documented but not exported
#' Unnest model results column
#'
#' Convenience functions for unnesting model columns (supports both if regression is stored nested or unnested - see \link{run_regression} for details).
#'
#' @param model_column name of the model column to unnest
#' @param nested_model whether the model is nested, if TRUE, must also provide \code{model_params}
#' @param model_params name of the model params column that holds all the other model columns (if \code{nested_model = TRUE})
#' @inheritParams unnest_select_data
unnest_model_column <- function(dt, model_column, model_params = model_params, nested_model = FALSE,
                                 select = everything(),
                                 keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {

  # safety checks
  if (missing(model_column)) stop("specify which model column to unnest", call. = FALSE)
  dt_quo <- enquo(dt)

  # deal with nested senarios
  if (nested_model) {
    dt_cols <- get_column_names(!!dt_quo, model_params = enquo(model_params), type_reqs = list(model_params = "list"))

    # unnest model params
    original_cols <- names(dt)
    dt <- unnest(dt, !!sym(dt_cols$model_params), .drop = FALSE)
    model_cols <- setdiff(names(dt), original_cols)
  }

  # unnest model column
  dt <- unnest_select_data(dt, select = !!enquo(select), nested_data = !!enquo(model_column),
                     keep_remaining_nested_data = keep_remaining_nested_data,
                     keep_other_list_data = keep_other_list_data)

  # deal with nested scenarios
  if (nested_model && keep_other_list_data) {
    # rennest model params
    model_cols <- model_cols[model_cols %in% names(dt)]
    dt <- dt %>% mutate(..row_id.. = row_number())
    dt <-
      inner_join(
        dt %>% dplyr::select(!!!map(model_cols, ~quo(-!!sym(.x)))),
        dt %>% dplyr::select(..row_id.., !!!map(model_cols, sym)) %>% nest(-..row_id.., .key = !!dt_cols$model_params),
        by = "..row_id.."
      ) %>%
      dplyr::select(-..row_id..)
  }

  return(dt)
}

# regressions =====

# note - documented but not exported
#' run a set of regressions
#'
#' @param dt data table
#' @param model the regression model or named list of regression models. If a named list is provided, the name(s) will be stored in the \code{model_name} column instead of the formula.
#' @param nest_model whether to nest the model outcome columns (for easier use in multi model systems), default is FALSE
#' @param min_n_datapoints the minimum number of data points required for applying the model(s). Note that there is always an additional check to make sure the minimum number of degrees of freedom for each model is met. If the minimum number of degrees of freedom required is not met, the model will/can not be calculated no matter what \code{min_n_datapoints} is set to.
#' @param model_data the nested model data column
#' @param model_filter_condition a filter to apply to the data before running the regression (if only a subset of the data is part of the calibration data) - stored in \code{in_reg}, by default no filter
#' @param model_name new column with the model formulae or names if supplied
#' @param model_enough_data new column with information on whether the model has enough data (based on the required degrees of freedom for the model)
#' @param model_fit the new model objects column
#' @param model_coefs the new model coefficients nested data frame column
#' @param model_summary the new model summary nested data frame column
#' @param model_params the nested model information (only relevant if \code{nest_model = TRUE})
#' @param in_reg name of the new logical column in the nested model_data that flags the data included in the regression (i.e. the data that fullfills the \code{model_filter_condition} if provided)
#' @param residual name of the new residual column in the nested model_data - residuals are only calculated for rows that are part of the regression (as determined by \code{model_filter_condition})
run_regression <- function(dt, model, nest_model = FALSE, min_n_datapoints = 1,
                           model_data = model_data, model_filter_condition = NULL,
                           model_name = model_name, model_enough_data = model_enough_data,
                           model_fit = model_fit, model_coefs = model_coefs,
                           model_summary = model_summary, model_params = model_params,
                           in_reg = in_reg, residual = residual) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_quo <- enquo(dt)
  dt_cols <- get_column_names(!!dt_quo, model_data = enquo(model_data), type_reqs = list(model_data = "list"))
  dt_new_cols <- get_new_column_names(
    model_name = enquo(model_name), model_enough_data = enquo(model_enough_data),
    model_fit = enquo(model_fit), model_coefs = enquo(model_coefs), model_summary = enquo(model_summary),
    model_params = enquo(model_params),
    in_reg = enquo(in_reg), residual = enquo(residual))
  filter_quo <- enquo(model_filter_condition) %>% { if(quo_is_null(.)) quo(TRUE) else . }

  # models
  if (missing(model)) stop("no regression model supplied", call. = FALSE)
  model_quos <- enquo(model)
  # resolve list of models
  if (quo_is_call(model_quos) && rlang::call_name(model_quos) %in% c("c", "list")) {
    lquos <- quos(!!!rlang::call_args(model_quos))
  } else {
    lquos <- quos(!!model_quos)
  }

  # safety checks on models
  if (length(lquos) == 0) stop("no regression model supplied", call. = FALSE)
  # @NOTE: loess and nls not currently supported because would have to find a way to make inversion work
  supported_models <- c("lm", "glm", "lme")
  lquos_are_models <- map_lgl(lquos, function(lq) quo_is_call(lq) && rlang::call_name(lq) %in% supported_models)
  if(!all(ok <- lquos_are_models)) {
    params <-
      str_c(names(lquos)[!ok] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
            map_chr(lquos[!ok], quo_text)) %>%
      collapse("', '", last = "' and '")
    if (sum(!ok) > 1)
      glue("'{params}' do not refer to valid models ",
           "({collapse(supported_models, sep = ', ', last = ' or ')})") %>% stop(call. = FALSE)
    else
      glue("'{params}' does not refer to a valid model ",
           "({collapse(supported_models, sep = ', ', last = ' or ')})") %>% stop(call. = FALSE)
  }

  # models data frame
  models <-
    tibble(
      model_formula = map_chr(lquos, quo_text),
      !!dt_new_cols$model_name := ifelse(nchar(names(lquos)) > 0, names(lquos), model_formula),
      model_quo = lquos
    ) %>%
    # don't keep separate formula column
    select(-model_formula)

  # check for duplicate model names/formulae
  if (any(dups <- duplicated(models[[dt_new_cols$model_name]]))){
    dup_names <- models[[dt_new_cols$model_name]][dups]
    glue("regressions with multiple models require unique model formulae or names (if specified), encountered duplicates: '{collapse(dup_names, \"', '\")}'") %>%
      stop(call. = FALSE)
  }

  # combination of data and model
  data_w_models <-
    eval_tidy(dt_quo) %>%
    mutate(..group_id.. = row_number()) %>% # for easier sorting
    tidyr::crossing(models) %>%
    # evaluation of model
    mutate(
      # check if there is any data
      !!dt_new_cols$model_enough_data := map_lgl(!!sym(dt_cols$model_data), ~{
        data <- .x
        check_expressions(data, filter_quo)
        nrow(filter(data, !!filter_quo)) >= min_n_datapoints
      }),
      # fit the model if there is any data
      !!dt_new_cols$model_fit :=
        pmap(list(m = model_quo, d = !!sym(dt_cols$model_data), run = !!sym(dt_new_cols$model_enough_data)),
             function(m, d, run) if (run) eval_tidy(m, data = filter(d, !!filter_quo)) else NULL),
      # figure out which fits actually have enough degrees of freedom
      !!dt_new_cols$model_enough_data :=
        map2_lgl(!!sym(dt_new_cols$model_fit), !!sym(dt_new_cols$model_enough_data),
                 ~if (.y) {
                   !any(coef(.x) %>% as.list() %>% map_lgl(is.na))
                  } else FALSE),
      # get the coefficients
      !!dt_new_cols$model_coefs := map2(
        !!sym(dt_new_cols$model_fit), !!sym(dt_new_cols$model_enough_data),
        ~if (.y) {
          mutate(as_tibble(tidy(.x)),
                 # add in significant level summary
                 signif = case_when(
                   p.value < 0.001 ~ "***",
                   p.value < 0.01 ~ "**",
                   p.value < 0.05 ~ "*",
                   p.value < 0.1 ~ ".",
                   TRUE ~ "-")
          )
        } else NULL),
      # get the summary
      !!dt_new_cols$model_summary :=
        map2(!!sym(dt_new_cols$model_fit), !!sym(dt_new_cols$model_enough_data),
             ~if (.y) { as_tibble(glance(.x)) } else { NULL })
    )

  # warnings
  if ((not_enough <- sum(!data_w_models[[dt_new_cols$model_enough_data]])) > 0)
    glue("{not_enough} of {nrow(data_w_models)} regressions have insufficient ",
         "degrees of freedom (not enough data given the regression models and/or ",
         "requested minimum number of data points) - please double check that all",
         "peaks are mapped correctly (see ?iso_get_problematic_peak_mappings and ",
         "?iso_summarize_peak_mappings)") %>%
    warning(immediate. = TRUE, call. = FALSE)

  # add residuals
  data_w_models <-
    data_w_models %>%
    mutate(
      !!dt_cols$model_data :=
        pmap(
          list(d = !!sym(dt_cols$model_data), fit = !!sym(dt_new_cols$model_fit), run = !!sym(dt_new_cols$model_enough_data)),
          function(d, fit, run) {
            d[[dt_new_cols$in_reg]] <- FALSE
            d[[dt_new_cols$residual]] <- NA_real_
            if (run) {
              # find which ones are in reg
              d[[dt_new_cols$in_reg]] <- rlang::eval_tidy(filter_quo, data = d)
              # add residuals
              d[d[[dt_new_cols$in_reg]], dt_new_cols$residual] <- residuals(fit)
            }
            return(d)
          })
    )

  # nest model
  if (nest_model) {
    # generated row id for unique matching id
    data_w_models <- data_w_models %>% mutate(..row_id.. = row_number())
    model_cols <- c(dt_new_cols$model_fit, dt_new_cols$model_coefs, dt_new_cols$model_summary)
    data_w_models <-
      inner_join(
        data_w_models %>% select(!!!map(model_cols, ~quo(-!!sym(.x)))),
        data_w_models %>% select(..row_id.., !!!map(model_cols, sym)) %>% nest(-..row_id.., .key = !!dt_new_cols$model_params),
        by = "..row_id.."
      ) %>%
      select(-..row_id..)
  }

  data_w_models %>%
    arrange(..group_id..) %>%
    select(-..group_id.., -model_quo)
}

# run regressions in grouped blocks (uses nest_data and run_regression)
# @note is this really used or should it be deprecated?
# @probably deprecate
run_grouped_regression <- function(dt, group_by = NULL, model = NULL, model_data = default(model_data), ...) {
  # this one should do the nesting, regression analyses all in once
  if (missing(model)) stop("no model supplied", call. = FALSE)
  model_quo <- enquo(model)
  model_data_quo <- enquo(model_data)
  nest_data(dt, group_by = !!enquo(group_by), nested_data = !!model_data_quo) %>%
    run_regression(model = !!model_quo, model_data = !!model_data_quo, ...)
}

# apply regression =====

# note - documented but not exported
# @FIXME: fix documentation, the inheritance from run_regression is not quite right
# @NOTE: model_name is only included for error reporting purposes - is that silly?
#' invert a regression for calibration purposes
#'
#' note: this is optimized for inverting regressions, but should also do prediction for regular regression at some point
#'
#' @param dt data table with calibrations
#' @param predict which value to calculate, must be one of the regression's independent variables
#' @param calculate_error whether to estimate the standard error from the calibration (using the Wald method), stores the result in the new \code{predict_error} column. Note that error calculation slows this function down a fair bit and is therefore disabled by default.
#' @inheritParams run_regression
#' @inheritParams unnest_model_column
#' @param predict_value the new column in the model_data that holds the predicted value
#' @param predict_error the new column in the model_data that holds the error of the predicted value (only created if \code{calculate_error = TRUE})
#' @param predict_range vector of 2 numbers, if provided will be used for finding the solution for the predict variable. By default uses the range observed in the calibration variables. Specifying the \code{predict_range} is usually only necessary if the calibration range should be extrapolated significantely.
apply_regression <- function(dt, predict, nested_model = FALSE, calculate_error = FALSE,
                             model_data = model_data, model_name = model_name,
                             model_fit = model_fit, model_params = model_params,
                             predict_value = pred, predict_error = pred_se,
                             predict_range = NULL) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_quo <- enquo(dt)

  if (nested_model) {
    # nested model
    dt_cols <- get_column_names(
      !!dt_quo, model_name = enquo(model_name), model_data = enquo(model_data),
      model_params = enquo(model_params),
      type_reqs = list(model_name = "character", model_data = "list", model_params = "list"))

    # check for columns inside nested data
    dt_cols <- c(
      dt_cols,
      get_column_names(unnest(!!dt_quo, !!sym(dt_cols$model_params)),
                       model_fit = enquo(model_fit), type_reqs = list(model_fit = "list")))

    # pull out the model fit
    dt <- dt %>%
      mutate(
        !!dt_cols$model_fit := map(!!sym(dt_cols$model_params), ~.x[[dt_cols$model_fit]])
      ) %>%
      unnest(!!sym(dt_cols$model_fit), .drop = FALSE)
  } else {
    # not nested model
    dt_cols <- get_column_names(
      !!dt_quo, model_data = enquo(model_data),
      model_name = enquo(model_name), model_fit = enquo(model_fit),
      type_reqs = list(model_name = "character", model_data = "list", model_fit = "list"))
  }

  # new cols (+predict)
  dt_new_cols <- get_new_column_names(
    predict = enquo(predict), predict_value = enquo(predict_value),
    predict_error = enquo(predict_error))

  # check that predict variable is part of all regressions
  dt <-
    dt %>%
    mutate(
      .predict_vars = map(
        !!sym(dt_cols$model_fit),
        ~tibble(var = all.vars(.x$terms), dependent = var %in% all.vars(.x$terms[[2]]))
      ),
      # only allowing inverting regression for independent variables
      # @FIXME: consider doing a two way split here and allowing regular predict on the dependent variable!
      # note: consider scenario though where dependent variable is e.g. log(y) or similar - possible to deal with that?
      .predict_x = dt_new_cols$predict,
      .predict_info = map(.predict_vars, ~filter(.x, var == dt_new_cols$predict, !dependent)),
      .predict_ok = map_lgl(.predict_info, ~nrow(.x) == 1),
      .predict_y = map(.predict_vars, ~filter(.x, dependent)$var),
      .predict_y_ok = map_lgl(.predict_y, ~length(.x) == 1),
      .predict_other_xs = map(.predict_vars, ~filter(.x, var != dt_new_cols$predict, !dependent)$var)
    )

  # safety checks
  if (!all(dt$.predict_ok)) {
    glue("cannot apply regression - variable '{dt_new_cols$predict}' is not a regressor in the following model(s): ",
         "{collapse(filter(dt, !.predict_ok)[[dt_cols$model_name]] %>% unique(), sep = ', ')}") %>%
      stop(call. = FALSE)
  }
  if (!all(dt$.predict_y_ok)) {
    glue("cannot apply regression - multiple dependent (y) variables are not supported, problematic model(s): ",
         "{collapse(filter(dt, !.predict_y_ok)[[dt_cols$model_name]] %>% unique(), sep = ', ')}") %>%
      stop(call. = FALSE)
  }

  # inversion functions
  safe_invest <- safely(invest)
  invest_interval_method <- if (calculate_error) "Wald" else "none"

  # apply inversion
  dt <-
    dt %>%
    mutate(
      !!dt_cols$model_data :=
        pmap(
          list(d = !!sym(dt_cols$model_data), fit = !!sym(dt_cols$model_fit), name = !!sym(dt_cols$model_name),
               x = .predict_x, y = .predict_y, other_xs = .predict_other_xs),

          # process data set for a single model
          function(d, fit, name, x, y, other_xs) {

            # range
            if(!is.null(predict_range)) {
              # parameter provided predict_range
              range <- predict_range
            } else {
              # estimate predict range based on available values
              # NOTE: should this be only values that were used for calib? (i.e. have residual column set)
              range <- base::range(d[[x]], na.rm = TRUE)
            }
            range_tolerance_escalation <- c(0, 1, 10, 100, 1000)

            # check for enough data (standard eval to avoid quoting trouble)
            d_enough_data <- rowSums(is.na(d[c(y, other_xs)]) * 1) == 0

            # data with rows
            d <- d %>%
              mutate(
                ..enough_data.. = d_enough_data,
                ..rn.. = 1:length(d_enough_data)
              )

            # do the calculate for each row
            d_prediction <- d %>%
              # NOTE: use group by and do to get a more informative progress bar in interactive use
              group_by(..rn..) %>%
              do({
                # values
                estimate <- NA_real_
                se <- NA_real_
                problem <- NA_character_

                # check whether have enough data
                if (.$..enough_data..) {

                  # cycle through range tolerance
                  for(range_tolerance in range_tolerance_escalation) {
                    # try to find fit
                    out <- safe_invest(
                      fit, y0 = .[[y]], x0.name = x, data = ., newdata = .[other_xs],
                      interval = invest_interval_method,
                      lower = range[1] - range_tolerance * diff(range),
                      upper = range[2] + range_tolerance * diff(range),
                      # NOTE: the following doesn't really do anything, using the tolerance ranges instead
                      extendInt = "yes"
                    )

                    # break as soon as success or a different error
                    if (is.null(out$error) || !str_detect(out$error$message, "not found in.*search interval")) {
                      break
                    }
                  }

                  # process outcome
                  if (is.null(out$error) && calculate_error) {
                    # with error estimates
                    estimate <- out$result$estimate
                    se <- out$result$se
                  } else if (is.null(out$error)) {
                    # no error estimates
                    estimate <- out$result
                  } else if (str_detect(out$error$message, "not found in the search interval")) {
                    problem <- glue(
                      "No solution for '{x}' in the interval {range[1] - range_tolerance * diff(range)} ",
                      "to {range[2] + range_tolerance * diff(range)}, potential fit is too far outside ",
                      "the calibration range",
                      "- consider manually adjusting parameter 'predict_range'.") %>%
                      as.character()
                  } else {
                    problem <- out$error$message
                  }
                } else {
                  # not enough data
                  problem <- glue("Not enough data, missing a value for at least one of the variables: ",
                                  "'{collapse(c(y, other_xs), sep = \"', '\", last = \"' or '\")}'") %>%
                    as.character()
                }

                # return data
                tibble(
                  !!dt_new_cols$predict_value := estimate,
                  !!dt_new_cols$predict_error := se,
                  ..problem.. = problem
                )
              })

            # warnings about problematic sets
            if (nrow(d_problematic <- filter(d_prediction, !is.na(..problem..))) > 0) {
              problems <- d_problematic %>%
                group_by(..problem..) %>%
                summarize(n = length(..rn..), rows = collapse(..rn.., sep = ", ", last = " and ")) %>%
                mutate(message = glue("{n} data rows ({rows}) failed with the following error/warning: {..problem..}"))
              glue("failed to calculate '{x}' with regression model '{name}' for {nrow( d_problematic)}/{nrow(d_prediction)} data entries:\n",
                   " - {collapse(problems$message, sep = '\n - ')}") %>%
                warning(immediate. = TRUE, call. = FALSE)

            }

            # remove error estimate column if not calculated
            if (!calculate_error) {
              d_prediction <- d_prediction %>% select(-!!sym(dt_new_cols$predict_error))
            }

            # return combined
            left_join(d, d_prediction, by = "..rn..") %>%
              select(-..rn.., -..enough_data.., -..problem..)
          })
    )

  # cleanup
  dt <- dt %>%
    # remove helper columns
    select(-starts_with(".predict"))

  # remove unnested columns if in nested mode
  if (nested_model) {
    dt <- dt %>% select(-!!sym(dt_cols$model_fit))
  }

  return(dt)
}

# regression range =====

#' evaluate regression ranges
#' @inheritParams run_regression
#' @inheritParams apply_regression
#' @param ... which terms to evaluate the range for. Can be individual columns or more complex numeric expressions. All must be valid within the scope of the model_data and only numeric values are currently evaluated.
#' @param model_range new column recording the range of all terms for each model and model_data combination.
#' @param in_range new column in the model_data that holds whether a data entry is within the model_range. Checks whether all terms are within the model_range and records a textual summary of the result in this new column.
evaluate_range <- function(
  dt, ..., nested_model = FALSE, model_data = model_data, model_params = model_params,
  in_reg = in_reg, model_range = model_range, in_range = in_range) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_quo <- enquo(dt)

  # terms
  terms_quos <- rlang::enquos(...)
  if (length(terms_quos) == 0) {
    stop("no terms for range evaluation are provided, please specify at least one term", call. = FALSE)
  }

  # dt columns
  dt_cols <- get_column_names(
    !!dt_quo, model_data = enquo(model_data), type_reqs = list(model_data = "list"))

  # nested model
  if (nested_model) {
    dt_cols <- c(
      dt_cols,
      get_column_names(
        !!dt_quo, model_params = enquo(model_params),type_reqs = list(model_params = "list"))
    )
  }

  # terms tibble
  terms <-
    tibble(
      term = map_chr(terms_quos, rlang::as_label),
      q = terms_quos
    )

  # data columns
  in_reg_quo <- enquo(in_reg)
  dt <- dt %>%
    mutate(
      ..data_cols.. = map(!!sym(dt_cols$model_data), ~
          {
            model_data <- .x
            get_column_names(model_data, in_reg = in_reg_quo,
                             type_reqs = list(in_reg = "logical"))
          })
    )

  # new columns
  dt_new_cols <- get_new_column_names(model_range = enquo(model_range), in_range = enquo(in_range))

  # find range
  dt_w_ranges <-
    dt %>%
    mutate(
      !!dt_new_cols$model_range := purrr::map2(
        !!sym(dt_cols$model_data),
        ..data_cols..,
        ~{
          # consider only data that is part of the regression
          d_in_calib <- .x[.x[[.y$in_reg]],]
          if (nrow(d_in_calib) > 0) {
            # determine the ranges for all terms
            tryCatch(
              terms_ranges <-
                terms %>%
                mutate(
                  values = map(q, ~rlang::eval_tidy(.x, data = d_in_calib)),
                  min = map_dbl(values, ~.x %>%
                                  { if(is.numeric(.)) { as.numeric(min(., na.rm = TRUE)) } else { NA_real_} }),
                  max = map_dbl(values, ~.x %>%
                                  { if(is.numeric(.)) { as.numeric(max(., na.rm = TRUE)) } else { NA_real_} })
                ) %>%
                select(-q, -values),
              error = function(e) {
                glue::glue(
                  "not all range evaluation terms are valid expressions in the context of the ",
                  "model data: {e$message}") %>%
                  stop(call. = FALSE)
              }
            )
            return(terms_ranges)
          } else {
            return(mutate(terms, min = NA_real_, max = NA_real_) %>% select(-q))
          }
        }
      )
    )

  # evaluate range
  dt_w_ranges <- dt_w_ranges %>%
    mutate(
      !!dt_cols$model_data :=
        map2(!!sym(dt_cols$model_data), !!sym(dt_new_cols$model_range), ~{
          model_data <- mutate(.x, ..data_id.. = row_number())

          # evaluate ranges
          terms_evals <- terms %>%
            # find data values for each term
            mutate(..values.. = map(q, ~
              mutate(
                model_data,
                value = rlang::eval_tidy(.x, data = model_data) %>% as.numeric()) %>%
                select(..data_id.., value)
              )
            ) %>%
            # remove quos
            select(-q) %>%
            # figure out if it's within our outside range
            unnest(..values..) %>%
            left_join(.y, by = "term") %>%
            mutate(
              in_range = case_when(
                is.na(min) | is.na(max) ~ sprintf("'%s' range NA", term),
                is.na(value) ~ sprintf("'%s' NA", term),
                value < min ~ sprintf("<'%s' range", term),
                value > max ~ sprintf(">'%s' range", term),
                TRUE ~ "OK"
              )
            ) %>%
            group_by(..data_id..) %>%
            summarize(
              !!dt_new_cols$in_range :=
                if (all(in_range == "OK")) "in range"
                else paste(unique(in_range[in_range != "OK"]), collapse = ", ")
            )

          # add in the range evaluation
          model_data %>%
            {
              if (dt_new_cols$in_range %in% names(.)) select(., -!!sym(dt_new_cols$in_range))
              else .
            } %>%
            left_join(select(terms_evals, ..data_id.., !!sym(dt_new_cols$in_range)), by = "..data_id..") %>%
            select(-..data_id..)
        })
    )

  # remove unnested columns if in nested mode
  if (nested_model) {
    dt_w_ranges <- dt_w_ranges %>%
      # nest the new model range column into the nested columns
      mutate(!!dt_cols$model_params := map2(!!sym(dt_cols$model_params), !!sym(dt_new_cols$model_range),
                                            ~{.x[[dt_new_cols$model_range]] <- list(.y); .x})) %>%
      # remove the not nested columns
      select(-!!sym(dt_new_cols$model_range))
  }

  return(dt_w_ranges %>% select(-..data_cols..))
}

