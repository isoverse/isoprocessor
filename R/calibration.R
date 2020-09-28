# STANDARDS ----

#' Add calibration standards
#'
#' Convenience function to add calibration standards to a continuous flow peak table (standalone or inside an isofiles object).
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @export
iso_add_standards <- function(...) {
  UseMethod("iso_add_standards")
}

#' @export
iso_add_standards.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_add_standards.iso_file <- function(iso_files, ...) {
  iso_add_standards(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_add_standards
#' @inheritParams iso_show_default_processor_parameters
#' @param iso_files collection of continuous flow iso_file objects
#' @param stds standards data frame
#' @param match_by what column(s) to match the standards by
#' @param is_std_peak new column that holds information about which ones are standard peaks (i.e. have known isotopic values)
#' @return iso files with standards data frame merged into the peak table and the following information column added:
#' \itemize{
#' \item{\code{is_std_peak}: }{a logical TRUE/FALSE indicating which data table entry is a standard}
#' }
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_add_standards.iso_file_list <- function(
  iso_files, stds, match_by = default(std_match_by), is_std_peak = default(is_std_peak), quiet = default(quiet)) {

  # continuous flow file check
  if (!isoreader::iso_is_continuous_flow(iso_files))
    stop("peak tables can only exist in continuous flow files", call. = FALSE)

  # get peak table
  peak_table <- iso_get_peak_table(iso_files, quiet = TRUE)
  if (nrow(peak_table) == 0) return(iso_files)

  # see if match_by column comes from file info
  match_by_quo <- resolve_defaults(enquo(match_by))
  file_info <- iso_get_file_info(iso_files, quiet = TRUE)
  match_by_cols <- get_column_names(file_info, match_by = match_by_quo, n_reqs = list(match_by = "*"), cols_must_exist = FALSE, warn = FALSE)$match_by
  if (length(match_by_cols) > 0) {
    match_by_cols <- stringr::str_subset(match_by_cols, fixed("file_id"), negate = TRUE)
    peak_table <- dplyr::left_join(dplyr::select(file_info, file_id, !!!match_by_cols), peak_table, by = "file_id")
  }

  # add standards
  peak_table_with_standards <-
    peak_table %>%
    iso_add_standards(
      stds = stds,
      match_by = !!match_by_quo,
      is_std_peak = !!enquo(is_std_peak),
      quiet = quiet
    )

  # remove extra file info columns again
  if (length(match_by_cols) > 0) {
    peak_table_with_standards <- dplyr::select(peak_table_with_standards, !!!map(match_by_cols, ~quo(-!!sym(.x))))
  }

  # assign peak table (note: go for direct assigment even if it generates some
  # NAs in columns that differ between iso_files, at this point if files are
  # processed together they should have similar enough peak tables and it's too
  # risky potentially missing an updated column)
  return(iso_set_peak_table(iso_files, peak_table_with_standards, quiet = TRUE))

}

#' @rdname iso_add_standards
#' @param dt data frame with the peak table
#' @param is_standard renamed to \code{is_std_peak} because the naming caused too much confusion, will be removed in future versions, please use \code{is_std_peak} instead
#' @family calibration functions
#' @export
iso_add_standards.data.frame <- function(dt, stds, match_by = default(std_match_by), is_std_peak = default(is_std_peak), quiet = default(quiet), is_standard = NULL) {

  # make sure params supplied
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(stds)) stop("no standards table supplied", call. = FALSE)
  if (missing(match_by)) stop("match_by parameter must be supplied", call. = FALSE)
  if (!missing(is_standard)) {
    stop("the parameter is_standard was renamed to is_std_peak to avoid confusion, please use is_std_peak instead", call. = FALSE)
  }

  # column names allowing standard and NSE
  match_by_quo <- enquo(match_by)
  dt_cols <- get_column_names(dt, match_by = match_by_quo, n_reqs = list(match_by = "+"))
  stds_cols <- get_column_names(stds, match_by = match_by_quo, n_reqs = list(match_by = "+"))
  new_cols <- get_new_column_names(is_std_peak = enquo(is_std_peak))

  # select standards
  stds <- mutate(stds, ..marker.. = TRUE)
  dt_w_stds <- dt %>%
    left_join(stds, by = dt_cols$match_by) %>%
    mutate(!!new_cols$is_std_peak := !is.na(..marker..)) %>%
    select(-..marker..) %>%
    # arrange properly
    select(dt_cols$match_by, new_cols$is_std_peak, everything())

  if (!quiet) {
    n_stds <- filter(dt_w_stds, !!sym(new_cols$is_std_peak))[dt_cols$match_by] %>% unique() %>% nrow()
    n_stds_rows <- filter(dt_w_stds, !!sym(new_cols$is_std_peak)) %>% nrow()
    glue("Info: matching standards by '{collapse(dt_cols$match_by, sep = \"', '\", last = \"' and '\")}' - ",
         "added {n_stds} standard entries to {n_stds_rows} out of {nrow(dt_w_stds)} rows, ",
         "added new column '{new_cols$is_std_peak}' to identify standard peaks") %>%
      message()
  }
  return(dt_w_stds)
}

# PREPARING -----

#' Prepare data set for calibration
#'
#' Nests data set in preparation for calibration calculations. Use the \code{group_by} parameter to group analyses as appropriate for calibration. Use \link{iso_get_calibration_data} to easily pull out columns from the nested data frame in \code{all_data} at any point before, during or after calibration.
#'
#' @param dt data table
#' @param group_by what to group by for indidual calibration calculations (use c(...) to select multiple) - set \code{group_by = NULL} to avoid grouping
#' @param nest_existing_calibrations whether to nest existing calibrations. This is usually not necessary and only leads to larger data objects. By default, existing calibrations are therefore never included in the nested data set.
#' @inheritParams iso_show_default_processor_parameters
#' @return a nested data set with the \code{group_by} columns out front and the remaining data in a new nested column named \code{all_data}
#' @family calibration functions
#' @export
iso_prepare_for_calibration <- function(dt, group_by = NULL, nest_existing_calibrations = FALSE, quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  group_quo <- enquo(group_by)
  nested_data_quo <- quo(all_data)
  dt_cols <- get_column_names(dt, group_by = group_quo, n_reqs = list(group_by = "*"))
  new_cols <- get_new_column_names(nested_data = nested_data_quo)
  if (new_cols$nested_data %in% names(dt))
    glue("'{new_cols$nested_data}' column already exists, cannot overwrite") %>% stop(call. = FALSE)

  if (!quiet) {
    if (length(dt_cols$group_by) > 0)
      glue("Info: preparing data for calibration by grouping based on ",
           "'{collapse(dt_cols$group_by, \"', '\", last = \"' and '\")}'") %>%
      message()
    else
      glue("Info: preparing data for calibration by nesting the entire dataset") %>%
      message()
  }

  # existing calibrations
  calib_columns <- c()
  if (!nest_existing_calibrations) {
    calib_columns <- get_existing_calibration_columns(dt, all_calibrations(dt))
  }

  # nested data
  nested_data <-
    nest_data(
      dt,
      group_by = c(!!!map(c(dt_cols$group_by, calib_columns), rlang::sym)),
      nested_data = !!nested_data_quo
    ) %>%
    select(dt_cols$group_by, !!nested_data_quo, everything())

  return(nested_data)
}

# CALIBRATION AUX -----

# convenience function for common calibration variables
get_calibration_vars <- function(calibration) {
  prefix_with_sep <- if (nchar(calibration) > 0) str_c(calibration, "_") else ""
  list(
    calib_name = if(calibration != "") as.character(glue("'{calibration}' ")) else "",
    model_name = str_c(prefix_with_sep, "calib"),
    model_enough_data = str_c(prefix_with_sep, "calib_ok"),
    model_data_points = str_c(prefix_with_sep, "calib_points"),
    model_params = str_c(prefix_with_sep, "calib_params"),
    residual = str_c(prefix_with_sep, "resid"),
    in_reg = str_c(prefix_with_sep, "in_calib"),
    in_range = str_c(prefix_with_sep, "in_range")
  )
}

# all calibraiton params columns
all_calibration_params <- function(df) {
  stringr::str_subset(names(df), "^.*calib_params$")
}

# convenience function to find calibrations (only works for nested ones, which is how iso_generate_calibration stores them)
all_calibrations <- function(df) {
  stringr::str_replace(all_calibration_params(df), "_?calib_params$", "")
}

# helper function to find the last calibration (same as all_calibrations, only for nested ones)
# based on the column position, throws an error if there are no calibrations
last_calibration <- function(df, check = TRUE) {
  all_calibs <- all_calibrations(df)
  if (check && length(all_calibs) == 0) {
    stop("could not find any calibration columns in this data frame. Please generate a calibration using ?iso_generate_calibration and make sure the '*calib_params' column is not removed from the data frame.", call. = FALSE)
  }
  return(tail(all_calibs, 1))
}

# convenience function for checking presence of calibration variables
# @param cols the parts of get_calibration_vars that should be checked
check_calibration_cols <- function(df, cols) {
  # df name and data frame test
  df_quo <- enquo(df)
  df_name <- rlang::as_label(df_quo)
  df <- rlang::eval_tidy(df_quo)
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)

  if (length(missing <- setdiff(unlist(cols), names(df))) > 0) {
    glue("'{collapse(missing, sep = \"', '\", last = \"' and '\")}' refer(s) to unknown column(s) in data frame '{df_name}' - make sure to run iso_generate_calibration() first and to use the same 'calibration' parameter value") %>%
      stop(call. = FALSE)
  }
}

# helper function to strip fit objects to make data frames smaller in size
# this makes iso_apply_calibration impossible to use but all other functions should be no problem
strip_regression_fits <- function(df) {
  for (calib_params in all_calibration_params(df)) {
    df <- mutate(
      df,
      !!sym(calib_params) := purrr::map(!!sym(calib_params), ~{
        .x$model_fit <- NULL
        .x
      }))
  }
  return(df)
}

# check if data frame still has regression fit for a calibration
has_model_range <- function(df, calibration = last_calibration(df)) {
  calib_params <- get_calibration_vars(calibration)$model_params
  all(purrr::map_lgl(df[[calib_params]], ~"model_range" %in% names(.x)))
}

# check if data frame still has regression fit for a calibration
has_regression_fit <- function(df, calibration = last_calibration(df)) {
  calib_params <- get_calibration_vars(calibration)$model_params
  all(purrr::map_lgl(df[[calib_params]], ~"model_fit" %in% names(.x)))
}

# convenience function to remove other calibration columns from the data frame
remove_other_calibrations <- function(dt, calibration) {
  all_calibs <- all_calibrations(dt)
  columns <- get_existing_calibration_columns(dt, all_calibs[all_calibs != calibration])
  if (length(columns) > 0)
    return(select(dt, !!!map(columns, ~quo(-!!.x))))
  else
    return(dt)
}

# get existing calibration columns
get_existing_calibration_columns <- function(dt, calibrations) {
  calibrations %>%
    map(~get_calibration_vars(.x)[c("model_name", "model_enough_data", "model_data_points", "model_params")]) %>%
    unlist() %>% unique() %>%
    intersect(names(dt))
}

# GENERATE CALIBRATION -----

#' Generate data calibration
#'
#' Generate a calibration for a specific variable based on one or multiple calibration models. Requires properly nested and grouped data, see \link{iso_prepare_for_calibration} for details. Note that to calibrate different variables, separate calls to this function should be issued each with different \code{calibration} names.
#'
#' @param dt nested data table with column \code{all_data} (see \link{iso_prepare_for_calibration})
#' @param model a single regression model (usually \link[stats]{lm} or \link[stats]{glm}) or a list of multiple alternative regression models for the calibration. If a named list is provided, the name(s) will be used instead of the formulas for the model identification column. If multiple models are provided, the entire data table rows will be duplicated to consider the different models in parallel. Note that \link[stats]{loess} models are supported but discouraged (and will cause a warning) because local polynomial regression fitting does not calibrate based on a hypothesized regression model and can easily mis-calibrate sparse data. The exception is for non-linear temporal drift corrections (use \code{calibration="drift"} to flag as such) which may reasonably require local polynomical regression fitting of the type \code{loess(y ~ file_datetime)} to account for temporal machine variations.
#' @param calibration an informative name for the calibration (could be e.g. \code{"d13C"} or \code{"conc"}). If provided, will be used as a prefix for the new columns generated by this function. This parameter is most useful if there are multiple variables in the data set that need to be calibrated (e.g. multiple delta values, concentration, etc.). If there is only a single variable to calibrate, the \code{calibration} parameter is completely optional and can just be left blank (the default).
#' @param use_in_calib column or filter condition to determine which subset of data to actually use for the calibration (default is the \code{is_std_peak} field introduced by \code{\link{iso_add_standards}}).
#' @param is_std_peak deprecated in favor of \code{use_in_calib}
#' @param is_standard deprecated in favor of \code{use_in_calib}
#' @inheritParams run_regression
#' @inheritParams iso_show_default_processor_parameters
#' @return the data table with the following columns added (prefixed by the \code{calibration} parameter if provided):
#' \itemize{
#'   \item{\code{calib}: }{the name of the calibration if provided in the \code{model} parameter, otherwise the formula}
#'   \item{\code{calib_ok}: }{a TRUE/FALSE column indicating whether there was enough data for calibration to be generated}
#'   \item{\code{calib_points}: }{an integer column indicating the total number of data points used in the calibration. Note that this field counts replicate data points: multiple data points that fall at the exact same x or y-value still count as individual data points for this metric.}
#'   \item{\code{calib_params}: }{a nested dataframe that holds the actual regression model fit, coefficients, summary and data range. These parameters are most easily accessed using the functions \code{\link{iso_unnest_calibration_coefs}}, \code{\link{iso_unnest_calibration_summary}}, \code{\link{iso_unnest_calibration_parameters}}, \code{\link{iso_unnest_calibration_range}}, or directly via \code{\link[tidyr]{unnest}}}
#'   \item{\code{resid} within \code{all_data}: }{a new column within the nested \code{all_data} that holds the residuals for all standards used in the regression model}
#' }
#' @family calibration functions
#' @export
iso_generate_calibration <- function(dt, model, calibration = "",
                                     use_in_calib = default(is_std_peak),
                                     min_n_datapoints = 2,
                                     is_std_peak = default(is_std_peak),
                                     is_standard = default(is_std_peak),
                                     quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(model)) stop("no calibration model(s) supplied", call. = FALSE)
  if (!missing(is_std_peak)) {
    warning("the parameter 'is_std_peak' was renamed to 'use_in_calib' to avoid confusion, please use 'use_in_calib' instead", immediate. = TRUE, call. = FALSE)
  }
  if (!missing(is_standard)) {
    warning("the parameter 'is_standard' was renamed to 'use_in_calib' to avoid confusion, please use 'use_in_calib' instead", immediate. = TRUE, call. = FALSE)
  }
  if (calibration %in% all_calibrations(dt)) {
    glue::glue(
      if(calibration == "") "this data frame already has an unnamed calibration, "
      else "this data frame already has a calibration named '{calibration}', ",
      "please specify a different calibration name with 'calibration=\"name\"' to add an additional calibration"
    ) %>% stop(call. = FALSE)
  }

  model_quos <- enquo(model)
  if (quo_is_call(model_quos) && rlang::call_name(model_quos) %in% c("c", "list")) {
    model_quos <- quos(!!!rlang::call_args(model_quos))
  } else {
    model_quos <- quos(!!model_quos)
  }
  filter_quo <-
    resolve_defaults(
      if (!missing(use_in_calib)) enquo(use_in_calib)
      else if (!missing(is_std_peak)) enquo(is_std_peak)
      else enquo(is_standard)
    )
  calib_vars <- get_calibration_vars(calibration)

  # information
  if (!quiet) {
    model_info <- map_chr(model_quos, as_label)
    models <-
      ifelse(
        nchar(names(model_quos)) > 0,
        sprintf("%s = '%s'", names(model_quos), model_info),
        sprintf("'%s'", model_info)
      )
    plural <- if (length(models) > 1) "s" else ""
    glue("Info: generating {calib_vars$calib_name}calibration based on {length(models)} model{plural} ({collapse(models, ', ')}) ",
         "for {nrow(dt)} data group(s) with standards filter '{get_quo_text(filter_quo)}'. ",
         "Storing residuals in new column '{calib_vars$residual}'. ",
         "Storing calibration info in new column '{calib_vars$in_reg}'.") %>%
      message()
  }

  # loess warning
  model_funcs <- map_chr(model_quos, ~if (rlang::quo_is_call(.x)) {rlang::call_name(.x)} else {""})
  if (calibration != "drift" && any(model_funcs == "loess")) {
    warning("local polynomial regression fitting ('loess') is discouraged except for drift corrections because it may lead to spurious calibration results. Please use with care.", immediate. = TRUE, call. = FALSE)
  }

  # auto correct loess date time
  model_quos <- correct_loess_date_time(dt, model_quos)

  # run regression
  dt_w_regs <- run_regression(
    dt = dt, model = quos(!!!model_quos), nest_model = TRUE,
    min_n_datapoints = min_n_datapoints,
    model_data = all_data, model_filter_condition = !!filter_quo,
    model_name = !!sym(calib_vars$model_name),
    model_enough_data = !!sym(calib_vars$model_enough_data),
    model_data_points = !!sym(calib_vars$model_data_points),
    model_params = !!sym(calib_vars$model_params),
    in_reg = !!sym(calib_vars$in_reg),
    residual = !!sym(calib_vars$residual),
    model_fit = model_fit, model_coefs = model_coefs, model_summary = model_summary
  )

  return(dt_w_regs)
}


# fix datetime loess in a model
correct_loess_date_time <- function(dt, model_quos) {

  stopifnot("all_data" %in% names(dt))
  models <- map_chr(model_quos, ~if (rlang::quo_is_call(.x)) {rlang::call_name(.x)} else {""})
  x_vars <- map(model_quos, get_model_formula_variables, get_y = FALSE)

  # check if a model is loess and column is datetime
  loess_datetime_columns <- map2(models, x_vars, function(model, x_vars) {
    if (model != "loess") return(character(0))
    x_var_is_datetime <-
      map_lgl(x_vars, function(x_var) {
        any(map_lgl(dt$all_data, ~x_var %in% names(.x) && inherits(.x[[x_var]], "POSIXct")))
      })
    return(x_vars[x_var_is_datetime])
  })

  # models that need updating
  needs_fix <- map_lgl(loess_datetime_columns, ~length(.x) > 0)
  needs_name <- needs_fix & nchar(names(model_quos)) == 0
  names(model_quos)[needs_name] <- map_chr(model_quos[needs_name], rlang::as_label)
  model_quos[needs_fix] <-
    map2(model_quos[needs_fix], loess_datetime_columns[needs_fix], function(mquo, cols) {
      text <- rlang::as_label(mquo)
      for(col in cols) {
        quoted_col <- sprintf("`%s`", col)
        if (stringr::str_detect(text, quoted_col))
          text <- stringr::str_replace_all(text, fixed(quoted_col), sprintf("as.numeric(%s)", quoted_col))
        else
          text <- stringr::str_replace_all(text, fixed(col), sprintf("as.numeric(%s)", col))
      }
      return(rlang::parse_quo(text, env = rlang::quo_get_env(mquo)))
    })

  return(model_quos)
}


#' Fetch problematic calibrations
#'
#' Fetch data sets that have problematic calibrations (not enough data or another error occured during calibration) for further inspection. This function is typically called after \link{iso_generate_calibration} to inspect problematic data sets. It requires the columns generated by \link{iso_generate_calibration} and the same \code{calibration} parameter used there.
#'
#' @inheritParams iso_generate_calibration
#' @param select which columns to select for display - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns.
#' @inheritParams iso_show_default_processor_parameters
#' @family calibration functions
#' @export
iso_get_problematic_calibrations <- function(dt, calibration = last_calibration(dt), select = everything(), quiet = default(quiet)) {

  # safety checks
  if (missing(dt) || !is.data.frame(dt)) stop("no data frame supplied", call. = FALSE)
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(dt, calib_vars$model_enough_data)
  dt_cols <- get_column_names(dt, select = enquo(select), n_reqs = list(select = "+"))

  # fetch
  dt_out <- dt %>%
    filter(!(!!sym(calib_vars$model_enough_data))) %>%
    dplyr::select(!!!c(dt_cols$select))

  if(!quiet) {
    # note: numbering does not make sense here because of the unique filter
    if (nrow(dt_out) == 0)
      glue("Info: there were no problematic calibrations") %>% message()
    else
      glue("Info: fetching problematic calibrations ({nrow(dt_out)} of {nrow(dt)})") %>%
      message()
  }

  if (nrow(dt_out) == 0)
    return(invisible(dt_out))
  else
    return(dt_out)
}

#' Remove problematic calibrations
#'
#' Remove calibrations that were problematic.
#' @inheritParams iso_generate_calibration
#' @param remove_calib_ok_column whether to automatically remove the calibration ok (\code{calib_ok}) column after using it to remove problematic calibrations. This helps automatically clean up the data table and remove information that is no longer needed.
#' @family calibration functions
#' @export
iso_remove_problematic_calibrations <- function(dt, calibration = last_calibration(dt), remove_calib_ok_column = TRUE, quiet = default(quiet)) {
  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_enough_data)

  # fetch
  dt_out <- filter(dt, !!sym(calib_vars$model_enough_data))

  if(!quiet) {
    if (nrow(dt_out) == nrow(dt))
      glue("Info: there are no problematic calibrations") %>%
      message()
    else
      glue("Info: removing problematic calibrations ({nrow(dt) - nrow(dt_out)} of {nrow(dt)})") %>%
      message()
  }

  # reomve calib ok column
  if (remove_calib_ok_column)
    dt_out <- dt_out %>% select(-!!sym(calib_vars$model_enough_data))

  return(dt_out)
}

# CALIBRATION RANGES -------

#' Evaluate calibration range
#'
#' Evaluates the calibration ranges for all calibrations and all data with respect to the provided terms (\code{...}). Generates a summary column called \code{in_range} (with \code{calibration} prefix if used) in the \code{all_data} data frames summarizing the range information. Also stores the calibration ranges themselves in a nested data frame, which can be accessed via \link{iso_unnest_calibration_range} if needed.
#'
#' Note that this function requires prior generation of a calibration (\code{\link{iso_generate_calibration}}). All measured parameters and derived terms can be included in the calibration range evalution. However, if the predicted term is intended to be included in the range evaluation, the calibration(s) must also be applied (\code{\link{iso_apply_calibration}}) first so the predicted term is actually available.
#'
#' @inheritParams evaluate_range
#' @inheritParams iso_show_default_processor_parameters
#' @family calibration functions
#' @export
iso_evaluate_calibration_range <- function(dt, ..., calibration = last_calibration(dt), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  terms_quos <- rlang::enquos(...)
  if (length(terms_quos) == 0) {
    stop("no terms for calibration range evaluation are provided, please specify at least one term", call. = FALSE)
  }

  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(dt, calib_vars$model_params)

  # information
  if (!quiet) {
    n_models <- length(unique(dt[[calib_vars$model_name]]))
    glue("Info: evaluating range for terms ",
         "'{glue::glue_collapse(map_chr(terms_quos, rlang::as_label), sep = \"', '\", last = \"' and '\")}' ",
         "in {calib_vars$calib_name}calibration ",
         "for {nrow(dt)/n_models} data group(s) in {n_models} model(s); ",
         "storing resulting summary for each data entry in new column '{calib_vars$in_range}'.") %>%
      message()
  }

  # evaluate range
  dt_out <- evaluate_range(
    dt = dt, !!!terms_quos,
    nested_model = TRUE,
    model_data = all_data,
    model_params = !!sym(calib_vars$model_params),
    in_reg = !!sym(calib_vars$in_reg),
    model_range = model_range,
    in_range = !!sym(calib_vars$in_range)
  )

  return(dt_out)
}

# APPLYING CALIBRATION --------

#' Apply calibration
#'
#' @inheritParams apply_regression
#' @param dt nested data table with \code{all_data} and calibration columns (see \link{iso_generate_calibration})
#' @param calibration name of the calibration to apply, must match the name used in \link{iso_generate_calibration} (if any)
#' @inheritParams iso_show_default_processor_parameters
#' @return the data table with the following columns added to the nested \code{all_data} \:
#' \itemize{
#'   \item{\code{predict} column with suffix \code{_pred}: }{the predicted value from applying the calibration}
#'   \item{\code{predict} column with suffix \code{_pred_se}: }{the error of the predicated value propagated from the calibration. Only created if \code{calculate_error = TRUE}.}
#'   \item{\code{predict} column with suffix \code{_pred_in_range}: }{reports whether a data entry is within the range of the calibration by checking whether ALL dependent and independent variables in the regression model are within the range of the calibration - is set to FALSE if any(!) of them are not - i.e. this column provides information on whether new values are extrapolated beyond a calibration model and treat the extrapolated ones with the appropriate care. Note that all missing predicted values (due to missing parameters) are also automatically flagged as not in range}
#' }
#' @family calibration functions
#' @export
iso_apply_calibration <- function(dt, predict, calibration = last_calibration(dt), predict_range = NULL,
                                  calculate_error = FALSE, quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(predict)) stop("no variable to predict specified", call. = FALSE)
  pred_quo <- enquo(predict)
  pred_col_quo <- pred_quo %>% as_label() %>% str_c("_pred") %>% sym()
  pred_se_col_quo <- pred_quo %>% as_label() %>% str_c("_pred_se") %>% sym()
  pred_se_in_range_quo <- pred_quo %>% as_label() %>% str_c("_pred_in_range") %>% sym()
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(dt, calib_vars$model_params)
  if (!has_regression_fit(dt, calibration = calibration))
    stop("calibration regression fits are no longer available in this data frame. Please make sure to run ?iso_apply_calibration after ?iso_generate_calibration, and not after ?iso_get_calibration_data (which removes the large calibration fit objects by default).", call. = FALSE)

  # information
  if (!quiet) {
    n_models <- length(unique(dt[[calib_vars$model_name]]))
    glue::glue(
      "Info: applying {calib_vars$calib_name}calibration ",
      "to infer '{as_label(pred_quo)}' ",
      "for {nrow(dt)/n_models} data group(s) in {n_models} model(s); ",
      "storing resulting value in new column '{as_label(pred_col_quo)}'",
      if (calculate_error) " and estimated error in new column '{as_label(pred_se_col_quo)}'" else "",
      ". This may take a moment... ") %>%
      message(appendLF = FALSE)
  }

  # apply regression
  dt_out <- apply_regression(
    dt = dt,
    predict = !!pred_quo,
    nested_model = TRUE,
    calculate_error = calculate_error,
    model_data = all_data,
    model_name = !!sym(calib_vars$model_name),
    model_fit = model_fit,
    model_params = !!sym(calib_vars$model_params),
    predict_value = !!pred_col_quo,
    predict_error = !!pred_se_col_quo,
    predict_range = predict_range
  )

  if (!quiet)
    message("finished.")

  return(dt_out)
}


# RETRIEVING INFORMATION --------

#' Retrieve calibration data
#'
#' Pull out data from the \code{all_data} nested calibration data frame (by default unnests all columnes). Typically used at various points after a dataset is prepared for calibration with \link{iso_prepare_for_calibration}, calibrations are generated with \link{iso_generate_calibration} and/or calibrations are applied with \link{iso_apply_calibration}. Note that when unnesting multi-row data (i.e. almost always), remaining data and calibration parameters are replicated across the unnested data set. This is NOT a problem for any downstream functions and in fact allows them to access the underlying calibrations if needed (e.g. in \code{\link{iso_mark_calibration_range}}). However, if this is undesired, please set \code{keep_remaining_data = FALSE} and \code{keep_calibrations = FALSE} when calling \code{iso_get_calibration_data} or use the \code{\link{iso_remove_list_columns}} function afterwards, which removes all nested information.
#'
#' @param dt nested data frame with column \code{all_data} (see \link{iso_prepare_for_calibration}) and any number of calibrations (see \link{iso_generate_calibration})
#' @param select which columns to unnest - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default (i.e. unnests all information).
#' @param keep_remaining_data whether to keep the remaining parts of the partially unnested \code{all_data} data set (irrelevant if \code{select = everything()}). By default, remainig data is kept.
#' @param keep_calibration_parameters whether to keep the calibration parameter columns (coefficients, summary, range) in the resulting data frame. By default, calibration parameters are kept to facilitate downstream operations that may need these calibration information (such as \link{iso_mark_calibration_range}).
#' @param keep_calibration_regressions whether to keep the actual calibration regression objects in the resulting data frame. By default, regression objects are NOT kept because they lead to large data objects and do not usually have additional use after \link{iso_apply_calibration} has been called. There is rarely a reason to change this parameter to \code{TRUE}.
#' @param keep_remaining_nested_data DEPRECATED - renamed to \code{keep_remaining_data}
#' @param keep_other_list_data DEPRECATED - renamed to \code{keep_calibrations}
#' @inheritParams iso_show_default_processor_parameters
#' @family calibration functions
#' @export
iso_get_calibration_data <- function(
  dt, select = everything(),
  keep_remaining_data = keep_remaining_nested_data,
  keep_calibration_parameters = keep_other_list_data,
  keep_calibration_regressions = FALSE,
  quiet = default(quiet),
  keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE) {

  # deprecation warnings
  if (!missing(keep_remaining_nested_data))
    warning("'keep_remaining_nested_data' was renamed to 'keep_remaining_data' for consistency. Please start using 'keep_remaining_data' as 'keep_remaining_nested_data' will be removed in a future version of isoprocessor.", immediate. = TRUE, call. = FALSE)
  if (!missing(keep_other_list_data))
    warning("'keep_other_list_data' was renamed to 'keep_calibration_parameters' for consistency. Please start using 'keep_calibration_parameters' as 'keep_other_list_data' will be removed in a future version of isoprocessor.", immediate. = TRUE, call. = FALSE)

  # info
  select_quo <- rlang::enquo(select)
  if (!quiet) {
    select_everything <- rlang::as_label(select_quo) == "everything()"
    glue::glue(
        "Info: retrieving ",
        if (select_everything) "all data"
        else isoreader:::get_info_message_concat(select_quo, prefix = "data column(s) "),
        if (!select_everything && keep_remaining_data)
          ", keeping remaining data in nested 'all_data'"
        else ""
      ) %>%
      message()
  }

  if (!keep_calibration_regressions)
    dt <- strip_regression_fits(dt)

  unnest_select_data(dt, select = !!select_quo, nested_data = all_data,
                     keep_remaining_nested_data = keep_remaining_data,
                     keep_other_list_data = keep_calibration_parameters)
}



#' Get calibration coefficients
#'
#' Retrieve calibration coefficients for a calibration generated by \link{iso_generate_calibration}.
#'
#' @inheritParams iso_get_calibration_data
#' @param keep_remaining_nested_data DEPRECATED - this parameter was removed for clarity and does no longer have any effect
#' @param keep_other_calibrations whether columns that belong to other calibrations (besides the one specified by \code{calibration}) should be kept in the data table. By default, they are removed for clarity.
#' @family calibration functions
#' @export
iso_get_calibration_coefficients <- function(
  dt, calibration = last_calibration(dt), select = everything(),
  keep_calibration_parameters = keep_other_list_data,
  keep_calibration_regressions = FALSE,
  keep_other_calibrations = FALSE,
  quiet = default(quiet),
  keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {

  # warnings
  if (!missing(keep_remaining_nested_data))
    warning("'keep_remaining_nested_data' was removed for clarity.", immediate. = TRUE, call. = FALSE)
  if (!missing(keep_other_list_data))
    warning("'keep_other_list_data' was renamed to 'keep_calibration_parameters' for consistency. Please start using 'keep_calibration_parameters' as 'keep_other_list_data' will be removed in a future version of isoprocessor.", immediate. = TRUE, call. = FALSE)
  if (keep_calibration_regressions && !keep_calibration_parameters)
    warning("cannot keep calibration regressions without keeping calibration parameters, please set 'keep_calibration_parameters = TRUE'", immediate. = TRUE, call. = FALSE)

  # safety check
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(dt, calib_vars$model_params)

  # info
  select_quo <- rlang::enquo(select)
  if (!quiet) {
    select_everything <- rlang::as_label(select_quo) == "everything()"
    glue::glue(
      "Info: retrieving ",
      if (select_everything) "all coefficient information"
      else isoreader:::get_info_message_concat(select_quo, prefix = "coefficient column(s) "),
      " for {calib_vars$calib_name}calibration"
    ) %>%
      message()
  }

  # strip regression fits
  if (!keep_calibration_regressions)
    dt <- strip_regression_fits(dt)

  # remove other calibrations
  if (!keep_other_calibrations)
    dt <- remove_other_calibrations(dt, calibration = calibration)

  unnest_model_column(
    dt, model_column = model_coefs, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
    select = !!select_quo,
    keep_remaining_nested_data = FALSE, keep_other_list_data = keep_calibration_parameters
  )
}

#' Get calibration summary
#'
#' Retrieve summary information for a calibration generated by \link{iso_generate_calibration}.
#'
#' @inheritParams iso_get_calibration_coefficients
#' @family calibration functions
#' @export
iso_get_calibration_summary <- function(
  dt, calibration = last_calibration(dt), select = everything(),
  keep_calibration_parameters = keep_other_list_data,
  keep_calibration_regressions = FALSE,
  keep_other_calibrations = FALSE,
  quiet = default(quiet),
  keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {

  # warnings
  if (!missing(keep_remaining_nested_data))
    warning("'keep_remaining_nested_data' was removed for clarity.", immediate. = TRUE, call. = FALSE)
  if (!missing(keep_other_list_data))
    warning("'keep_other_list_data' was renamed to 'keep_calibration_parameters' for consistency. Please start using 'keep_calibration_parameters' as 'keep_other_list_data' will be removed in a future version of isoprocessor.", immediate. = TRUE, call. = FALSE)
  if (keep_calibration_regressions && !keep_calibration_parameters)
    warning("cannot keep calibration regressions without keeping calibration parameters, please set 'keep_calibration_parameters = TRUE'", immediate. = TRUE, call. = FALSE)

  # safety check
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(dt, calib_vars$model_params)

  # info
  select_quo <- rlang::enquo(select)
  if (!quiet) {
    select_everything <- rlang::as_label(select_quo) == "everything()"
    glue::glue(
      "Info: retrieving ",
      if (select_everything) "all summary information"
      else isoreader:::get_info_message_concat(select_quo, prefix = "summary column(s) "),
      " for {calib_vars$calib_name}calibration"
    ) %>%
      message()
  }

  # strip regression fits
  if (!keep_calibration_regressions)
    dt <- strip_regression_fits(dt)

  # remove other calibrations
  if (!keep_other_calibrations)
    dt <- remove_other_calibrations(dt, calibration = calibration)

  unnest_model_column(
    dt, model_column = model_summary, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
    select = !!select_quo,
    keep_remaining_nested_data = FALSE, keep_other_list_data = keep_calibration_parameters
  )
}

#' Get calibration parameters
#'
#' This function is deprecated for clarity. Please use \code{\link{iso_get_calibration_coefficients}} and \code{\link{iso_get_calibration_summary}} directly.
#'
#' @inheritParams iso_get_calibration_coefficients
#' @param select_from_coefs which columns from the fit coeffiencts to include, supports full dplyr syntax including renaming
#' @param select_from_summary which columns from the fit summary to include, supports full dplyr syntax including renaming
#' @export
iso_get_calibration_parameters <-
  function(dt, calibration = last_calibration(dt),
           select_from_coefs = everything(), select_from_summary = everything(),
           keep_calibration_parameters = keep_other_list_data,
           keep_calibration_regressions = FALSE,
           keep_other_calibrations = FALSE,
           quiet = default(quiet),
           keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {

    if (!missing(keep_remaining_nested_data))
      warning("'keep_remaining_nested_data' was removed for clarity.", immediate. = TRUE, call. = FALSE)
    if (!missing(keep_other_list_data))
      warning("'keep_other_list_data' was renamed to 'keep_calibration_parameters' for consistency. Please start using 'keep_calibration_parameters' as 'keep_other_list_data' will be removed in a future version of isoprocessor.", immediate. = TRUE, call. = FALSE)

    dt %>%
      # unnest calibration coefs
      iso_get_calibration_coefficients(
        calibration = calibration,
        select = !!enquo(select_from_coefs),
        keep_calibration_parameters = TRUE,
        keep_calibration_regressions = TRUE,
        keep_other_calibrations = TRUE) %>%
      # unnest calibration summary
      iso_get_calibration_summary(
        calibration = calibration,
        select = !!enquo(select_from_summary),
        keep_calibration_parameters = keep_calibration_parameters,
        keep_calibration_regressions = keep_calibration_regressions,
        keep_other_calibrations = keep_other_calibrations)
  }

#' Get calibration range
#'
#' Retrieve range information created by \link{iso_evaluate_calibration_range}.
#'
#' @inheritParams iso_get_calibration_coefficients
#' @family calibration functions
#' @export
iso_get_calibration_range <- function(
  dt, calibration = last_calibration(dt), select = everything(),
  keep_calibration_parameters = keep_other_list_data,
  keep_calibration_regressions = FALSE,
  keep_other_calibrations = FALSE,
  quiet = default(quiet),
  keep_remaining_nested_data = FALSE, keep_other_list_data = FALSE) {

  # warnings
  if (!missing(keep_remaining_nested_data))
    warning("'keep_remaining_nested_data' was removed for clarity.", immediate. = TRUE, call. = FALSE)
  if (!missing(keep_other_list_data))
    warning("'keep_other_list_data' was renamed to 'keep_calibration_parameters' for consistency. Please start using 'keep_calibration_parameters' as 'keep_other_list_data' will be removed in a future version of isoprocessor.", immediate. = TRUE, call. = FALSE)
  if (keep_calibration_regressions && !keep_calibration_parameters)
    warning("cannot keep calibration regressions without keeping calibration parameters, please set 'keep_calibration_parameters = TRUE'", immediate. = TRUE, call. = FALSE)

  # safety checks
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(dt, calib_vars$model_params)
  if(!has_model_range(dt, calibration = calibration))
    glue::glue("calibration range has not yet been evaluated for this calibration, ",
               "please run ?iso_evaluate_calibration_range(calibration = '{calibration}', ...) first") %>%
    stop(call. = FALSE)

  # info
  select_quo <- rlang::enquo(select)
  if (!quiet) {
    select_everything <- rlang::as_label(select_quo) == "everything()"
    glue::glue(
      "Info: retrieving ",
      if (select_everything) "all calibration range information"
      else isoreader:::get_info_message_concat(select_quo, prefix = "calibration range column(s) "),
      " for {calib_vars$calib_name}calibration"
    ) %>%
      message()
  }

  # strip regression fits
  if (!keep_calibration_regressions)
    dt <- strip_regression_fits(dt)

  # remove other calibrations
  if (!keep_other_calibrations)
    dt <- remove_other_calibrations(dt, calibration = calibration)

  unnest_model_column(
    dt, model_column = model_range, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
    select = !!select_quo,
    keep_remaining_nested_data = FALSE, keep_other_list_data = keep_calibration_parameters
  )
}


# DEPRECATED =====

#' @rdname iso_get_calibration_data
#' @details \code{iso_unnest_data} is deprecated in favor of the more specific \code{iso_get_calibration_data} and will be removed in a future version of isoprocessor.
#' @export
iso_unnest_data <- function(...) {
  warning("'iso_unnest_data' is deprecated in favor of the more specific 'iso_get_calibration_data' and will be removed in future versions of isoprocessor. Please use 'iso_get_calibration_data' instead", immediate. = TRUE, call. = FALSE)
  iso_get_calibration_data(...)
}

#' @rdname iso_get_calibration_coefficients
#' @details \code{iso_unnest_calibration_coefs} is deprecated in favor of the equivalent \code{iso_get_calibration_coefficients} to standardize function call names. Please start using \code{iso_get_calibration_coefficients} as \code{iso_unnest_calibration_coefs} will be deprecated in a future version of isoprocessor.
#' @export
iso_unnest_calibration_coefs <- function(...) {
  warning("'iso_unnest_calibration_coefs' is deprecated in favor of 'iso_get_calibration_coefs' and will be removed in future versions of isoprocessor. Please use 'iso_get_calibration_coefs' instead", immediate. = TRUE, call. = FALSE)
  iso_get_calibration_coefs(...)
}

#' @rdname iso_get_calibration_summary
#' @details \code{iso_unnest_calibration_summary} is deprecated in favor of the equivalent \code{iso_get_calibration_summary} to standardize function call names. Please start using \code{iso_get_calibration_summary} as \code{iso_unnest_calibration_summary} will be deprecated in a future version of isoprocessor.
#' @export
iso_unnest_calibration_summary <- function(...) {
  warning("'iso_unnest_calibration_summary' is deprecated in favor of 'iso_get_calibration_summary' and will be removed in future versions of isoprocessor. Please use 'iso_get_calibration_summay' instead", immediate. = TRUE, call. = FALSE)
  iso_get_calibration_summary(...)
}

#' @rdname iso_get_calibration_parameters
#' @details \code{iso_unnest_calibration_parameters} is deprecated in favor of the equivalent \code{iso_get_calibration_parameters} to standardize function call names. Please start using \code{iso_get_calibration_parameters} as \code{iso_unnest_calibration_parameters} will be deprecated in a future version of isoprocessor.
#' @export
iso_unnest_calibration_parameters <- function(...) {
  warning("'iso_unnest_calibration_parameters' is deprecated in favor of 'iso_get_calibration_parameters' and will be removed in future versions of isoprocessor. Please use 'iso_get_calibration_summay' instead", immediate. = TRUE, call. = FALSE)
  iso_get_calibration_parameters(...)
}

#' @rdname iso_get_calibration_range
#' @details \code{iso_unnest_calibration_range} is deprecated in favor of the equivalent \code{iso_get_calibration_range} to standardize function call names. Please start using \code{iso_get_calibration_range} as \code{iso_unnest_calibration_range} will be deprecated in a future version of isoprocessor.
#' @export
iso_unnest_calibration_range <- function(...) {
  warning("'iso_unnest_calibration_range' is deprecated in favor of 'iso_get_calibration_data' and will be removed in future versions of isoprocessor. Please use 'iso_get_calibration_data' instead", immediate. = TRUE, call. = FALSE)
  iso_get_calibration_range(...)
}


