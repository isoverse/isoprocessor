# PREPARING ----

#' Add calibration standards
#' @param dt data table
#' @param stds standards data frame
#' @param match_by what column(s) to match the standards by
#' @param is_std_peak new column that holds information about which ones are standard peaks (i.e. have known isotopic values)
#' @param is_standard renamed to \code{is_std_peak} because the naming caused too much confusion, will be removed in future versions, please use \code{is_std_peak} instead
#' @inheritParams iso_show_default_processor_parameters
#' @return data frame with standards data frame merged in and the following information column added:
#' \itemize{
#' \item{\code{is standard}: }{a logical TRUE/FALSE indicating which data table entry is a standard}
#' }
#' @family calibration functions
#' @export
iso_add_standards <- function(dt, stds, match_by = default(std_match_by), is_std_peak = default(is_std_peak), quiet = default(quiet), is_standard = NULL) {

  # make sure params supplied
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(stds)) stop("no standards table supplied", call. = FALSE)
  if (!missing(is_standard)) {
    stop("the parameter is_standard was renamed to is_std_peak to avoid confusion, please use is_std_peak instead", call. = FALSE)
  }

  # column names allowing standard and NSE
  dt_cols <- get_column_names(!!enquo(dt), match_by = enquo(match_by), n_reqs = list(match_by = "+"))
  stds_cols <- get_column_names(!!enquo(stds), match_by = enquo(match_by), n_reqs = list(match_by = "+"))
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

#' Prepare data set for calibration
#'
#' Nests data set in preparation for calibration calculations. Use the \code{group_by} parameter to group analyses as appropriate for calibration. Use \link{iso_unnest_data} to easily pull out additional columns from the nested data frame in \code{all_data} at any point before, during or after calibration.
#'
#' @param dt data table
#' @param group_by what to group by for indidual calibration calculations (use c(...) to select multiple) - set \code{group_by = NULL} to avoid grouping
#' @inheritParams iso_show_default_processor_parameters
#' @return a nested data set with the \code{group_by} columns out front and the remaining data in a new nested column named \code{all_data}
#' @export
iso_prepare_for_calibration <- function(dt, group_by = NULL, quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  group_quo <- enquo(group_by)
  nested_data_quo <- quo(all_data)
  dt_cols <- get_column_names(!!enquo(dt), group_by = group_quo, n_reqs = list(group_by = "*"))
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

  return(nest_data(dt, group_by = !!group_quo, nested_data = !!nested_data_quo))
}

# CALIBRATIONS -----

# convenience function for common calibration variables
get_calibration_vars <- function(calibration) {
  prefix_with_sep <- if (nchar(calibration) > 0) str_c(calibration, "_") else ""
  list(
    calib_name = if(calibration != "") as.character(glue("'{calibration}' ")) else "",
    model_name = str_c(prefix_with_sep, "calib"),
    model_enough_data = str_c(prefix_with_sep, "calib_ok"),
    model_params = str_c(prefix_with_sep, "calib_params"),
    residual = str_c(prefix_with_sep, "resid"),
    in_reg = str_c(prefix_with_sep, "in_calib"),
    in_range = str_c(prefix_with_sep, "in_range")
  )
}

# convenience function for checking presence of calibration variables
# @param cols the parts of get_calibration_vars that should be checked
check_calibration_cols <- function(df, cols) {
  # df name and data frame test
  df_name <- enquo(df) %>% quo_text()
  df <- enquo(df) %>% eval_tidy()
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)

  if (length(missing <- setdiff(unlist(cols), names(df))) > 0) {
    glue("'{collapse(missing, sep = \"', '\", last = \"' and '\")}' refer(s) to unknown column(s) in data frame '{df_name}' - make sure to run iso_generate_calibration() first and to use the same 'calibration' parameter") %>%
      stop(call. = FALSE)
  }
}

#' Generate data calibration
#'
#' Generate a calibration for a specific variable based on one or multiple calibration models. Requires properly nested and grouped data, see \link{iso_prepare_for_calibration} for details. Note that to calibrate different variables, separate calls to this function should be issued each with different \code{calibration} names.
#'
#' @param dt nested data table with column \code{all_data} (see \link{iso_prepare_for_calibration})
#' @param model a single regression model or a list of multiple alternative regression models for the calibration. If a named list is provided, the name(s) will be used instead of the formulaes for the model identification column. Note that if multiple models are provided, the entire data table rows will be duplicated to consider the different models in parallel.
#' @param calibration an informative name for the calibration (could be e.g. \code{"d13C"} or \code{"conc"}). If provided, will be used as a prefix for the new columns generated by this function. This parameter is most useful if there are multiple variables in the data set that need to be calibrated (e.g. multiple delta values, concentration, etc.). If there is only a single variable to calibrate, the \code{calibration} parameter is completely optional and can just be left blank (the default).
#' @param is_std_peak column or filter condition to determine which subset of data to actually use for the calibration (default is the \code{is_std_peak} field introduced by \code{\link{iso_add_standards}}).
#' @inheritParams run_regression
#' @inheritParams iso_show_default_processor_parameters
#' @return the data table with the following columns added (prefixed by the \code{calibration} parameter if provided):
#' \itemize{
#'   \item{\code{calib}: }{the name of the calibration if provided in the \code{model} parameter, otherwise the formula}
#'   \item{\code{calib_ok}: }{a TRUE/FALSE column indicating whether there was enough data for calibration to be generated}
#'   \item{\code{calib_params}: }{a nested dataframe that holds the actual regression model fit, coefficients, summary and data range. These parameters are most easily accessed using the functions \code{\link{iso_unnest_calibration_coefs}}, \code{\link{iso_unnest_calibration_summary}}, \code{\link{iso_unnest_calibration_parameters}}, \code{\link{iso_unnest_calibration_range}}, or directly via \code{\link[tidyr]{unnest}}}
#'   \item{\code{resid} within \code{all_data}: }{a new column within the nested \code{all_data} that holds the residuals for all standards used in the regression model}
#' }
#' @export
iso_generate_calibration <- function(dt, model, calibration = "", is_std_peak = default(is_std_peak), min_n_datapoints = 2, quiet = default(quiet), is_standard = NULL) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(model)) stop("no calibration model(s) supplied", call. = FALSE)
  if (!missing(is_standard)) {
    stop("the parameter is_standard was renamed to is_std_peak to avoid confusion, please use is_std_peak instead", call. = FALSE)
  }

  dt_quo <- enquo(dt)
  model_quos <- enquo(model)
  filter_quo <- enquo(is_std_peak) %>% resolve_defaults()
  calib_vars <- get_calibration_vars(calibration)

  # information
  if (!quiet) {
    if (quo_is_call(model_quos) && quo_text(lang_head(model_quos)) %in% c("c", "list")) {
      lquos <- quos(!!!lang_args(model_quos))
    } else {
      lquos <- quos(!!!model_quos)
    }
    models <- str_c(names(lquos) %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) }, map_chr(lquos, quo_text))
    plural <- if (length(models) > 1) "s" else ""
    glue("Info: generating {calib_vars$calib_name}calibration based on {length(models)} model{plural} ('{collapse(models, \"', '\")}') ",
         "for {nrow(dt)} data group(s) with standards filter '{quo_text(filter_quo)}'. ",
         "Storing residuals in new column '{calib_vars$residual}'. ",
         "Storing calibration info in new column '{calib_vars$in_reg}'.") %>%
      message()
  }

  # run regression
  dt_w_regs <- run_regression(
    dt = dt, model = !!model_quos, nest_model = TRUE,
    min_n_datapoints = min_n_datapoints,
    model_data = all_data, model_filter_condition = !!filter_quo,
    model_name = !!sym(calib_vars$model_name),
    model_enough_data = !!sym(calib_vars$model_enough_data),
    model_params = !!sym(calib_vars$model_params),
    in_reg = !!sym(calib_vars$in_reg),
    residual = !!sym(calib_vars$residual),
    model_fit = model_fit, model_coefs = model_coefs, model_summary = model_summary
  )

  return(dt_w_regs)
}


#' Fetch problematic calibrations
#'
#' Fetch data sets that have problematic calibrations (not enough data or another error occured during calibration) for further inspection. This function is typically called after \link{iso_generate_calibration} to inspect problematic data sets. It requires the columns generated by \link{iso_generate_calibration} and the same \code{calibration} parameter used there.
#'
#' @inheritParams iso_generate_calibration
#' @param select which columns to select for display - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns.
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_get_problematic_calibrations <- function(dt, calibration = "", select = everything(), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_enough_data)
  dt_cols <- get_column_names(!!enquo(dt), select = enquo(select), n_reqs = list(select = "+"))

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
#' @export
iso_remove_problematic_calibrations <- function(dt, calibration = "", remove_calib_ok_column = TRUE, quiet = default(quiet)) {
  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_enough_data)

  # fetch
  dt_out <- filter(dt, !!sym(calib_vars$model_enough_data))

  if(!quiet && nrow(dt_out) != nrow(dt)) {
    glue("Info: removing problematic calibrations ({nrow(dt) - nrow(dt_out)} of {nrow(dt)})") %>%
      message()
  }

  # reomve calib ok column
  if (remove_calib_ok_column)
    dt_out <- dt_out %>% select(-!!sym(calib_vars$model_enough_data))

  return(dt_out)
}

# EVALUATING CALIBRATION RANGES -------

#' Evaluate calibration range
#'
#' Evaluates the calibration ranges for all calibrations and all data with respect to the provided terms (\code{...}). Generates a summary column called \code{in_range} (with \code{calibration} prefix if used) in the \code{all_data} data frames summarizing the range information. Also stores the calibration ranges themselves in a nested data frame, which can be accessed via \link{iso_unnest_calibration_range} if needed.
#'
#' Note that this function requires prior generation of a calibration (\code{\link{iso_generate_calibration}}). All measured parameters and derived terms can be included in the calibration range evalution. However, if the predicted term is intended to be included in the range evaluation, the calibration(s) must also be applied (\code{\link{iso_apply_calibration}}) first so the predicted term is actually available.
#'
#' @inheritParams evaluate_range
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_evaluate_calibration_range <- function(dt, ..., calibration = "", quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  terms_quos <- rlang::enquos(...)
  if (length(terms_quos) == 0) {
    stop("no terms for calibration range evaluation are provided, please specify at least one term", call. = FALSE)
  }

  dt_quo <- enquo(dt)
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!dt_quo, calib_vars$model_params)

  # information
  if (!quiet) {
    glue("Info: evaluating range for terms ",
         "'{glue::glue_collapse(map_chr(terms_quos, rlang::as_label), sep = \"', '\", last = \"' and '\")}' ",
         "in {calib_vars$calib_name}calibration for {nrow(dt)} data group(s); ",
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

# INVERTING CALIBRATION --------

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
#' @export
iso_apply_calibration <- function(dt, predict, calibration = "", predict_range = NULL,
                                  calculate_error = FALSE, quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(predict)) stop("no variable to predict specified", call. = FALSE)
  dt_quo <- enquo(dt)
  pred_quo <- enquo(predict)
  pred_col_quo <- pred_quo %>% quo_text() %>% str_c("_pred") %>% sym()
  pred_se_col_quo <- pred_quo %>% quo_text() %>% str_c("_pred_se") %>% sym()
  pred_se_in_range_quo <- pred_quo %>% quo_text() %>% str_c("_pred_in_range") %>% sym()
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!dt_quo, calib_vars$model_params)

  # information
  if (!quiet) {
    glue("Info: applying {calib_vars$calib_name}calibration ",
         "to infer '{quo_text(pred_quo)}' for {nrow(dt)} data group(s); ",
         "storing resulting value in new column '{quo_text(pred_col_quo)}'") %>%
      message(appendLF = FALSE)
    if (calculate_error)
      glue(" and estimated error in new column '{quo_text(pred_se_col_quo)}'") %>%
      message(appendLF = FALSE)

    message(". This may take a moment... ", appendLF = FALSE)
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


# UNNESTING --------

#' Unnest data
#'
#' Pull out columns from the \code{all_data} column. Typically used at various points after a dataset is prepared for calibration with \link{iso_prepare_for_calibration}, calibrations are generated with \link{iso_generate_calibration} and/or calibrations are applied with \link{iso_apply_calibration}. Note that unnesting of data columns that have multiple values per data set leads to row replication.
#'
#' @inheritParams unnest_model_column
#' @export
iso_unnest_data <- function(dt, select = everything(), keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE) {
  unnest_select_data(dt, select = !!enquo(select), nested_data = all_data, keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' Unnest calibration parameters
#'
#' Convenience function to unnest both calibration coefficients (\link{iso_unnest_calibration_coefs}) and calibration summary (\link{iso_unnest_calibration_summary}) columns in a single step.
#' @inheritParams iso_unnest_calibration_coefs
#' @inheritParams iso_unnest_calibration_summary
#' @param select_from_coefs which columns from the fit coeffiencts to include, supports full dplyr syntax including renaming
#' @param select_from_summary which columns from the fit summary to include, supports full dplyr syntax including renaming
#' @export
iso_unnest_calibration_parameters <-
  function(dt, calibration = "",
           select_from_coefs = everything(), select_from_summary = everything(),
           keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {

    dt %>%
      # unnest calibration coefs
      iso_unnest_calibration_coefs(
        calibration = calibration,
        select = !!enquo(select_from_coefs)) %>%
      # unnest calibration summary
      iso_unnest_calibration_summary(
        calibration = calibration,
        select = !!enquo(select_from_summary),
        keep_remaining_nested_data = keep_remaining_nested_data,
        keep_other_list_data = keep_other_list_data)
}

#' Unnest calibration coefficients
#'
#' Retrieve calibration coefficients for a calibration. Problematic calibrations are silently omitted (use \link{iso_get_problematic_calibrations} and \link{iso_remove_problematic_calibrations} to deal with them more explicitly).
#'
#' @inheritParams iso_generate_calibration
#' @inheritParams unnest_model_column
#' @export
iso_unnest_calibration_coefs <- function(dt, calibration = "", select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  unnest_model_column(
    dt, model_column = model_coefs, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
    select = !!(enquo(select)),
    keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data
  )
}

#' Unnest calibration summary
#'
#' Retrieve summary entries for a calibration. Problematic calibrations are silently omitted (use \link{iso_get_problematic_calibrations} and \link{iso_remove_problematic_calibrations} to deal with them more explicitly).
#'
#' @inheritParams iso_generate_calibration
#' @inheritParams unnest_model_column
#' @export
iso_unnest_calibration_summary <- function(dt, calibration = "", select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  unnest_model_column(
    dt,
    model_column = model_summary, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
    select = !!(enquo(select)),
    keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data
  )
}

#' Unnest calibration range
#'
#' Retrieve range information created by \link{iso_evaluate_calibration_range}. Problematic calibrations are silently omitted (use \link{iso_get_problematic_calibrations} and \link{iso_remove_problematic_calibrations} to deal with them more explicitly).
#'
#' @inheritParams iso_generate_calibration
#' @inheritParams unnest_model_column
#' @export
iso_unnest_calibration_range <- function(dt, calibration = "", select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  unnest_model_column(
    dt,
    model_column = model_range, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
    select = !!(enquo(select)),
    keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data
  )
}
