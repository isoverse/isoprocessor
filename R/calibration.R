# PREPARING ----

#' Add calibration standards
#' @param dt data table
#' @param stds standards data frame
#' @param match_by what column(s) to match the standards by
#' @param is_standard new column that holds information about what is a standard and what isn't
#' @return data frame with standards data frame merged in and the following information column added:
#' \itemize{
#' \item{\code{is standard}: }{a logical TRUE/FALSE indicating which data table entry is a standard}
#' }
#' @family calibration functions
#' @export
iso_add_standards <- function(dt, stds, match_by = default(std_match_by), quiet = default(quiet)) {

  # make sure params supplied
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(stds)) stop("no standards table supplied", call. = FALSE)

  # column names allowing standard and NSE
  dt_cols <- get_column_names(!!enquo(dt), match_by = enquo(match_by), n_reqs = list(match_by = "+"))
  stds_cols <- get_column_names(!!enquo(stds), match_by = enquo(match_by), n_reqs = list(match_by = "+"))
  new_cols <- get_new_column_names(is_standard = quo(is_standard))

  # select standards
  stds <- mutate(stds, ..marker.. = TRUE)
  dt_w_stds <- dt %>%
    left_join(stds, by = dt_cols$match_by) %>%
    mutate(!!new_cols$is_standard := !is.na(..marker..)) %>%
    select(-..marker..) %>%
    # arrange properly
    select(dt_cols$match_by, new_cols$is_standard, everything())

  if (!quiet) {
    n_stds <- filter(dt_w_stds, !!sym(new_cols$is_standard))[dt_cols$match_by] %>% unique() %>% nrow()
    n_stds_rows <- filter(dt_w_stds, !!sym(new_cols$is_standard)) %>% nrow()
    glue("Info: matching standards by '{collapse(dt_cols$match_by, sep = \"', '\", last = \"' and '\")}' - ",
         "added {n_stds} standard entries to {n_stds_rows} out of {nrow(dt_w_stds)} rows") %>%
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
    residual = str_c(prefix_with_sep, "resid")
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
#' @param is_standard column or filter condition to determine which subset of data to actually use for the calibration (default is the \code{is_standard} field introduced by \code{\link{iso_add_standards}}).
#' @return the data table with the following columns added (prefixed by the \code{calibration} parameter if provided):
#' \itemize{
#'   \item{\code{calib}: }{the name of the calibration if provided in the \code{model} parameter, otherwise the formula}
#'   \item{\code{calib_ok}: }{a TRUE/FALSE column indicating whether there was enough data for calibration to be generated}
#'   \item{\code{calib_params}: }{a nested dataframe that holds the actual regression model fit, coefficients and summary. These parameters are most easily accessed using the functions \code{\link{iso_unnest_calib_coefs}} and \code{\link{iso_unnest_calib_summary}}, or directly via \code{\link[tidyr]{unnest}}}
#'   \item{\code{resid} within \code{all_data}: }{a new column within the nested \code{all_data} that holds the residuals for all standards used in the regression model}
#' }
#' @export
iso_generate_calibration <- function(dt, model, calibration = "", is_standard = default(is_standard), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(model)) stop("no calibration model(s) supplied", call. = FALSE)
  dt_quo <- enquo(dt)
  model_quos <- enquo(model)
  filter_quo <- enquo(is_standard) %>% resolve_defaults()
  calib_vars <- get_calibration_vars(calibration)

  # information
  if (!quiet) {
    if (quo_is_lang(model_quos) && quo_text(lang_head(model_quos)) %in% c("c", "list")) {
      lquos <- quos(!!!lang_args(model_quos))
    } else {
      lquos <- quos(!!!model_quos)
    }
    models <- str_c(names(lquos) %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) }, map_chr(lquos, quo_text))
    plural <- if (length(models) > 1) "s" else ""
    glue("Info: generating {calib_vars$calib_name}calibration based on {length(models)} model{plural} ('{collapse(models, \"', '\")}') ",
         "for {nrow(dt)} data group(s) with standards filter '{quo_text(filter_quo)}'. ",
         "Storing residuals in new column '{calib_vars$residual}'.") %>%
      message()
  }

  # run regression
  dt_w_regs <- run_regression(
    !!dt_quo, model = !!model_quos, nest_model = TRUE,
    model_data = all_data, model_filter_condition = !!filter_quo,
    model_name = !!sym(calib_vars$model_name),
    model_enough_data = !!sym(calib_vars$model_enough_data),
    model_params = !!sym(calib_vars$model_params),
    residual = !!sym(calib_vars$residual),
    model_fit = model_fit, model_range = model_range, model_coefs = model_coefs, model_summary = model_summary
  )

  return(dt_w_regs)
}


#' Fetch problematic calibrations
#'
#' Fetch data sets that have problematic calibrations (not enough data or another error occured during calibration) for further inspection. This function is typically called after \link{iso_generate_calibration} to inspect problematic data sets. It requires the columns generated by \link{iso_generate_calibration} and the same \code{calibration} parameter used there.
#'
#' @inheritParams iso_generate_calibration
#' @param select which columns to select for display - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns.
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

# Note: does not have a 'remove_info_colum' feature because it makes it harder to automatically remove problematic models during unnesting
#' Remove problematic calibrations
#'
#' Remove calibrations that were problematic.
#' @inheritParams iso_generate_calibration
#' @export
iso_remove_problematic_calibrations <- function(dt, calibration = "", quiet = default(quiet)) {
  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_enough_data)

  # fetch
  dt_out <- filter(dt, !!sym(calib_vars$model_enough_data))

  if(!quiet) {
    # note: numbering does not make sense here because of the unique filter
    glue("Info: removing problematic calibrations ({nrow(dt) - nrow(dt_out)} of {nrow(dt)})") %>%
      message()
  }

  return(dt_out)
}

#' Calibrate delta values
#'
#' Before running this function, make sure to group the data appropriately by running \code{\link{iso_prepare_for_calibration}}.
#'
#' @inheritParams iso_prepare_for_calibration
#' @param model one or more models to run delta calibration with
#' @param is_standard column or filter condition to determine which data to use for the calibration (default is the field introduced by \code{\link{iso_add_standards}})
#' @param delta_residual name of the new column for keeping track of the residuals of the calibration model
#' @export
iso_calibrate_delta <- function(dt, model, is_standard = default(is_standard), calibration_data = default(calibration_data), delta_residual = default(delta_residual), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(model)) stop("no calibration model(s) supplied", call. = FALSE)
  dt_quo <- enquo(dt)
  model_quos <- enquo(model)
  filter_quo <- enquo(is_standard) %>% resolve_defaults()
  resid_quo <- enquo(delta_residual) %>% resolve_defaults()
  data_quo <- enquo(calibration_data) %>% resolve_defaults()

  # information
  if (!quiet) {
    if (quo_is_lang(model_quos) && quo_text(lang_head(model_quos)) %in% c("c", "list")) {
      lquos <- quos(!!!lang_args(model_quos))
    } else {
      lquos <- quos(!!!model_quos)
    }
    models <- str_c(names(lquos) %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) }, map_chr(lquos, quo_text))
    plural <- if (length(models) > 1) "s" else ""
    glue("Info: calculating delta calibration fits based on {length(models)} model{plural} ('{collapse(models, \"', '\")}') ",
         "for {nrow(dt)} data group(s) in '{quo_text(data_quo)}' with filter '{quo_text(filter_quo)}'; storing residuals in '{quo_text(resid_quo)}'.") %>% message()
  }

  # run regression
  min_n_data_points <- 2
  dt_w_regs <- run_regression(
    !!dt_quo, model = !!model_quos, model_data = !!data_quo, model_filter_condition = !!filter_quo,
    model_name = calib_delta_name, model_fit = calib_delta_fit,
    model_coefs = calib_delta_coefs, model_summary = calib_delta_summary,
    residual = !!resid_quo, min_n_data_points = min_n_data_points
  )

  # information on missing regs
  missing_regs <- dt_w_regs %>% filter(map_lgl(calib_delta_fit, is.null))
  if (nrow(missing_regs) > 0) {
    glue("{nrow(missing_regs)} out of {nrow(dt)} data group(s) did not have enough data (<{min_n_data_points} records) for a regression fit") %>%
      warning(immediate. = TRUE, call. = FALSE)
  }

  return(dt_w_regs)
}


#' Calibrate area values
#'
#' Before running this function, make sure to group the data appropriately by running \code{\link{iso_prepare_for_calibration}}.
#'
#' @inheritParams iso_calibrate_delta
#' @note not unit tested yet!
#' @export
iso_calibrate_area <- function(dt, model, is_standard = default(is_standard), calibration_data = default(calibration_data), area_residual = default(area_residual), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(model)) stop("no calibration model(s) supplied", call. = FALSE)
  dt_quo <- enquo(dt)
  model_quos <- enquo(model)
  filter_quo <- enquo(is_standard) %>% resolve_defaults()
  resid_quo <- enquo(area_residual) %>% resolve_defaults()
  data_quo <- enquo(calibration_data) %>% resolve_defaults()

  # information
  if (!quiet) {
    if (quo_is_lang(model_quos) && quo_text(lang_head(model_quos)) %in% c("c", "list")) {
      lquos <- quos(!!!lang_args(model_quos))
    } else {
      lquos <- quos(!!!model_quos)
    }
    models <- str_c(names(lquos) %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) }, map_chr(lquos, quo_text))
    plural <- if (length(models) > 1) "s" else ""
    glue("Info: calculating area calibration fits based on {length(models)} model{plural} ('{collapse(models, \"', '\")}') ",
         "for {nrow(dt)} data group(s) in '{quo_text(data_quo)}' with filter '{quo_text(filter_quo)}'; storing residuals in '{quo_text(resid_quo)}'.") %>% message()
  }

  # run regression
  min_n_data_points <- 2
  dt_w_regs <- run_regression(
    !!dt_quo, model = !!model_quos, model_data = !!data_quo, model_filter_condition = !!filter_quo,
    model_name = calib_area_name, model_fit = calib_area_fit,
    model_coefs = calib_area_coefs, model_summary = calib_area_summary,
    residual = !!resid_quo, min_n_data_points = min_n_data_points
  )

  # information on missing regs
  missing_regs <- dt_w_regs %>% filter(map_lgl(calib_area_fit, is.null))
  if (nrow(missing_regs) > 0) {
    glue("{nrow(missing_regs)} out of {nrow(dt)} data group(s) did not have enough data (<{min_n_data_points} records) for a regression fit") %>%
      warning(immediate. = TRUE, call. = FALSE)
  }

  return(dt_w_regs)
}

# INVERTING CALIBRATION --------

#' Apply calibration
#'
#' @param dt nested data table with \code{all_data} and calibration columns (see \link{iso_generate_calibration})
#' @param predict which value to calculate, must be one of the regression's independent variables
#' @param calibration name of the calibration to apply, must match the name used in \link{iso_generate_calibration} (if any)
#' @param calculate_error whether to estimate the standard error from the calibration
#' @return the data table with the following columns added to the nested \code{all_data} \:
#' \itemize{
#'   \item{\code{predict} column with suffix \code{_pred}: }{the predicted value from applying the calibration}
#'   \item{\code{predict} column with suffix \code{_pred_se}: }{the error of the predicated value propagated from the calibration}
#' }
#' @export
iso_apply_calibration <- function(dt, predict, calibration = "", calculate_error = FALSE, quiet = default(quiet)) {

  # safety checks


}

#' Apply delta calibration
#' @inheritParams iso_calibrate_delta
#' @param predict which value to calculate, must be one of the regression's independent variables
#' @param delta_pred what should the name of the new prediction column be?
#' @export
iso_apply_delta_calibrations <- function(dt, predict, delta_pred = default(delta_pred), calibration_data = default(calibration_data), quiet = default(quiet)) {

  # safety checks
  dt_quo <- enquo(dt)
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (length(missing <- setdiff(c("calib_delta_fit"), names(dt))) > 0) {
    glue("missing columns in data table: '{collapse(missing, \"', '\")}'. Make sure to run iso_calibrate_delta() first and keep these columns.") %>%
      stop(call. = FALSE)
  }

  # columns
  dt_cols <- get_column_names(dt_quo, predict = enquo(predict), calibration_data = enquo(calibration_data))
  dt_cols$calib_fit <- "calib_delta_fit"
  dt_new_cols <- get_new_column_names(delta_pred = enquo(delta_pred))
  dt_new_cols$delta_pred_se <- str_c(dt_new_cols$delta_pred, "_se")

  # model details
  dt_cal <- dt %>%
    mutate(
      # find the models' y variables
      .y_var = map_chr(!!sym(dt_cols$calib_fit), function(m) {
        y <- all.vars(m$terms[[2]])
        if (length(y) != 1) stop(glue("incorrect number of dependent variables {length(y)}: {collapse(y)}"))
        return(y)
      }),
      # find the models' auxiliary x variables
      .k_vars = map2(!!sym(dt_cols$calib_fit), .y_var, function(m, y) {
        all.vars(m$terms) %>% { .[.!=dt_cols$predict & .!=y] }
      })
    )

  # new var to predict
  x_pred <- "x_pred"
  x_pred_se <- str_c(x_pred, "_se")

  # independent variable to infer
  x_var <- "x1"
  # dependent variable in model
  y_var <- all.vars(mod$terms[[2]])
  # other variables in model
  k_vars <- all.vars(mod$terms) %>% { .[.!=x_var & .!=y_var] }

  if (length(y_var) != 1)
    stop(glue("incorrect number of dependent variables {length(y_var)}: {collapse(y_var)}"))

  invert_data <-
    #sample_n(test_data, 10) %>%
    test_data %>%
    mutate(x_known = !!sym(x_var)) %>%
    mutate(.x_min = min(!!sym(x_var), na.rm = TRUE),
           .x_max = max(!!sym(x_var), na.rm = TRUE)) %>%
    mutate(..row.. = row_number(y)) %>%
    nest(-..row.., -.x_min, -.x_max) %>%
    mutate(
      result = pmap(
        list(data = data, xmin = .x_min, xmax = .x_max),
        function(data, xmin, xmax) {
          range_tolerance_percent <- 1
          range_tolerance <- range_tolerance_percent/2 * (xmax - xmin)
          safe_invest <- safely(invest)
          out <- safe_invest(mod, y0=data[[y_var]], interval = "Wald", x0.name = x_var,
                             lower = xmin - range_tolerance, upper = xmax + range_tolerance, extendInt = "yes",
                             newdata = data[k_vars])
          if (is.null(out$error)) {
            return(out$result)
          } else {
            warning(out$error$message, immediate. = TRUE, call. = FALSE)
            return(list(estimate = NA, se = NA))
          }
        }),
      !!x_pred := map_dbl(result, ~.x$estimate),
      !!x_pred_se := map_dbl(result, ~.x$se)
    ) %>%
    select(-result, -..row..) %>%
    unnest(data)

}

# UNNESTING --------

#' Unnest data
#'
#' Pull out columns from the \code{all_data} column. Typically used at various points after a dataset is prepared for calibration with \link{iso_prepare_for_calibration}, calibrations are generated with \link{iso_generate_calibration} and/or calibrations are applied with \link{iso_apply_calibration}.
#'
#' @inheritParams unnest_model_column
#' @export
iso_unnest_data <- function(dt, select = everything(), keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE) {
  unnest_select_data(dt, select = !!enquo(select), nested_data = all_data, keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' Unnest calibration coefficients
#'
#' Retrieve calibration coefficients for a calibration. Automatically removes problematic calibrations (see \link{iso_remove_problematic_calibrations}) if any are still present.
#'
#' @inheritParams iso_generate_calibration
#' @inheritParams unnest_model_column
#' @export
iso_unnest_calibration_coefs <- function(dt, calibration = "", select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  dt %>%
    # remove problematic calibrations first
    iso_remove_problematic_calibrations(calibration = calibration, quiet = TRUE) %>%
    # unnest from the nested data frame
    unnest_model_column(model_column = model_coefs, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
                        select = !!(enquo(select)),
                        keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' Unnest calibration summary
#'
#' Retrieve summary entries for a calibration. Automatically removes problematic calibrations (see \link{iso_remove_problematic_calibrations}) if any are still present.
#'
#' @inheritParams iso_generate_calibration
#' @inheritParams unnest_model_column
#' @export
iso_unnest_calibration_summary <- function(dt, calibration = "", select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  dt %>%
    # remove problematic calibrations first
    iso_remove_problematic_calibrations(calibration = calibration, quiet = TRUE) %>%
    # unnest from the nested data frame
    unnest_model_column(model_column = model_summary, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
                        select = !!(enquo(select)),
                        keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' Unnest calibration range
#'
#' Retrieve range information for a calibration. Automatically removes problematic calibrations (see \link{iso_remove_problematic_calibrations}) if any are still present.
#'
#' @inheritParams iso_generate_calibration
#' @inheritParams unnest_model_column
#' @export
iso_unnest_calibration_range <- function(dt, calibration = "", select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  calib_vars <- get_calibration_vars(calibration)
  check_calibration_cols(!!enquo(dt), calib_vars$model_params)

  dt %>%
    # remove problematic calibrations first
    iso_remove_problematic_calibrations(calibration = calibration, quiet = TRUE) %>%
    # unnest from the nested data frame
    unnest_model_column(model_column = model_range, nested_model = TRUE, model_params = !!sym(calib_vars$model_params),
                        select = !!(enquo(select)),
                        keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}
