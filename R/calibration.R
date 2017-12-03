#' Add calibration standards
#' @inheritParams iso_print_data_table
#' @param stds the standards table
#' @param match_by default is the compound parameter also used in peak mapping
#' @param is_standard new column that holds information about what is a standard and what isn't
#' @note FIXME write unit tests
#' @export
iso_add_standards <- function(dt, stds, match_by = default(compound), is_standard = default(is_standard), quiet = default(quiet)) {

  # make sure params supplied
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(stds)) stop("no standards table supplied", call. = FALSE)

  # column names allowing standard and NSE
  dt_cols <- get_column_names(!!enquo(dt), match_by = enquo(match_by))
  stds_cols <- get_column_names(!!enquo(stds), match_by = enquo(match_by))
  new_cols <- get_new_column_names(is_standard = enquo(is_standard))

  # select standards
  stds <- mutate(stds, ..marker.. = TRUE)
  dt_w_stds <- dt %>%
    left_join(stds, by = dt_cols$match_by) %>%
    mutate(!!new_cols$is_standard := !is.na(..marker..)) %>%
    select(-..marker..)

  if (!quiet) {
    glue("Info: added {length(unique(dt[[dt_cols$match_by]]))} standard entries to {nrow(filter(dt_w_stds, !!as.name(new_cols$is_standard)))} out of {nrow(dt_w_stds)} rows") %>%
      message()
  }
  return(dt_w_stds)
}

#' Prepare data set for calibration
#' @inheritParams iso_print_data_table
#' @param stds the standards table
#' @param group_by what to group by for indidual calibration calculations (use c(...) to select multiple) - set \code{group_by = NULL} to avoid grouping
#' @param keep which column(s) to keep beyond the group_by (use c(...) to select multiple). Note that these should be unique across grouping variables, otherwise it will result in multiple lines of the same regression data per group.
#' @param calibration_data column name for the combined calibration data
#' @note FIXME write unit tests
#' @export
iso_prepare_for_calibration <- function(dt, group_by = default(group_by), calibration_data = default(calibration_data), quiet = default(quiet)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  group_quo <- enquo(group_by)
  calib_quo <- enquo(calibration_data)
  dt_cols <- get_column_names(!!enquo(dt), group_by = group_quo, n_reqs = list(group_by = "*"))
  new_cols <- get_new_column_names(calibration_data = calib_quo)

  if (!quiet) {
    if (length(dt_cols$group_by) > 0)
      glue("Info: preparing data for calibration by grouping based on '{collapse(dt_cols$group_by, \"', '\", last = \"' and '\")}' and nesting the grouped datasets into '{new_cols$calibration_data}'") %>%
      message()
    else
      glue("Info: preparing data for calibration by nesting the entire dataset into '{new_cols$calibration_data}'") %>%
      message()
  }

  return(nest_data(dt, group_by = !!group_quo, nested_data = !!calib_quo))
}

#' Calibrate delta values
#'
#' Before running this function, make sure to group the data appropriately by running \code{\link{iso_prepare_for_calibration}}.
#'
#' @inheritParams iso_prepare_for_calibration
#' @param model one or more models to run delta calibration with
#' @param is_standard column or filter condition to determine which data to use for the calibration (default is the field introduced by \code{\link{iso_prepare_for_calibration}})
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
         "for {nrow(dt)} data group(s) in '{quo_text(data_quo)}' with filter '{quo_text(filter_quo)}'; storing residuals in '{quo_text(resid_quo)}.'") %>% message()
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

# @note: could consider providing a combined delta_calib_coefs and summary function (similar to how iso_visualize_delta_calib_fits does it)
#' Retrieve calibration information
#'
#' @details \code{iso_unnest_delta_calib_coefs} unnests the regression fit coefficients for the delta calibration
#' @inheritParams unnest_model_results
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_delta_calib_coefs <- function(dt, select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = calib_delta_coefs, select = UQ(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' @details \code{iso_unnest_delta_calib_summary} unnests the regression fit summary for the delta calibration
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_delta_calib_summary <- function(dt, select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = calib_delta_summary, select = UQ(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' @details \code{iso_unnest_calib_data} unnests the data
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_calib_data <- function(dt, select = everything(), calibration_data = default(calibration_data), keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = !!enquo(calibration_data), select = UQ(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

# @note implement
calibrate_concentrations <- function(dt, stds) {

}
