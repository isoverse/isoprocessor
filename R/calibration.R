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

# CALIBRATIONS -----

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

#' @details \code{iso_unnest_calib_data} unnests the data
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_calib_data <- function(dt, select = everything(), calibration_data = default(calibration_data), keep_remaining_nested_data = TRUE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = !!enquo(calibration_data), select = !!(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

# @note: could consider providing a combined delta_calib_coefs and summary function (similar to how iso_visualize_delta_calib_fits does it)
#' Retrieve calibration information
#'
#' @details \code{iso_unnest_delta_calib_coefs} unnests the regression fit coefficients for the delta calibration
#' @inheritParams unnest_model_results
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_delta_calib_coefs <- function(dt, select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = calib_delta_coefs, select = !!(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' @details \code{iso_unnest_delta_calib_summary} unnests the regression fit summary for the delta calibration
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_delta_calib_summary <- function(dt, select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = calib_delta_summary, select = !!(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

# @note: could consider providing a combined area_calib_coefs and summary function (similar to how iso_visualize_delta_calib_fits does it)
# @note: maybe combine all these different unnest functions in some way??
#' Retrieve calibration information
#'
#' @details \code{iso_unnest_area_calib_coefs} unnests the regression fit coefficients for the area calibration
#' @inheritParams unnest_model_results
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_area_calib_coefs <- function(dt, select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = calib_area_coefs, select = !!(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}

#' @details \code{iso_unnest_area_calib_summary} unnests the regression fit summary for the area calibration
#' @rdname iso_unnest_model_results
#' @export
iso_unnest_area_calib_summary <- function(dt, select = everything(), keep_remaining_nested_data = FALSE, keep_other_list_data = TRUE) {
  unnest_model_results(dt, model_results = calib_area_summary, select = !!(enquo(select)), keep_remaining_nested_data = keep_remaining_nested_data, keep_other_list_data = keep_other_list_data)
}



