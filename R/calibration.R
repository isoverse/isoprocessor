#' Add calibration standards
#' @inheritParams iso_print_data_table
#' @param stds the standards table
#' @param match_by default is the compound parameter also used in peak mapping
#' @param is_standard new column that holds information about what is a standard and what isn't
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
#' @param match_by what to use to match the standards to the data (use c(...) to select multiple)
#' @param group_by what to group by for indidual calibration calculations (use c(...) to select multiple), no grouping by default (i.e. single calibration across all data)
#' @param keep which column(s) to keep beyond the group_by (use c(...) to select multiple). Note that these should be unique across grouping variables, otherwise it will result in multiple lines of the same regression data per group.
#' @param calibration_data column name for the combined calibration data
#' @export
prepare_for_calibration <- function(dt, stds, match_by = default(match_by), group_by = NULL, keep = NULL, calibration_data = default(calibration_data)) {

  # make sure params supplied
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(stds)) stop("no standards table supplied", call. = FALSE)


  nested_dt <- nest_data(dt, group_by = !!enquo(group_by), nested_data = !!quo(calibration_data))


  return(nested_dt)
}


calibrate_delta <- function(dt, stds) {

  # determine which parameter to predict for each model

  # - provide selection criterion for what should be used as standard
  # - throw error whenever there are any missing pieces of information for
  # - allow for inversion

}


calibrate_concentrations <- function(dt, stds) {

}

#' Calculate calibrations.
#' @note: todo - find a better way to deal with the amplitude part
#' @note: todo - deal with the keep columns better so they don't end up working as grouping columns during nesting! maybe join back in after the operations?
#'
#' @inheritParams iso_print_data_table
#' @param stds the standards table
#' @param match_by what to use to match the standards to the data (use c(...) to select multiple)
#' @param group_by what to group by for indidual calibration calculations (use c(...) to select multiple)
#' @param keep which column(s) to keep beyond the group_by (use c(...) to select multiple).
#' Note that these should be unique across grouping variables, otherwise it will result in multiple lines of the same regression data per group.
#' @param measured where is the measured data stored?
#' @param known where is the known data value stored?
#' @param ampl where is the signal amplitude stored?
#' @export
get_standards_calibrations <- function(dt, stds, match_by = default(match_by), group_by = default(group_by),
                                       measured = default(measured), known = default(known), ampl = default(ampl), keep = default(NULL)) {

  # make sure params supplied
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(stds)) stop("no standards table supplied", call. = FALSE)

  # column names allowing standard and NSE
  dt_cols <- get_column_names(!!enquo(dt), group_by = enquo(group_by), match_by = enquo(match_by),
                              keep = enquo(keep), measured = enquo(measured), ampl = enquo(ampl),
                              n_reqs = list(group_by = "*", keep = "*"))
  stds_cols <- get_column_names(!!enquo(stds), match_by = enquo(match_by), known = enquo(known))

  # select standards
  data_w_stds <- dt %>%
    filter(type == "standard", is_ref_peak == "no") %>%
    left_join(stds, by = dt_cols$match_by) %>%
    mutate(is_std = !is.na(.data[[stds_cols$known]]))

  # model
  model <- quo(lm(UQE(as.name(dt_cols$measured)) ~ UQE(as.name(stds_cols$known))))

  # evaluate
  data_w_stds %>%
    filter(is_std) %>%
    # nest the data (make sure it's a data_frame)
    as_data_frame() %>%
    nest(!!!cols_to_quos(dt_cols[c("group_by", "keep")], negate = TRUE)) %>%
    mutate(
      ampl_mean = map_dbl(data, ~mean(.x[[dt_cols$ampl]])),
      ampl_sd = map_dbl(data, ~sd(.x[[dt_cols$ampl]])),
      fit = map(data, ~eval_tidy(model, data = .x)),
      coefficients = map(fit, tidy),
      summary = map(fit, glance),
      intercept = map_dbl(coefficients, `[`, 1, 2),
      intercept_se = map_dbl(coefficients, `[`, 1, 3),
      slope = map_dbl(coefficients, `[`, 2, 2),
      slope_se = map_dbl(coefficients, `[`, 2, 3)
    )
}
