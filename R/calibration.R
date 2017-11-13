#' Calculate calibrations.
#' @note: todo - find a better way to deal with the amplitude part
#' @note: todo - deal with the keep columns better so they don't end up working as grouping columns during nesting! maybe join back in after the operations?
#'
#' @param dt the data table
#' @param stds the standards table
#' @param match_by what to use to match the standards to the data (use c(...) to select multiple)
#' @param group_by what to group by for indidual calibration calculations (use c(...) to select multiple)
#' @param keep which column(s) to keep beyond the group_by (use c(...) to select multiple).
#' Note that if these should be unique across grouping variables, otherwise it will result in multiple lines of the same regression data per group.
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
  dt_cols <- get_column_names(!!enquo(dt), group_by = enquo(group_by), match_by = enquo(match_by), keep = enquo(keep),
                              measured = enquo(measured), ampl = enquo(ampl),
                              n_reqs = list(group_by = "*", keep = "*"))
  stds_cols <- get_column_names(!!enquo(stds), match_by = enquo(match_by), known = enquo(known))

  # select standards
  data_w_stds <- data_table %>%
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
      fit = map(data, ~summary(eval_tidy(model, data = .x))),
      coefficients = map(fit, "coefficients"),
      intercept = map_dbl(coefficients, `[`, 1, 1),
      intercept_se = map_dbl(coefficients, `[`, 1, 2),
      slope = map_dbl(coefficients, `[`, 2, 1),
      slope_se = map_dbl(coefficients, `[`, 2, 2),
      r2 = map_dbl(fit, "r.squared")
    )
}
