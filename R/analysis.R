# analysis functions

#' Identify outliers
#'
#'
#'
#' @export
iso_identify_outliers <- function(dt, y, plus_minus_value = NULL, plus_minus_sd = NULL, group_by = NULL, is_outlier = default(is_outlier)) {

  # use value or sd?
  use_value <- !is.null(plus_minus_value)
  use_sd <- !is.null(plus_minus_sd)
  y_quo <- rlang::enquo(y)

  # safety checks
  if (missing(dt)) stop("no data frame provided", call. = FALSE)
  if (rlang::quo_is_missing(y_quo)) stop("no y-variable defined", call. = FALSE)
  if (use_value + use_sd > 1) {
    stop("more than one cutoff defined: please provide only one of 'plus_minus_value' or 'plus_minus_sd'", call. = FALSE)
  } else if (use_value + use_sd == 0) {
    stop("no cutoff provided: please provide one of 'plus_minus_value' or 'plus_minus_sd'", call. = FALSE)
  }

  # need numeric single cutoffs
  if (use_value && (!is.numeric(plus_minus_value) || length(plus_minus_value) != 1L))
    stop("please provide a single numeric value for plus/minus value cutoff", call. = FALSE)
  if (use_sd && (!is.numeric(plus_minus_sd) || length(plus_minus_sd) != 1L))
    stop("please provide a single numeric value for plus/minus sd cutoff", call. = FALSE)

  # grouping
  group_by_expr <- rlang::enexpr(group_by)
  if (!rlang::is_null(group_by_expr)) {
    if (rlang::is_call(group_by_expr) && rlang::call_name(group_by_expr) == "c") {
      group_by_cols <- rlang::call_args(group_by_expr)
    } else {
      group_by_cols <- list(group_by_expr)
    }
    dt <- dt %>% dplyr::group_by(!!!group_by_cols)
  }

  # new columns
  dt_new_cols <- get_new_column_names(is_outlier = enquo(is_outlier))

  # calculations
  dt <- dt %>%
    dplyr::mutate(
      !!dt_new_cols$is_outlier :=
        if (use_sd)
          !!quo(abs(!!y_quo - base::mean(!!y_quo)) > !!plus_minus_sd * stats::sd(!!y_quo))
      else
        !!quo(abs(!!y_quo - base::mean(!!y_quo)) > !!plus_minus_value)
    )

  # ungroup if grouping was provided
  if (!rlang::is_null(group_by_expr)) {
    dt <- dplyr::ungroup(dt)
  }

  return(dt)
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
    dplyr::filter(n >= cutoff) %>%
    dplyr::ungroup()
}

